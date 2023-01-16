module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.ArrayBuffer.Typed (buffer, fromArray, set, whole)
import Data.ArrayBuffer.Types (Float32Array)
import Data.Float32 (fromNumber')
import Data.Int (ceil, floor, toNumber)
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), maybe)
import Data.UInt (fromInt)
import Deku.Attribute ((!:=))
import Deku.Attributes (klass_)
import Deku.Control (text, text_, (<#~>))
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event (create)
import QualifiedDo.Alt as Alt
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.GPU.BufferSource (fromUint32Array)
import Web.GPU.GPU (requestAdapter)
import Web.GPU.GPUAdapter (requestDevice)
import Web.GPU.GPUBindGroupEntry (GPUBufferBinding, gpuBindGroupEntry)
import Web.GPU.GPUBindGroupLayoutEntry (gpuBindGroupLayoutEntry)
import Web.GPU.GPUBufferBindingLayout (GPUBufferBindingLayout)
import Web.GPU.GPUBufferBindingType as GPUBufferBindingType
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasAlphaMode (opaque)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (configure, getCurrentTexture)
import Web.GPU.GPUCommandEncoder (beginComputePass, copyBufferToTexture, finish)
import Web.GPU.GPUComputePassEncoder as GPUComputePassEncoder
import Web.GPU.GPUDevice (createBindGroup, createBindGroupLayout, createBuffer, createCommandEncoder, createComputePipeline, createPipelineLayout, createShaderModule)
import Web.GPU.GPUDevice as GPUDevice
import Web.GPU.GPUExtent3D (gpuExtent3DWH)
import Web.GPU.GPUProgrammableStage (GPUProgrammableStage)
import Web.GPU.GPUQueue (onSubmittedWorkDone, submit, writeBuffer)
import Web.GPU.GPUShaderStage as GPUShaderStage
import Web.GPU.GPUTextureFormat as GPUTextureFormat
import Web.GPU.GPUTextureUsage as GPUTextureUsage
import Web.GPU.HTMLCanvasElement (getContext)
import Web.GPU.Internal.Bitwise ((.|.))
import Web.GPU.Internal.RequiredAndOptional (x)
import Web.GPU.Navigator (gpu)
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement (height, setHeight, setWidth, toElement, width)
import Web.HTML.Window (navigator, requestAnimationFrame)
import Web.Promise as Web.Promise

averager :: forall a. EuclideanRing a => Effect (a -> Effect a)
averager = do
  ct <- Ref.new zero
  val <- Ref.new zero
  pure \v -> do
    ct' <- Ref.read ct
    val' <- Ref.read val
    Ref.write (ct' + one) ct
    Ref.write (val' + v) val
    pure $ val' / ct'

convertPromise :: Web.Promise.Promise ~> Control.Promise.Promise
convertPromise = unsafeCoerce

type FrameInfo = { avgFrame :: Number, avgTime :: Number }

gpuMe :: Effect Unit -> (FrameInfo -> Effect Unit) -> HTMLCanvasElement -> Effect Unit
gpuMe showErrorMessage pushFrameInfo canvas = launchAff_ $ delay (Milliseconds 20.0) *> liftEffect do
  context <- getContext canvas >>= maybe
    (showErrorMessage *> throwError (error "could not find context"))
    pure
  let maxCanvasWidth = 4096
  let maxBufferWidth = ceil (toNumber maxCanvasWidth * 4.0 / 256.0) * 256
  let maxCanvasHeight = 2000
  let sizeOfUInt = 1
  timeDeltaAverager <- averager
  frameDeltaAverager <- averager
  startsAt <- getTime <$> now
  currentFrame <- Ref.new 0
  entry <- window >>= navigator >>= gpu >>= case _ of
    Nothing -> do
      showErrorMessage
      throwError $ error "WebGPU is not supported"
    Just entry -> pure entry
  launchAff_ do
    adapter <- (toAffE $ convertPromise <$> requestAdapter entry (x {})) >>=
      case _ of
        Nothing -> liftEffect do
          showErrorMessage
          throwError $ error "WebGPU is not supported"
        Just adapter -> pure adapter
    device <- (toAffE $ convertPromise <$> requestDevice adapter (x {})) >>=
      case _ of
        Nothing -> liftEffect do
          showErrorMessage
          throwError $ error "WebGPU is not supported"
        Just device -> pure device
    queue <- liftEffect $ GPUDevice.queue device
    canvasInfoBuffer <- liftEffect $ createBuffer device $ x
      { size: 16 -- align(4) size(16)
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.storage
      }
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: maxBufferWidth * maxCanvasHeight * sizeOfUInt
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    let
      skyDesc = x
        { code:
            """
fn lerp(a: f32, b: f32, t: f32) -> f32 {
  return a + (b - a) * t;
}

fn lerpv(a: vec3<f32>, b: vec3<f32>, t: f32) -> vec3<f32> {
  return a + (b - a) * t;
}

struct rendering_info_struct {
  real_canvas_width: u32, // width of the canvas in pixels
  overshot_canvas_width: u32, // width of the canvas in pixels so that the byte count per pixel is a multiple of 256
  canvas_height: u32, // height of the canvas in pixels
  current_time: f32 // current time in seconds
}

struct ray {
  origin: vec3<f32>,
  direction: vec3<f32>
}

fn point_at_parameter(r: ptr<function,ray>, t: f32) -> vec3<f32> {
  return (*r).origin + t * (*r).direction;
}

const white = vec3<f32>(1.0, 1.0, 1.0);
const sky_blue = vec3<f32>(0.5, 0.7, 1.0);

fn color(r: ptr<function,ray>) -> vec3<f32> {
  var unit_direction = normalize((*r).direction);
  var t = 0.5 * (unit_direction.y + 1.0);
  return lerpv(white, sky_blue, t);
}


const pi = 3.141592653589793;
const origin = vec3(0.0, 0.0, 0.0);

@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read_write> resultMatrix : array<u32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width * rendering_info.canvas_height) {
    return;
  }
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  // normalize so that the smaller dimension is 2 end-to-end
  var ambitus_x = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect < 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var p_x = f32(global_id.x % rendering_info.real_canvas_width) / f32(rendering_info.real_canvas_width);
  var p_y = 1. - f32(global_id.x / rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var bound = ((global_id.x / rendering_info.real_canvas_width) * rendering_info.overshot_canvas_width) + (global_id.x % rendering_info.real_canvas_width) ;
  var r: ray;
  r.origin = origin;
  r.direction = lower_left_corner + vec3(p_x * ambitus_x, p_y * ambitus_y, 0.0);
  var my_color = color(&r);
  resultMatrix[bound] = pack4x8unorm(vec4<f32>(my_color.b, my_color.g, my_color.r, 1.f));
}"""
        }
    skyModule <- liftEffect $ createShaderModule device skyDesc
    let
      (skyStage :: GPUProgrammableStage) = x
        { "module": skyModule
        , entryPoint: "main"
        }
    skyBufferBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    skyBufferPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ skyBufferBindGroupLayout ] }
    skyBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: skyBufferBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: canvasInfoBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      }
    skyComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: skyBufferPipelineLayout
      , compute: skyStage
      }
    let
      (config :: GPUCanvasConfiguration) = x
        { device
        , format: GPUTextureFormat.bgra8unorm
        , usage:
            GPUTextureUsage.renderAttachment .|. GPUTextureUsage.copyDst
        , alphaMode: opaque
        }
    liftEffect $ configure context config
    let
      encodeCommands colorTexture = do
        cw <- clientWidth (toElement canvas)
        ch <- clientHeight (toElement canvas)
        setWidth (floor cw) canvas
        setHeight (floor ch) canvas
        canvasWidth <- width canvas
        canvasHeight <- height canvas
        let bufferWidth = ceil (toNumber canvasWidth * 4.0 / 256.0) * 256
        let overshotWidth = bufferWidth / 4
        tn <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
        cf <- Ref.read currentFrame
        Ref.write (cf + 1) currentFrame
        commandEncoder <- createCommandEncoder device (x {})
        cinfo <- fromArray $ map fromInt [ canvasWidth, overshotWidth, canvasHeight, 0 ]
        let asBuffer = buffer cinfo
        whole asBuffer >>= \(x :: Float32Array) -> void $ set x (Just 3) [ fromNumber' tn ]
        writeBuffer queue canvasInfoBuffer 0 (fromUint32Array cinfo)
        skyPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline skyPassEncoder
          skyComputePipeline
        GPUComputePassEncoder.setBindGroup skyPassEncoder 0
          skyBindGroup
        GPUComputePassEncoder.dispatchWorkgroups skyPassEncoder $ ceil (toNumber (canvasWidth * canvasHeight) / 64.0)
        GPUComputePassEncoder.end skyPassEncoder
        copyBufferToTexture
          commandEncoder
          (x { buffer: wholeCanvasBuffer, bytesPerRow: bufferWidth })
          (x { texture: colorTexture })
          (gpuExtent3DWH canvasWidth canvasHeight)
        toSubmit <- finish commandEncoder
        submit queue [ toSubmit ]
        launchAff_ do
          toAffE $ convertPromise <$> (onSubmittedWorkDone queue)
          liftEffect do
            tnx <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
            cfx <- Ref.read currentFrame
            avgTime <- timeDeltaAverager (tnx - tn)
            avgFrame <- frameDeltaAverager (toNumber (cfx - cf))
            pushFrameInfo { avgTime, avgFrame }
    let
      render = unit # fix \f _ -> do
        colorTexture <- getCurrentTexture context
        encodeCommands colorTexture
        window >>= void <<< requestAnimationFrame (f unit)

    liftEffect render

main :: Effect Unit
main = do
  frameInfo <- create
  errorMessage <- create
  runInBody Deku.do
    D.div_
      [ D.canvas
          Alt.do
            klass_ "absolute w-full h-full"
            D.SelfT !:= gpuMe (errorMessage.push unit) frameInfo.push
          []
      , D.div
          Alt.do
            klass_ "absolute p-3 text-white"
          [ errorMessage.event $> false <|> pure true <#~>
              if _ then
                text (_.avgTime >>> show >>> ("Avg time: " <> _) <$> frameInfo.event)
              else text_ "Your device does not support WebGPU"
          ]
      ]
