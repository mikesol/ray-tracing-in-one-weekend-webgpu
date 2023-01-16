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
      frenchFlagDesc = x
        { code:
            """
struct rendering_info_struct {
  real_canvas_width: u32, // width of the canvas in pixels
  overshot_canvas_width: u32, // width of the canvas in pixels so that the byte count per pixel is a multiple of 256
  canvas_height: u32, // height of the canvas in pixels
  current_time: f32 // current time in seconds
}

@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read_write> resultMatrix : array<u32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width * rendering_info.canvas_height) {
    return;
  }
  var bound = ((global_id.x / rendering_info.real_canvas_width) * rendering_info.overshot_canvas_width) + (global_id.x % rendering_info.real_canvas_width) ;
  var bmw = global_id.x % rendering_info.real_canvas_width;
  var bmh = global_id.x / rendering_info.real_canvas_width;
  var french_or_dutch = rendering_info.current_time % 2.0 < 1.0;
  var is_blue = select(bmh >= 2 * rendering_info.canvas_height / 3, bmw < rendering_info.real_canvas_width / 3, french_or_dutch);
  var is_white = select(bmh < 2 * rendering_info.canvas_height / 3 && bmh >= rendering_info.canvas_height / 3, bmw < 2 * rendering_info.real_canvas_width / 3 && bmw >= rendering_info.real_canvas_width / 3, french_or_dutch);
  var is_red = select(bmh < rendering_info.canvas_height / 3, bmw >= 2 * rendering_info.real_canvas_width / 3, french_or_dutch);
  var b = select(0.f, 1.f, is_blue || is_white);
  var g = select(0.f, 1.f, is_white);
  var r = select(0.f, 1.f, is_red || is_white);
  resultMatrix[bound] = pack4x8unorm(vec4<f32>(b, g, r, 1.f));
}"""
        }
    frenchFlagModule <- liftEffect $ createShaderModule device frenchFlagDesc
    let
      (frenchFlagStage :: GPUProgrammableStage) = x
        { "module": frenchFlagModule
        , entryPoint: "main"
        }
    frenchFlagBufferBindGroupLayout <- liftEffect $ createBindGroupLayout device
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
    frenchFlagBufferPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ frenchFlagBufferBindGroupLayout ] }
    frenchFlagBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: frenchFlagBufferBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: canvasInfoBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      }
    frenchFlagComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: frenchFlagBufferPipelineLayout
      , compute: frenchFlagStage
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
        frenchFlagPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline frenchFlagPassEncoder
          frenchFlagComputePipeline
        GPUComputePassEncoder.setBindGroup frenchFlagPassEncoder 0
          frenchFlagBindGroup
        GPUComputePassEncoder.dispatchWorkgroups frenchFlagPassEncoder $ ceil (toNumber (canvasWidth * canvasHeight) / 64.0)
        GPUComputePassEncoder.end frenchFlagPassEncoder
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
