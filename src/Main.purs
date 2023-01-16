module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.Array (fold, length)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Typed (class TypedArray, buffer, fromArray, set, setTyped, whole)
import Data.ArrayBuffer.Types (ArrayView, Float32Array)
import Data.Float32 (fromNumber')
import Data.Int (ceil, floor, toNumber)
import Data.Int.Bits (complement, (.&.))
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
import Web.GPU.GPUBuffer (GPUBuffer, getMappedRange, unmap)
import Web.GPU.GPUBufferBindingLayout (GPUBufferBindingLayout)
import Web.GPU.GPUBufferBindingType as GPUBufferBindingType
import Web.GPU.GPUBufferUsage (GPUBufferUsageFlags)
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasAlphaMode (opaque)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (configure, getCurrentTexture)
import Web.GPU.GPUCommandEncoder (beginComputePass, copyBufferToTexture, finish)
import Web.GPU.GPUComputePassEncoder as GPUComputePassEncoder
import Web.GPU.GPUDevice (GPUDevice, createBindGroup, createBindGroupLayout, createBuffer, createCommandEncoder, createComputePipeline, createPipelineLayout, createShaderModule)
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

-- defs
inputData :: String
inputData =
  """
// input data
struct rendering_info_struct {
  real_canvas_width: u32, // width of the canvas in pixels
  overshot_canvas_width: u32, // width of the canvas in pixels so that the byte count per pixel is a multiple of 256
  canvas_height: u32, // height of the canvas in pixels
  n_spheres: u32, // number of spheres
  current_time: f32 // current time in seconds
} 
"""

lerp :: String
lerp =
  """
// lerp
fn lerp(a: f32, b: f32, t: f32) -> f32 {
  return a + (b - a) * t;
}
"""

lerpv :: String
lerpv =
  """
// lerpv
fn lerpv(a: ptr<function,vec3<f32>>, b: ptr<function,vec3<f32>>, t: f32) -> vec3<f32> {
  return (*a) + ((*b) - (*a)) * t;
}
"""

ray :: String
ray =
  """// ray
struct ray {
  origin: vec3<f32>,
  direction: vec3<f32>
}
"""

hitRecord :: String
hitRecord =
  """
// hit record
struct hit_record {
  t: f32,
  p: vec3<f32>,
  normal: vec3<f32>
}

      """

pointAtParameter :: String
pointAtParameter =
  """
// point at parameter
fn point_at_parameter(r: ptr<function,ray>, t: f32) -> vec3<f32> {
  return (*r).origin + t * (*r).direction;
}"""

hitSphere :: String
hitSphere =
  """
// hit sphere
fn hit_sphere(cx: f32, cy: f32, cz: f32, radius: f32, r: ptr<function,ray>, t_min: f32, t_max: f32, hit_t: ptr<function,f32>) -> bool {
  var center = vec3(cx, cy, cz);
  var oc = (*r).origin - center;
  var a = dot((*r).direction, (*r).direction);
  var b = dot(oc, (*r).direction);
  var c = dot(oc, oc) - radius * radius;
  var discriminant = b * b - a * c;
  if (discriminant > 0) {
    var temp = (-b - sqrt(discriminant)) / a;
    if (temp < t_max && temp > t_min) {
      *hit_t = temp;
      return true;
    }
    temp = (-b + sqrt(discriminant)) / a;
    if (temp < t_max && temp > t_min) {
      *hit_t = temp;
      return true;
    }
  }
  return false;
}
"""

makeHitRec :: String
makeHitRec =
  """
// make hit rec
fn make_hit_rec(cx: f32, cy: f32, cz: f32, radius: f32, t: f32, r: ptr<function,ray>, rec: ptr<function,hit_record>) -> bool {
  (*rec).t = t;
  (*rec).p = point_at_parameter(r, t);
  (*rec).normal = ((*rec).p - vec3(cx,cy,cz)) / radius;
  return true;
}

"""

usefulConsts :: String
usefulConsts =
  """
const f_max = 3.40282346638528859812e+38f;
const reasonable_upper_bound = 10.0f;
const origin = vec3(0.0, 0.0, 0.0);
      """

boundNormalization :: String
boundNormalization =
  """
fn normalize_t_to_bounds(x: f32) -> f32 {
  return (1.f - max(0.f, min(1.f, (x / reasonable_upper_bound))));
}
fn unnormalize_bounds_to_t(x: f32) -> f32 {
  return (1.f - x) * reasonable_upper_bound;
}
      """

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

createBufferF
  :: forall a t
   . TypedArray a t
  => GPUDevice
  -> ArrayView a
  -> GPUBufferUsageFlags
  -> Effect GPUBuffer
createBufferF device arr usage = do
  let
    desc = x
      { size: ((byteLength (buffer arr)) + 3) .&. complement 3
      , usage
      , mappedAtCreation: true
      }
  buffer <- createBuffer device desc
  writeArray <- getMappedRange buffer >>= whole
  _ <- setTyped writeArray Nothing arr
  unmap buffer
  pure buffer

gpuMe :: Effect Unit -> (FrameInfo -> Effect Unit) -> HTMLCanvasElement -> Effect Unit
gpuMe showErrorMessage pushFrameInfo canvas = launchAff_ $ delay (Milliseconds 20.0) *> liftEffect do
  context <- getContext canvas >>= maybe
    (showErrorMessage *> throwError (error "could not find context"))
    pure
  let maxCanvasWidth = 4096
  let maxBufferWidth = ceil (toNumber maxCanvasWidth * 4.0 / 256.0) * 256
  let maxCanvasHeight = 2000
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
    let hitsBufferSize = maxCanvasWidth * maxCanvasHeight * 4 -- 4 because we're storing u32s
    let canvasBufferSize = maxBufferWidth * maxCanvasHeight * 1 -- 1 because bgra8unorm is 1 byte per pixel
    queue <- liftEffect $ GPUDevice.queue device
    canvasInfoBuffer <- liftEffect $ createBuffer device $ x
      { size: 20 -- align(4) size(20)
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.storage
      }
    let
      rawSphereData = map fromNumber'
        [ 0.0
        , 0.0
        , -1.0
        , 0.5
        , 0.0
        , -100.5
        , -1.0
        , 100.0
        ]
    let nSpheres = length rawSphereData / 4
    sphereData :: Float32Array <- liftEffect $ fromArray rawSphereData
    sphereBuffer <- liftEffect $ createBufferF device sphereData GPUBufferUsage.storage
    hitsBuffer <- liftEffect $ createBuffer device $ x
      { size: hitsBufferSize
      -- , usage: GPUBufferUsage.storage
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: canvasBufferSize
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    let
      clearSpheresDesc = x
        { code:
            fold
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read_write> result_matrix : array<u32>;
@group(1) @binding(0) var<storage, read> sphere_info : array<f32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width * rendering_info.canvas_height * rendering_info.n_spheres) {
    return;
  }
  
  result_matrix[global_id.x] = 0u;
}"""
              ]
        }
    clearSpheresModule <- liftEffect $ createShaderModule device clearSpheresDesc
    let
      (clearSpheresStage :: GPUProgrammableStage) = x
        { "module": clearSpheresModule
        , entryPoint: "main"
        }
    let
      hitDesc = x
        { code:
            fold
              [ lerp
              , lerpv
              , inputData
              , ray
              , pointAtParameter
              , hitSphere
              , usefulConsts
              , boundNormalization
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read_write> result_matrix : array<atomic<u32>>;
@group(1) @binding(0) var<storage, read> sphere_info : array<f32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width * rendering_info.canvas_height * rendering_info.n_spheres) {
    return;
  }
  var cwch = rendering_info.real_canvas_width * rendering_info.canvas_height;
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var ambitus_x = select(2.0 * aspect, 2.0, aspect < 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var p_x = f32(global_id.x % rendering_info.real_canvas_width) / f32(rendering_info.real_canvas_width);
  var p_y = 1. - f32((global_id.x / rendering_info.real_canvas_width) % rendering_info.canvas_height) / f32(rendering_info.canvas_height);
  var sphere = global_id.x / cwch;
  var r: ray;
  r.origin = origin;
  r.direction = lower_left_corner + vec3(p_x * ambitus_x, p_y * ambitus_y, 0.0);
  var hit_t: f32;
  var sphere_ix = sphere * 4;
  var i_was_hit = hit_sphere(sphere_info[sphere_ix], sphere_info[sphere_ix + 1], sphere_info[sphere_ix + 2], sphere_info[sphere_ix + 3], &r, 0.0001, 1000.f, &hit_t);
  if (i_was_hit) {
    var norm_t = 1.f / hit_t;
    var sphere_idx = f32(sphere);
    _ = atomicMax(&result_matrix[global_id.x % cwch], pack2x16float(vec2<f32>(sphere_idx, norm_t)));
  }
}"""
              ]
        }
    hitModule <- liftEffect $ createShaderModule device hitDesc
    let
      (hitStage :: GPUProgrammableStage) = x
        { "module": hitModule
        , entryPoint: "main"
        }
    let
      masterpieceDesc = x
        { code:
            fold
              [ lerp
              , lerpv
              , inputData
              , ray
              , pointAtParameter
              , hitRecord
              , makeHitRec
              , usefulConsts
              , boundNormalization
              , """
// color
fn hit_color(r: ptr<function,ray>, rec: ptr<function,hit_record>) -> vec3<f32> {
  var normal = (*rec).normal;
  return 0.5 * vec3<f32>(normal.x + 1.0, normal.y + 1.0, normal.z + 1.0);
}

fn sky_color(r: ptr<function,ray>) -> vec3<f32> {
  var unit_direction = normalize((*r).direction);
  var t = 0.5 * (unit_direction.y + 1.0);
  var white = vec3<f32>(1.0, 1.0, 1.0);
  var sky_blue = vec3<f32>(0.5, 0.7, 1.0);
  return lerpv(&white, &sky_blue, t);
}

// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read_write> result_matrix : array<u32>;
@group(1) @binding(0) var<storage, read> sphere_info : array<f32>;
@group(2) @binding(0) var<storage, read> hit_info : array<u32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width * rendering_info.canvas_height) {
    return;
  }
  var cwch = rendering_info.real_canvas_width * rendering_info.canvas_height;
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var ambitus_x = select(2.0 * aspect, 2.0, aspect < 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var p_x = f32(global_id.x % rendering_info.real_canvas_width) / f32(rendering_info.real_canvas_width);
  var p_y = 1. - f32((global_id.x / rendering_info.real_canvas_width) % rendering_info.canvas_height) / f32(rendering_info.canvas_height);
  var sphere = global_id.x / (rendering_info.real_canvas_width * rendering_info.canvas_height);
  var r: ray;
  r.origin = origin;
  r.direction = lower_left_corner + vec3(p_x * ambitus_x, p_y * ambitus_y, 0.0);
  var hit_bound = global_id.x;
  var bound = ((global_id.x / rendering_info.real_canvas_width) * rendering_info.overshot_canvas_width) + (global_id.x % rendering_info.real_canvas_width) ;
  var was_i_hit = hit_info[hit_bound];
  if (was_i_hit == 0u) {
    var my_color = sky_color(&r);
    result_matrix[bound] = pack4x8unorm(vec4<f32>(my_color.b, my_color.g, my_color.r, 1.f));
  } else {
    var unpacked = unpack2x16float(was_i_hit);
    var sphere_idx = u32(unpacked[0]);
    var sphere_offset = sphere_idx * 4;
    var norm_t = 1.f / unpacked[1];
    var rec: hit_record;
    _ = make_hit_rec(sphere_info[sphere_offset], sphere_info[sphere_offset + 1], sphere_info[sphere_offset + 2], sphere_info[sphere_offset + 3], norm_t, &r, &rec);
    var my_color = hit_color(&r, &rec);
    result_matrix[bound] = pack4x8unorm(vec4<f32>(my_color.b, my_color.g, my_color.r, 1.f));
   }
}
"""
              ]
        }
    masterpieceModule <- liftEffect $ createShaderModule device masterpieceDesc
    let
      (masterpieceStage :: GPUProgrammableStage) = x
        { "module": masterpieceModule
        , entryPoint: "main"
        }
    rwBindGroupLayout <- liftEffect $ createBindGroupLayout device
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
    rBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    rwWithSpheresPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ rwBindGroupLayout, rBindGroupLayout ] }
    rwWithSpheresAndHitsPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ rwBindGroupLayout, rBindGroupLayout, rBindGroupLayout ] }
    masterpieceBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rwBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: canvasInfoBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      }
    hitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rwBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: canvasInfoBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: hitsBuffer } :: GPUBufferBinding)
          ]
      }
    spheresBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: sphereBuffer } :: GPUBufferBinding)
          ]
      }
    justHitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: hitsBuffer } :: GPUBufferBinding)
          ]
      }
    clearSpheresPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: rwWithSpheresPipelineLayout
      , compute: clearSpheresStage
      }
    hitComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: rwWithSpheresPipelineLayout
      , compute: hitStage
      }
    masterpieceComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: rwWithSpheresAndHitsPipelineLayout
      , compute: masterpieceStage
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
        cinfo <- fromArray $ map fromInt [ canvasWidth, overshotWidth, canvasHeight, nSpheres, 0 ]
        let asBuffer = buffer cinfo
        whole asBuffer >>= \(x :: Float32Array) -> void $ set x (Just 4) [ fromNumber' tn ]
        writeBuffer queue canvasInfoBuffer 0 (fromUint32Array cinfo)
        -- not necessary in the loop, but useful as a stress test for animating positions
        computePassEncoder <- beginComputePass commandEncoder (x {})
        -- clear spheres
        GPUComputePassEncoder.setPipeline computePassEncoder clearSpheresPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          hitsBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          spheresBindGroup
        GPUComputePassEncoder.dispatchWorkgroups computePassEncoder $ ceil (toNumber (canvasWidth * canvasHeight * nSpheres) / 64.0)
        -- spheres
        GPUComputePassEncoder.setPipeline computePassEncoder
          hitComputePipeline
        GPUComputePassEncoder.dispatchWorkgroups computePassEncoder $ ceil (toNumber (canvasWidth * canvasHeight * nSpheres) / 64.0)
        -- masterpiece
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          masterpieceBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 2
          justHitsBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder
          masterpieceComputePipeline
        GPUComputePassEncoder.dispatchWorkgroups computePassEncoder $ ceil (toNumber (canvasWidth * canvasHeight) / 64.0)
        --
        GPUComputePassEncoder.end computePassEncoder
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
