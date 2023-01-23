module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Gen (elements)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.Array (intercalate, length, replicate, (!!), (..))
import Data.Array.NonEmpty (NonEmptyArray, cons', drop, fromNonEmpty, snoc, snoc', sortBy, take, toArray, uncons)
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (class TypedArray, buffer, fromArray, set, setTyped, whole)
import Data.ArrayBuffer.Typed as Typed
import Data.ArrayBuffer.Types (ArrayView, Float32Array, Uint32Array)
import Data.Float32 (fromNumber')
import Data.Float32 as F
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (ceil, floor, toNumber)
import Data.Int.Bits (complement, shl, (.&.))
import Data.JSDate (getTime, now)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Number (pow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.UInt (fromInt)
import Debug (spy)
import Deku.Attribute ((!:=))
import Deku.Attributes (id_, klass_)
import Deku.Control (text, text_, (<#~>))
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect, foreachE)
import Effect.Aff (Milliseconds(..), delay, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Random (random, randomInt)
import Effect.Ref as Ref
import FRP.Event (create)
import QualifiedDo.Alt as Alt
import Random.LCG (mkSeed)
import Record (union)
import Test.QuickCheck.Gen (Gen, evalGen)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.GPU.BufferSource (fromUint32Array)
import Web.GPU.GPU (requestAdapter)
import Web.GPU.GPUAdapter (requestDevice)
import Web.GPU.GPUBindGroupEntry (GPUBufferBinding, gpuBindGroupEntry)
import Web.GPU.GPUBindGroupLayoutEntry (gpuBindGroupLayoutEntry)
import Web.GPU.GPUBuffer (GPUBuffer, getMappedRange, mapAsync, unmap)
import Web.GPU.GPUBufferBindingLayout (GPUBufferBindingLayout)
import Web.GPU.GPUBufferBindingType as GPUBufferBindingType
import Web.GPU.GPUBufferUsage (GPUBufferUsageFlags)
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasAlphaMode (opaque)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (configure, getCurrentTexture)
import Web.GPU.GPUCommandEncoder (beginComputePass, copyBufferToTexture, finish)
import Web.GPU.GPUComputePassEncoder as GPUComputePassEncoder
import Web.GPU.GPUDevice (GPUDevice, createBindGroup, createBindGroupLayout, createBuffer, createCommandEncoder, createComputePipeline, createPipelineLayout, createShaderModule, limits)
import Web.GPU.GPUDevice as GPUDevice
import Web.GPU.GPUExtent3D (gpuExtent3DWH)
import Web.GPU.GPUMapMode as GPUMapMode
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

testNSpheres :: Int
testNSpheres = 512

testAntiAliasMax :: Int
testAntiAliasMax = 4

testBounces :: Int
testBounces = 32

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
  n_bvh_nodes: u32, // number of bvh_nodes
  anti_alias_passes: u32, // number of spheres
  current_time: f32 // current time in seconds
}

struct position_info {
  x: u32,
  y: u32,
  z: u32,
  c: atomic<u32>
}
"""

type NodeBounds =
  ( aabb_min_x :: Number
  , aabb_min_y :: Number
  , aabb_min_z :: Number
  , aabb_max_x :: Number
  , aabb_max_y :: Number
  , aabb_max_z :: Number
  )

newtype BVHNode = BVHNode
  { left :: Int
  , right :: Int
  , parent :: Int
  , is_sphere :: Int
  | NodeBounds
  }

derive instance Newtype BVHNode _
derive newtype instance Show BVHNode

type Sphere' =
  { cx :: Number
  , cy :: Number
  , cz :: Number
  , radius :: Number
  }

newtype XYZ = XYZ { x :: Int, y :: Int, z :: Int }
derive newtype instance Show XYZ
interpolateXYZOverRange :: (Number -> Number) -> Int -> XYZ -> Array XYZ
interpolateXYZOverRange f i (XYZ { x, y, z }) = (0 .. (i - 1)) <#> \j ->
  let
    m = f ((toNumber j) / (toNumber i))
  in
    XYZ
      { x: ceil (toNumber x * m)
      , y: ceil (toNumber y * m)
      , z: ceil (toNumber z * m)
      }

interpolate :: Number -> Number -> Number -> Number -> Number -> Number
interpolate x0 y0 x1 y1 x =
  let
    m = (y1 - y0) / (x1 - x0)
  in
    y0 + m * (x - x0)

keepEverythingTheSameForABitThenDecreaseProgressivelyToAroundOneEighth :: Number -> Number
keepEverythingTheSameForABitThenDecreaseProgressivelyToAroundOneEighth x =
  if x < 0.1 then 1.0
  else interpolate 0.1 1.0 1.0 0.125 x

newtype Sphere = Sphere Sphere'

derive instance Newtype Sphere _
derive newtype instance Show Sphere

data Axis = XAxis | YAxis | ZAxis

spheresToFlatRep :: NonEmptyArray Sphere -> Array Number
spheresToFlatRep arr = join $ toArray $ map (\(Sphere { cx, cy, cz, radius }) -> [ cx, cy, cz, radius ]) arr

spheresToBVHNodes :: Int -> NonEmptyArray Sphere -> NonEmptyArray BVHNode
spheresToBVHNodes seed arr = out
  where
  out = mapWithIndex (\i (BVHNode x) -> BVHNode (x { parent = fromMaybe 0 $ Map.lookup i parMap })) initialRes
  parMap =
    Map.fromFoldable
      $ join
      $ mapWithIndex (\i (BVHNode { left, right, is_sphere }) -> if is_sphere == 1 then [] else [ Tuple left i, Tuple right i ])
      $ toArray initialRes
  initialRes = (evalGen (go [] (mapWithIndex Tuple arr)) { newSeed: mkSeed seed, size: 10 }).array

  go
    :: Array BVHNode
    -> NonEmptyArray (Tuple Int Sphere)
    -> Gen
         { array :: NonEmptyArray BVHNode
         , index :: Int
         , n :: BVHNode
         }
  go bvhs spheres = do
    let { head, tail } = uncons spheres
    case tail of
      [] -> do
        let Tuple i (Sphere a) = head
        let
          n = BVHNode
            { aabb_min_x: a.cx - a.radius
            , aabb_min_y: a.cy - a.radius
            , aabb_min_z: a.cz - a.radius
            , aabb_max_x: a.cx + a.radius
            , aabb_max_y: a.cy + a.radius
            , aabb_max_z: a.cz + a.radius
            , left: i
            , parent: 0
            , right: 0
            , is_sphere: 1
            }
        pure
          { array: snoc' bvhs n
          , index: length bvhs
          , n
          }
      _ -> do
        i <- elements (fromNonEmpty $ NonEmpty XAxis [ YAxis, ZAxis ])
        let sorted = sortAlong i spheres
        -- we have proof that this is at least 2, so we can use unsafeCoerce
        let l = take (NEA.length sorted / 2) sorted
        let r = drop (NEA.length sorted / 2) sorted
        { array: bvhsL, index: leftIndex, n: nl } <- go bvhs ((unsafeCoerce :: Array ~> NonEmptyArray) l)
        { array: bvhsR, index: rightIndex, n: nr } <- go (toArray bvhsL) ((unsafeCoerce :: Array ~> NonEmptyArray) r)
        let sb = surroundingBox nl nr
        let
          n = BVHNode
            ( sb `union`
                { left: leftIndex
                , right: rightIndex
                , parent: 0
                , is_sphere: 0
                }
            )
        pure
          { array: snoc bvhsR n
          , index: NEA.length bvhsR
          , n
          }

  surroundingBox :: BVHNode -> BVHNode -> { | NodeBounds }
  surroundingBox (BVHNode box0) (BVHNode box1) =
    { aabb_min_x: min box0.aabb_min_x box1.aabb_min_x
    , aabb_min_y: min box0.aabb_min_y box1.aabb_min_y
    , aabb_min_z: min box0.aabb_min_z box1.aabb_min_z
    , aabb_max_x: max box0.aabb_max_x box1.aabb_max_x
    , aabb_max_y: max box0.aabb_max_y box1.aabb_max_y
    , aabb_max_z: max box0.aabb_max_z box1.aabb_max_z
    }

  cf :: (Sphere' -> Number) -> Tuple Int Sphere -> Tuple Int Sphere -> Ordering
  cf f = (compare `on` (snd >>> unwrap >>> f))

  sortAlong :: Axis -> NonEmptyArray (Tuple Int Sphere) -> NonEmptyArray (Tuple Int Sphere)
  sortAlong axis iarr = case axis of
    XAxis -> sortBy (cf _.cx) iarr
    YAxis -> sortBy (cf _.cy) iarr
    ZAxis -> sortBy (cf _.cz) iarr

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

bvhNodesToFloat32Array :: NonEmptyArray BVHNode -> Effect Float32Array
bvhNodesToFloat32Array arr = do
  flar <- join <$> traverse go (toArray arr)
  fromArray flar
  where
  go :: BVHNode -> Effect (Array F.Float32)
  go
    ( BVHNode
        { aabb_min_x
        , aabb_min_y
        , aabb_min_z
        , aabb_max_x
        , aabb_max_y
        , aabb_max_z
        , left
        , right
        , parent
        , is_sphere
        }
    ) = do
    tl :: Uint32Array <- fromArray [ fromInt left, fromInt right, fromInt parent, fromInt is_sphere ]
    tlv :: Array F.Float32 <- (whole :: _ -> Effect Float32Array) (DV.buffer (DV.whole (Typed.buffer tl))) >>= Typed.toArray
    pure $
      [ fromNumber' aabb_min_x
      , fromNumber' aabb_min_y
      , fromNumber' aabb_min_z
      , fromNumber' aabb_max_x
      , fromNumber' aabb_max_y
      , fromNumber' aabb_max_z
      ] <> tlv

gpuMe :: Effect Unit -> (FrameInfo -> Effect Unit) -> HTMLCanvasElement -> Effect Unit
gpuMe showErrorMessage pushFrameInfo canvas = launchAff_ $ delay (Milliseconds 250.0) *> liftEffect do
  context <- getContext canvas >>= maybe
    (showErrorMessage *> throwError (error "could not find context"))
    pure
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
    deviceLimits <- liftEffect $ limits device
    --------------------------
    -- range
    let
      xyzs =
        interpolateXYZOverRange keepEverythingTheSameForABitThenDecreaseProgressivelyToAroundOneEighth (8 * 16) (XYZ { x: 64, y: 32, z: 32 })
    positions :: Uint32Array <- liftEffect $ fromArray $ join $ (map (\(XYZ { x, y, z }) -> [ fromInt x, 
    fromInt y, fromInt z ] <> map fromInt (replicate 61 0)) xyzs)
    positionBuffer <- liftEffect $ createBufferF device positions GPUBufferUsage.storage
    canvasInfoBuffer <- liftEffect $ createBuffer device $ x
      { size: 28 -- align(4) size(28)
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.storage
      }
    bvhNodeBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    sphereBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    seed <- liftEffect $ randomInt 42 42424242
    randos <- liftEffect $ sequence $ replicate (testNSpheres - 2) $ Sphere <$> ({ cx: _, cy: 0.25, cz: _, radius: 0.06 } <$> (random <#> \n -> n * 8.0 - 4.0) <*> (random <#> \n -> n * 8.0 - 4.0))
    let
      spheres =
        cons' (Sphere { cx: 0.0, cy: 0.0, cz: -1.0, radius: 0.5 })
          ( [ Sphere { cx: 0.0, cy: -100.5, cz: -1.0, radius: 100.0 }
            ] <> randos
          )
      bvhNodes = spheresToBVHNodes seed spheres
      rawSphereData = map fromNumber' (spheresToFlatRep spheres)
    --logShow bvhNodes
    --logShow spheres
    bvhNodeData <- liftEffect $ bvhNodesToFloat32Array bvhNodes
    let nSpheres = NEA.length spheres
    let nBVHNodes = NEA.length bvhNodes
    sphereData :: Float32Array <- liftEffect $ fromArray rawSphereData
    workgroupManagementBuffer <- liftEffect $ createBuffer device $ x
      { size: 1 `shl` 15
      , usage: GPUBufferUsage.storage
      }
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    let
      dasUbershaderDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> result_array : array<u32>;
@group(2) @binding(0) var<storage, read_write> workgroup_limits : position_info;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.z * rendering_info.real_canvas_width * rendering_info.canvas_height + global_id.y * rendering_info.real_canvas_width + global_id.x;
  if (ix >= rendering_info.real_canvas_width * rendering_info.canvas_height * rendering_info.anti_alias_passes) {
    return;
  }
  var m = vec4(sin(f32(global_id.x))*0.5+0.5,0.0,0.0,1.0);
  m.y = sin(f32(global_id.y))*0.5+0.5;
  m.z = cos(f32(global_id.z))*0.5+0.5;
  if (global_id.x % 2u == 0u) {
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
      m.x = sin(f32(m.x))*0.5+0.5;
      m.y = sin(f32(m.y))*0.5+0.5;
      m.z = cos(f32(m.z))*0.5+0.5;
  } else {
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;
      m.x = cos(f32(m.x))*0.5+0.5;
      m.y = cos(f32(m.y))*0.5+0.5;
      m.z = sin(f32(m.z))*0.5+0.5;    
  }
  result_array[ix] = pack4x8unorm(m);
}"""
              ]
        }
    dasUbershaderModule <- liftEffect $ createShaderModule device dasUbershaderDesc
    let
      (dasUbershaderStage :: GPUProgrammableStage) = x
        { "module": dasUbershaderModule
        , entryPoint: "main"
        }
    readerBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [
                -- info about the current scene
                gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              -- spheres
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              -- bounding boxes
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    wBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    -- for when we are reading from a context and writing to a buffer
    readOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout, wBindGroupLayout ] }
    readerBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: readerBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: canvasInfoBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: sphereBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: bvhNodeBuffer } :: GPUBufferBinding)
          ]
      }
    dasUbershaderBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      }
    let
      makePositionBindGroup offset = createBindGroup device $ x
        { layout: wBindGroupLayout
        , entries:
            [ gpuBindGroupEntry 0
                (x { buffer: wholeCanvasBuffer, offset } :: GPUBufferBinding)
            ]
        }
    dasUbershaderPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: dasUbershaderStage
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
    loopN <- liftEffect $ Ref.new 0
    logShow xyzs
    let maxStorageBufferBindingSize = deviceLimits.maxStorageBufferBindingSize
    let
      encodeCommands colorTexture = do
        whichLoop <- Ref.modify (_ + 1) loopN
        cw <- clientWidth (toElement canvas)
        ch <- clientHeight (toElement canvas)
        setWidth (floor cw) canvas
        setHeight (floor ch) canvas
        canvasWidth <- width canvas
        canvasHeight <- height canvas
        let bufferWidth = ceil (toNumber canvasWidth * 4.0 / 256.0) * 256
        let overshotWidth = bufferWidth / 4
        let antiAliasPasses = testAntiAliasMax -- min testAntiAliasMax $ ceil (toNumber maxStorageBufferBindingSize / (toNumber (canvasWidth * canvasHeight * nSpheres * 4)))
        -- logShow antiAliasPasses
        tn <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
        cf <- Ref.read currentFrame
        Ref.write (cf + 1) currentFrame
        commandEncoder <- createCommandEncoder device (x {})
        let workgroupX = ceil (toNumber canvasWidth / 16.0)
        let workgroupY = ceil (toNumber canvasHeight / 16.0)
        cinfo <- fromArray $ map fromInt
          [ canvasWidth
          , overshotWidth
          , canvasHeight
          , nSpheres
          , nBVHNodes
          , antiAliasPasses
          , 0
          ]
        let asBuffer = buffer cinfo
        let wgx = 1 `shl` 12
        let wgy = 8
        let wgz = 8
        whole asBuffer >>= \(x :: Float32Array) -> void $ set x (Just 6) [ fromNumber' tn ]
        writeBuffer queue canvasInfoBuffer 0 (fromUint32Array cinfo)
        -- not necessary in the loop, but useful as a stress test for animating positions
        computePassEncoder <- beginComputePass commandEncoder (x {})
        -- set reader for all computations
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          readerBindGroup
        -- clear workgroups
        GPUComputePassEncoder.setPipeline computePassEncoder dasUbershaderPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          dasUbershaderBindGroup
        foreachE (mapWithIndex Tuple xyzs) \(Tuple i (XYZ { x,y,z})) -> do
          GPUComputePassEncoder.setBindGroup computePassEncoder 2
            =<< makePositionBindGroup (i * 256)
          GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder x y z
        --
        GPUComputePassEncoder.end computePassEncoder
        copyBufferToTexture
          commandEncoder
          (x { buffer: wholeCanvasBuffer, bytesPerRow: bufferWidth })
          (x { texture: colorTexture })
          (gpuExtent3DWH canvasWidth canvasHeight)
        toSubmit <- finish commandEncoder
        submit queue [ toSubmit ]
        tnz <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
        pure unit
        launchAff_ do
          toAffE $ convertPromise <$> onSubmittedWorkDone queue
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
            klass_ "absolute p-3 text-slate-400"
          [ errorMessage.event $> false <|> pure true <#~>
              if _ then
                text (_.avgTime >>> show >>> ("Avg time: " <> _) <$> frameInfo.event)
              else text_ "Your device does not support WebGPU"
          ]
      , D.div
          Alt.do
            id_ "debug-gpu"
          []
      ]