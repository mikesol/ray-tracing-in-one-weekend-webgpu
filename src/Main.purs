module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Gen (elements)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.Array (fold, intercalate, length, replicate)
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
import Data.Int.Bits (complement, (.&.))
import Data.JSDate (getTime, now)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.UInt (fromInt)
import Debug (spy)
import Deku.Attribute ((!:=))
import Deku.Attributes (id_, klass_)
import Deku.Control (text, text_, (<#~>))
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
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

kernelX :: Int
kernelX = 16

kernelY :: Int
kernelY = 4

squareSide :: Int
squareSide = 8

squareArea = squareSide * squareSide :: Int

kernelSize :: Int
kernelSize = kernelX * kernelY

antiAliasPasses :: Int
antiAliasPasses = 1

totalPixels :: Int
totalPixels = squareArea * antiAliasPasses

arrLength = 512 :: Int

overcommit :: Int
overcommit = totalPixels / kernelSize

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
"""

aabb :: String
aabb =
  """

struct aabb {
  aabb_min: vec3<f32>,
  aabb_max: vec3<f32>
}

fn aabb_hit(bounds: ptr<function,aabb>, r: ptr<function,ray>, tmin: f32, tmax: f32) -> bool
{
  var a: u32 = 0;
  loop {
    if a >= 3 {
      break;
    }
    var invD = 1.0 / (*r).direction[a];
    var t0 = ((*bounds).aabb_min[a] - (*r).origin[a]) * invD;
    var t1 = ((*bounds).aabb_max[a] - (*r).origin[a]) * invD;
    if (invD < 0.0) {
      var tmp = t0;
      t0 = t1;
      t1 = tmp;
    }
    var bmin = select(tmin, t0, t0 > tmin); // t0 > tmin ? t0 : tmin;
    var bmax = select(tmax, t1, t1 < tmax); // t1 < tmax ? t1 : tmax;
    if (bmax <= bmin) {
      return false;
    }
    a++;
  }
  return true;
}
"""

bvhNode :: String
bvhNode =
  """
struct bvh_node {
  aabb_min_x: f32,
  aabb_min_y: f32,
  aabb_min_z: f32,
  aabb_max_x: f32,
  aabb_max_y: f32,
  aabb_max_z: f32,
  left: u32,
  right: u32,
  parent: u32,
  is_sphere: u32
}

fn bvh_node_bounding_box(node: ptr<function,bvh_node>, box: ptr<function,aabb>) -> bool
{
  (*box).aabb_min = vec3<f32>((*node).aabb_min_x, (*node).aabb_min_y, (*node).aabb_min_z);
  (*box).aabb_max = vec3<f32>((*node).aabb_max_x, (*node).aabb_max_y, (*node).aabb_max_z);
  return true;
}

"""

newtype HitBVHInfo = HitBVHInfo
  { hitTName :: String
  , nodesName :: String
  , rName :: String
  , spheresName :: String
  , tMaxName :: String
  , tMinName :: String
  }

sphereBoundingBox :: String
sphereBoundingBox =
  """
fn sphere_bounding_box(cx: f32, cy: f32, cz: f32, radius: f32, box: ptr<function,aabb>) -> bool
{
  var center = vec3(cx, cy, cz);
  (*box).aabb_min = center - vec3(radius, radius, radius);
  (*box).aabb_max = center + vec3(radius, radius, radius);
  return true;
}
  """

antiAliasFuzzing :: String
antiAliasFuzzing =
  """
const fuzz_fac = 1.0;
const half_fuzz_fac = fuzz_fac / 2.0;
fn fuzz2(i: u32, n: u32, d: u32) -> f32
{
    var fi = f32(i);
    return (fi + (fuzz_fac * pow(f32(n) / f32(d), 0.5) - half_fuzz_fac));
}
fn fuzz3(i: u32, n: u32, d: u32) -> f32
{
    var fi = f32(i);
    return fi;
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
const color_mult = 1 << 8;
const origin = vec3(0.0, 0.0, 0.0);
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
    canvasInfoBuffer <- liftEffect $ createBuffer device $ x
      { size: 28 -- align(4) size(28)
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.storage
      }
    -- debugBuffer <- liftEffect $ createBuffer device $ x
    --   { size: 65536
    --   , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
    --   }
    debugOutputBuffer <- liftEffect $ createBuffer device $ x
      { size: 65536
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.mapRead
      }
    seed <- liftEffect $ randomInt 42 42424242
    randos <- liftEffect $ sequence $ replicate 512 $ Sphere <$> ({ cx: _, cy: 0.25, cz: _, radius: 0.125 } <$> (random <#> \n -> n * 16.0 - 8.0) <*> (random <#> \n -> n * 16.0 - 8.0))
    let
      spheres =
        cons' (Sphere { cx: 0.0, cy: 0.0, cz: -1.0, radius: 0.5 })
          ( [ Sphere { cx: 0.0, cy: -100.5, cz: -1.0, radius: 100.0 }
            ] <> randos
          )
      bvhNodes = spheresToBVHNodes seed spheres
      rawSphereData = map fromNumber' (spheresToFlatRep spheres)
    logShow bvhNodes
    logShow spheres
    bvhNodeData <- liftEffect $ bvhNodesToFloat32Array bvhNodes
    let nSpheres = NEA.length spheres
    let nBVHNodes = NEA.length bvhNodes
    sphereData :: Float32Array <- liftEffect $ fromArray rawSphereData
    sphereBuffer <- liftEffect $ createBufferF device sphereData GPUBufferUsage.storage
    bvhNodeBuffer <- liftEffect $ createBufferF device bvhNodeData GPUBufferUsage.storage
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    let
      clearColorsBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> result_array : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  result_array[global_id.y * rendering_info.overshot_canvas_width + global_id.x] = 0u;
}"""
              ]
        }
    clearColorsBufferModule <- liftEffect $ createShaderModule device clearColorsBufferDesc
    let
      (clearColorsBufferStage :: GPUProgrammableStage) = x
        { "module": clearColorsBufferModule
        , entryPoint: "main"
        }
    let
      mainComputeDesc = x
        { code: spy "text" $ fold
            [ lerp
            , lerpv
            , inputData
            , ray
            , antiAliasFuzzing
            , pointAtParameter
            , hitSphere
            , hitRecord
            , makeHitRec
            , aabb
            , bvhNode
            , sphereBoundingBox
            , usefulConsts
            , """
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

fn pix_from_pixvol(pixvol: u32) -> u32 {
  return pixvol & 0xffff;
}

fn vol_from_pixvol(pixvol: u32) -> u32 {
  return pixvol >> 16;
}

fn pix_and_vol_to_pixvol(pix: u32, vol: u32) -> u32 {
  return pix | (vol << 16);
}

var<workgroup> pxs: array<f32, """
            , show totalPixels
            , """>;
var<workgroup> pys: array<f32, """
            , show totalPixels
            , """>;
var<workgroup> rays: array<ray, """
            , show totalPixels
            , """>;
var<workgroup> hits: array<f32, """
            , show totalPixels
            , """>;
var<workgroup> colors: array<vec3<f32>, """
            , show totalPixels
            , """>;
var<workgroup> sphere_indices: array<u32, """
            , show totalPixels
            , """>;
var<workgroup> bvh_read_playhead: u32;
var<workgroup> bvh_write_playhead: atomic<u32>;
var<workgroup> sphere_read_playhead: u32;
var<workgroup> sphere_write_playhead: atomic<u32>;
var<workgroup> bvh_ixs: array<u32, """
            , show (arrLength)
            , """>;
var<workgroup> sphere_ixs: array<u32, """
            , show (arrLength)
            , """>;

@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read> sphere_info : array<f32>;
@group(0) @binding(2) var<storage, read> bvh_nodes : array<bvh_node>;
@group(1) @binding(0) var<storage, read_write> result_array : array<u32>;
@compute @workgroup_size("""
            , show kernelX
            , """, """
            , show kernelY
            , """, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>, @builtin(local_invocation_id) local_id : vec3<u32>) {
  var aa_f32 = f32("""
            , show antiAliasPasses
            , """);
  var cwch = rendering_info.real_canvas_width * rendering_info.canvas_height;
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var ambitus_x = select(2.0 * aspect, 2.0, aspect < 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var t_min = 0.0001;
  var t_max = 10000.f;
  var locy = local_id.y * """
            , show kernelX
            , """;
  var locxy = locy + local_id.x;
  var last_node_ix = rendering_info.n_bvh_nodes - 1;
  var last_node = bvh_nodes[last_node_ix];
  for (var i: u32 = 0; i < """
            , show totalPixels
            , """;  i += """
            , show kernelSize
            , """) {
    var ix = locxy + i;
    var xk = ix % """
            , show squareSide
            , """;
    var yk = (ix / """
            , show squareSide
            , """) % """
            , show squareSide
            , """;
    var zk = ix / """
            , show squareArea
            , """;
    var real_x = (global_id.x / """
            , show kernelX
            , """ ) * """
            , show squareSide
            , """ + xk;
    var real_y = (global_id.y / """
            , show kernelY
            , """) * """
            , show squareSide
            , """ + yk;
    var px = fuzz2(real_x, zk,"""
            , show antiAliasPasses
            , """) / f32(rendering_info.real_canvas_width);
    pxs[ix] = px;
    var py = 1. - fuzz2(real_y, zk, """
            , show antiAliasPasses
            , """) / f32(rendering_info.canvas_height);
    pys[ix] = py;
    rays[ix].origin = origin;
    rays[ix].direction = lower_left_corner + vec3(px * ambitus_x, py * ambitus_y, 0.0);
    hits[ix] = 1000.f;
    sphere_indices[ix] = 0u;
    var last_node_pixvol = pix_and_vol_to_pixvol(ix, last_node_ix);
    if (last_node.is_sphere == 1u) {
      sphere_ixs[ix] = last_node_pixvol;
    } else {
      bvh_ixs[ix] = last_node_pixvol;
    }
  }
  if (locxy == 0) {
    if (last_node.is_sphere == 1u) {
      atomicStore(&sphere_write_playhead, """
            , show totalPixels
            , """);
    } else {
      atomicStore(&bvh_write_playhead, """
            , show totalPixels
            , """);
    }
    sphere_read_playhead = 0u;
    bvh_read_playhead = 0u;
  }
  var bvh_playhead = 0u;
  var sphere_playhead = 0u;
  var currentSphereWrite = atomicLoad(&sphere_write_playhead);
  var currentBVHWrite = atomicLoad(&bvh_write_playhead);
  var currentSphereRead = sphere_read_playhead;
  var currentBVHRead = bvh_read_playhead;
  var currentBVH = currentBVHWrite - currentBVHRead;
  var currentSphere = currentSphereWrite - currentSphereRead;
  var currentOpN = max(currentBVH, currentSphere);
  var currentOp = select(1, 0, currentOpN == currentBVH);
  workgroupBarrier();
  var is_done = false;
  for (var safe_cutoff: u32 = 0; safe_cutoff < 512u; safe_cutoff++) {
    if (!is_done) {
      currentOpN = min("""
            , show kernelSize
            , """, currentOpN);
      if (locxy < currentOpN) {

        switch currentOp {
          default: {}
          case 0: {
            // bvh
            var bloc = bvh_ixs[(locxy + currentBVHRead) % """
            , show (arrLength)
            , """];
            var bloc_pix = pix_from_pixvol(bloc);
            var node = bvh_nodes[vol_from_pixvol(bloc)];
            var ray = rays[bloc_pix];
            var bbox: aabb;
            bvh_node_bounding_box(&node, &bbox);
            var was_aabb_hit = aabb_hit(&bbox, &ray, t_min, t_max);
            /////
            if (was_aabb_hit) {
              if (bvh_nodes[node.left].is_sphere == 1u) {
                var ix = atomicAdd(&sphere_write_playhead, 1u);
                sphere_ixs[(ix) % """
            , show (arrLength)
            , """] = pix_and_vol_to_pixvol(bloc_pix, node.left);
              } else {
                var ix = atomicAdd(&bvh_write_playhead, 1u);
                bvh_ixs[(ix) % """
            , show (arrLength)
            , """] = pix_and_vol_to_pixvol(bloc_pix, node.left);
              }
              if (bvh_nodes[node.right].is_sphere == 1u) {
                var ix = atomicAdd(&sphere_write_playhead, 1u);
                sphere_ixs[(ix) % """
            , show (arrLength)
            , """] = pix_and_vol_to_pixvol(bloc_pix, node.right);
              } else {
                var ix = atomicAdd(&bvh_write_playhead, 1u);
                bvh_ixs[(ix) % """
            , show (arrLength)
            , """] = pix_and_vol_to_pixvol(bloc_pix, node.right);
              }
            }
          }
          case 1: {
            // sphere
            var sloc = sphere_ixs[(locxy + currentSphereRead) % """
            , show (arrLength)
            , """];
            var node = bvh_nodes[vol_from_pixvol(sloc)];
            var sloc_pix = pix_from_pixvol(sloc);
            var ray = rays[sloc_pix];
            var hit_t: f32;
            var sphere_ix = node.left * 4u;
            var sphere_hit = hit_sphere(
              sphere_info[sphere_ix],
              sphere_info[sphere_ix+1],
              sphere_info[sphere_ix+2],
              sphere_info[sphere_ix+3],
              &ray,
              t_min,
              t_max,
              &hit_t);
            var i_plus_1 = node.left + 1u;
            var old_t = hits[sloc_pix];
            var new_t = select(old_t, select(old_t, hit_t, hit_t < old_t), sphere_hit);
            hits[sloc_pix] = new_t;
            ///
            var old_ix = sphere_indices[sloc_pix];
            sphere_indices[sloc_pix] =
              select(old_ix, select(old_ix, i_plus_1, new_t != old_t), sphere_hit);
          }
        }
      }
    }
    workgroupBarrier();
    if (!is_done) {
      switch currentOp {
        default: {}
        case 0: {
          if (locxy == 0) {
            bvh_read_playhead += currentOpN;
          }
        }
        case 1: {
          if (locxy == 0) {
            sphere_read_playhead += currentOpN;
          }
        }
      }
    }
    workgroupBarrier();
    if (!is_done) {
      currentSphereWrite = atomicLoad(&sphere_write_playhead);
      currentBVHWrite = atomicLoad(&bvh_write_playhead);
      currentSphereRead = sphere_read_playhead;
      currentBVHRead = bvh_read_playhead;
      currentBVH = currentBVHWrite - currentBVHRead;
      currentSphere = currentSphereWrite - currentSphereRead;
      currentOpN = max(currentBVH, currentSphere);
      currentOp = select(1, 0, currentOpN == currentBVH);
      is_done = currentOpN == 0u;
    }
  }
  for (var i: u32 = 0; i < """
            , show totalPixels
            , """; i += """
            , show kernelSize
            , """) {
    var ix = locxy + i;
    var hit = hits[ix];
    var sphere_idx = sphere_indices[ix];
    var ray_of_light = rays[ix];
    var was_i_hit = sphere_idx > 0u;
    var my_color = vec3(0.0,0.0,0.0);
    if (!was_i_hit) {
      my_color = sky_color(&ray_of_light);
    } else {
      sphere_idx = sphere_idx - 1;
      var sphere_offset = sphere_idx * 4;
      var norm_t = hit;
      var rec: hit_record;
      _ = make_hit_rec(sphere_info[sphere_offset], sphere_info[sphere_offset + 1], sphere_info[sphere_offset + 2], sphere_info[sphere_offset + 3], norm_t, &ray_of_light, &rec);
      my_color = hit_color(&ray_of_light, &rec);
    }
    colors[ix] = my_color;
  }
  workgroupBarrier();

  // this last bit only needs to be done for the square grid
  if (locxy >= """
            , show squareArea
            , """) {
    return;
  }
  var bbb = 0.0;
  var ggg = 0.0; 
  var rrr = 0.0;
  for (var i: u32 = 0; i < """
            , show totalPixels
            , """; i += """
            , show squareArea
            , """) {
    var my_color = colors[locxy + i];
    bbb += (my_color.b / aa_f32);
    ggg += (my_color.g / aa_f32);
    rrr += (my_color.r / aa_f32);
  }

  var xk = locxy % """
            , show squareSide
            , """;
  var yk = (locxy / """
            , show squareSide
            , """) % """
            , show squareSide
            , """;
  var real_x = (global_id.x / """
            , show kernelX
            , """ ) * """
            , show squareSide
            , """ + xk;
  var real_y = (global_id.y / """
            , show kernelY
            , """) * """
            , show squareSide
            , """ + yk;
  var overshot_idx = real_x + (real_y * rendering_info.overshot_canvas_width);
  result_array[overshot_idx] = pack4x8unorm(vec4(bbb, ggg, rrr, 1.f));
  //result_array[overshot_idx] = pack4x8unorm(vec4(1.f, 0.3f, 0.7f, 1.f));
}
"""
            ]
        }
    mainComputeModule <- liftEffect $ createShaderModule device mainComputeDesc
    let
      (mainComputeStage :: GPUProgrammableStage) = x
        { "module": mainComputeModule
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
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout ] }
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
    wCanvasBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      }
    clearColorsBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: clearColorsBufferStage
      }
    mainComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: mainComputeStage
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
    -- loopN <- liftEffect $ Ref.new 0
    let
      encodeCommands colorTexture = do
        -- whichLoop <- Ref.modify (_ + 1) loopN
        canvasWidth <- width canvas
        canvasHeight <- height canvas
        let bufferWidth = ceil (toNumber canvasWidth * 4.0 / 256.0) * 256
        let overshotWidth = bufferWidth / 4
        tn <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
        cf <- Ref.read currentFrame
        Ref.write (cf + 1) currentFrame
        commandEncoder <- createCommandEncoder device (x {})
        let workgroupOvershootX = ceil (toNumber overshotWidth / 16.0)
        let workgroupOvershootY = ceil (toNumber overshotWidth / 16.0)
        let workgroupX = ceil (toNumber canvasWidth / toNumber squareSide)
        let workgroupY = ceil (toNumber canvasHeight / toNumber squareSide)
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
        whole asBuffer >>= \(x :: Float32Array) -> void $ set x (Just 6) [ fromNumber' tn ]
        writeBuffer queue canvasInfoBuffer 0 (fromUint32Array cinfo)
        -- not necessary in the loop, but useful as a stress test for animating positions
        computePassEncoder <- beginComputePass commandEncoder (x {})
        -- set reader for all computations
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          readerBindGroup
        -- clear colors as they're subject to an atomic operation
        GPUComputePassEncoder.setPipeline computePassEncoder clearColorsBufferPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wCanvasBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupOvershootX workgroupOvershootY 1
        ------------------------
        -- do bvh, color mapping and anti-aliasing
        GPUComputePassEncoder.setPipeline computePassEncoder
          mainComputePipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        --
        GPUComputePassEncoder.end computePassEncoder
        copyBufferToTexture
          commandEncoder
          (x { buffer: wholeCanvasBuffer, bytesPerRow: bufferWidth })
          (x { texture: colorTexture })
          (gpuExtent3DWH canvasWidth canvasHeight)
        -- copyBufferToBuffer commandEncoder debugBuffer 0 debugOutputBuffer 0 65536
        toSubmit <- finish commandEncoder
        submit queue [ toSubmit ]
        let debugCondition = false -- whichLoop == 100
        launchAff_ do
          toAffE $ convertPromise <$> if debugCondition then mapAsync debugOutputBuffer GPUMapMode.read else onSubmittedWorkDone queue
          liftEffect do
            when debugCondition do
              -- bfr <- getMappedRange debugOutputBuffer
              -- buffy <- (Typed.whole bfr :: Effect Uint32Array) >>= Typed.toArray
              -- let _ = spy "buffy" buffy
              unmap debugOutputBuffer
            tnx <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
            cfx <- Ref.read currentFrame
            avgTime <- timeDeltaAverager (tnx - tn)
            avgFrame <- frameDeltaAverager (toNumber (cfx - cf))
            pushFrameInfo { avgTime, avgFrame }
    let
      render = unit # fix \f _ -> do
        cw <- clientWidth (toElement canvas)
        ch <- clientHeight (toElement canvas)
        setWidth (floor cw) canvas
        setHeight (floor ch) canvas
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