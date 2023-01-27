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

contrastThreshold = 0.35 :: Number

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

arrLength = 64 :: Int

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
const fuzz_fac = 0.75;
const half_fuzz_fac = fuzz_fac / 2.0;
fn fuzzMe(i: u32, n: u32, d: u32) -> f32
{
    var fi = f32(i);
    // we stagger things to take advantage of the fact that the first pass has been done already
    var fnn = f32((2*n+1));
    var fd = f32((2*d));
    return fi + ((fnn / fd) * fuzz_fac) - half_fuzz_fac;
}
fn noFuzz(i: u32, n: u32, d: u32) -> f32
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

calcColors :: String
calcColors =
  """
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
"""

type MainComputeInfo =
  ( kX :: Int
  , kY :: Int
  , arrL :: Int
  , sqS :: Int
  , sqA :: Int
  , aaP :: Int
  , usesContrast :: Boolean
  )

mainComputeBody :: { | MainComputeInfo } -> String
mainComputeBody { kX, kY, arrL, sqS, sqA, aaP, usesContrast } = fold
  [ """
// main

var<workgroup> colors: array<vec3<f32>, """
  , show (sqA * aaP)
  , """>;

@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read> sphere_info : array<f32>;
@group(0) @binding(2) var<storage, read> bvh_nodes : array<bvh_node>;
@group(1) @binding(0) var<storage, read_write> color_arary : array<u32>;
"""
  , if usesContrast then
      """
@group(2) @binding(0) var<storage, read> contrast_ixs : array<u32>;
"""
    else ""
  , """
@compute @workgroup_size("""
  , show kX
  , """, """
  , show kY
  , """, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>, @builtin(local_invocation_id) local_id : vec3<u32>) {
  ////////////////////////////////////////
  ////////////////////////////////////////
  var px: f32;
  var py: f32;
  var ray: ray;
  var hit: f32;
  var color: vec3<f32>;
  var sphere_ix: u32;
  var bvh_read_playhead: u32;
  var bvh_write_playhead: u32;
  var sphere_read_playhead: u32;
  var sphere_write_playhead: u32;
  var bvh_ixs: array<u32, """
  , show (arrL)
  , """>;
  var sphere_ixs: array<u32, """
  , show (arrL)
  , """>;

  ////////////////////////////////////////
  ////////////////////////////////////////
  var aa_total = f32("""
  , show aaP
  , """);
  var cwch = rendering_info.real_canvas_width * rendering_info.canvas_height;
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var ambitus_x = select(2.0 * aspect, 2.0, aspect < 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var last_node_ix = rendering_info.n_bvh_nodes - 1;
  var locy = local_id.y * """
  , show kX
  , """;
  var locxy = locy + local_id.x;
  var xk = locxy % """
  , show sqS
  , """;
  var yk = (locxy / """
  , show sqS
  , """) % """
  , show sqS
  , """;
  var aa_pass = locxy / """
  , show sqA
  , """;
  var t_min = 0.0001;
  var t_max = 10000.f;
  var my_color = vec3(0.0,0.0,0.0);
  var real_x = """
  , if usesContrast then fold [ "(contrast_ixs[global_id.x / ", show $ kX * kY, "] & 0xffff) + xk;" ]
    else fold
      [ """(global_id.x / """
      , show kX
      , """ ) * """
      , show sqS
      , """ + xk;"""
      ]
  , """
  var real_y = """
  , if usesContrast then fold [ "(contrast_ixs[global_id.x / ", show $ kX * kY, "] >> 16) + yk;" ]
    else fold
      [ """(global_id.y / """
      , show kY
      , """) * """
      , show sqS
      , """ + yk;"""
      ]
  , """

  ////////////////////////////////////////
  var last_node = bvh_nodes[last_node_ix];
  px = """
  , if aaP == 1 then "noFuzz" else "fuzzMe"
  , """(real_x,aa_pass,"""
  , show aaP
  , """) / f32(rendering_info.real_canvas_width);
  py = 1. - ("""
  , if aaP == 1 then "noFuzz" else "fuzzMe"
  , """(real_y,aa_pass,"""
  , show aaP
  , """) / f32(rendering_info.canvas_height));
  ray.origin = origin;
  ray.direction = lower_left_corner + vec3(px * ambitus_x, py * ambitus_y, 0.0);
  hit = 1000.f;
  sphere_ix = 0u;
  if (last_node.is_sphere == 1u) {
    sphere_ixs[0] = last_node_ix;
    sphere_write_playhead = 1u;
  } else {
    bvh_ixs[0] = last_node_ix;
    bvh_write_playhead = 1u;
  }
  sphere_read_playhead = 0u;
  bvh_read_playhead = 0u;
  var currentBVH = bvh_write_playhead - bvh_read_playhead;
  var currentSphere = sphere_write_playhead - sphere_read_playhead;
  var is_done = false;
  loop {
    if (is_done) {
      break;
    }
    var starting_bvh_write_playhead = bvh_write_playhead;
    var starting_sphere_write_playhead = sphere_write_playhead;
    // bvh
    if (starting_bvh_write_playhead > bvh_read_playhead) {
      var bloc = bvh_ixs[bvh_read_playhead % """
  , show (arrL)
  , """];
      var node = bvh_nodes[bloc];
      var bbox: aabb;
      bvh_node_bounding_box(&node, &bbox);
      var was_aabb_hit = aabb_hit(&bbox, &ray, t_min, t_max);
      /////
      if (was_aabb_hit) {
        if (bvh_nodes[node.left].is_sphere == 1u) {
          sphere_ixs[sphere_write_playhead % """
  , show (arrL)
  , """] = node.left;
          sphere_write_playhead++;
        } else {
          bvh_ixs[(bvh_write_playhead) % """
  , show (arrL)
  , """] = node.left;
          bvh_write_playhead++;
        }
        if (bvh_nodes[node.right].is_sphere == 1u) {
          sphere_ixs[(sphere_write_playhead) % """
  , show (arrL)
  , """] = node.right;
          sphere_write_playhead++;
        } else {
          bvh_ixs[(bvh_write_playhead) % """
  , show (arrL)
  , """] = node.right;
          bvh_write_playhead++;
        }
      }
      bvh_read_playhead += 1;
    }
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    if (starting_sphere_write_playhead > sphere_read_playhead) {
      // sphere
      var sloc = sphere_ixs[(sphere_read_playhead) % """
  , show (arrL)
  , """];
      var node = bvh_nodes[sloc];
      var hit_t: f32;
      var sphere_test = node.left * 4u;
      var sphere_hit = hit_sphere(
        sphere_info[sphere_test],
        sphere_info[sphere_test+1],
        sphere_info[sphere_test+2],
        sphere_info[sphere_test+3],
        &ray,
        t_min,
        t_max,
        &hit_t);
      var i_plus_1 = node.left + 1u;
      var old_t = hit;
      var new_t = select(old_t, select(old_t, hit_t, hit_t < old_t), sphere_hit);
      hit = new_t;
      ///
      var old_ix = sphere_ix;
      sphere_ix =
        select(old_ix, select(old_ix, i_plus_1, new_t != old_t), sphere_hit);
      sphere_read_playhead += 1;
    }
    is_done = (sphere_write_playhead == sphere_read_playhead) && (bvh_write_playhead == bvh_read_playhead);
  }

  var was_i_hit = sphere_ix > 0u;
  if (!was_i_hit) {
    my_color = (sky_color(&ray));
  } else {
    sphere_ix = sphere_ix - 1;
    var sphere_offset = sphere_ix * 4;
    var norm_t = hit;
    var rec: hit_record;
    _ = make_hit_rec(sphere_info[sphere_offset], sphere_info[sphere_offset + 1], sphere_info[sphere_offset + 2], sphere_info[sphere_offset + 3], norm_t, &ray, &rec);
    my_color = (hit_color(&ray, &rec));
  }
  colors[xk + (yk * """
  , show sqS
  , """) + (aa_pass * """
  , show sqA
  , """) ] = my_color;
  // this last bit only needs to be done for the square grid
"""
  , if usesContrast then fold
      [ """
  workgroupBarrier();
  if (locxy >= """
      , show sqA
      , """) { return; }
  var overshot_idx = real_x + (real_y * rendering_info.overshot_canvas_width);
  var proto_color = unpack4x8unorm(color_arary[overshot_idx]);
  var cur_color = vec3(proto_color.z, proto_color.y, proto_color.x);
  var cur_color2 = vec3(proto_color.z, proto_color.y, proto_color.x) / f32("""
      , show $ aaP + 1
      , """);
  for (var iii = 0u; iii < """
      , show aaP
      , """; iii++) {
    cur_color2 += (colors[locxy + (iii * """
      , show sqA
      , """)] / f32("""
      , show $ aaP + 1
      , """));
  }
  color_arary[overshot_idx] = pack4x8unorm(vec4(cur_color2.b, cur_color2.g, cur_color2.r, 1.f));
"""
      ]
    else
      """
  var overshot_idx = real_x + (real_y * rendering_info.overshot_canvas_width);
  color_arary[overshot_idx] = pack4x8unorm(vec4(my_color.b, my_color.g, my_color.r, 1.f));
"""
  , """}
"""
  ]

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
    randos <- liftEffect $ sequence $ replicate 512 $ Sphere <$> ({ cx: _, cy: _, cz: _, radius: 0.125 } <$> (random <#> \n -> n * 16.0 - 8.0) <*> (random <#> \n -> n * 3.0 + 0.25) <*> (random <#> \n -> n * 16.0 - 8.0))
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
    contrastIxs <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    contrastCounter <- liftEffect $ createBuffer device $ x
      { size: 12
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }

    let
      zeroOutBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> color_arary : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  color_arary[global_id.y * rendering_info.overshot_canvas_width + global_id.x] = 0u;
}"""
              ]
        }
    zeroOutBufferModule <- liftEffect $ createShaderModule device zeroOutBufferDesc
    let
      (zeroOutBufferStage :: GPUProgrammableStage) = x
        { "module": zeroOutBufferModule
        , entryPoint: "main"
        }
    let
      resetContrastBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> contrast_array : array<u32>;
@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  contrast_array[0u] = 0u;
  contrast_array[1u] = 1u;
  contrast_array[2u] = 1u;
}"""
              ]
        }
    resetContrastBufferModule <- liftEffect $ createShaderModule device resetContrastBufferDesc
    let
      (resetContrastBufferStage :: GPUProgrammableStage) = x
        { "module": resetContrastBufferModule
        , entryPoint: "main"
        }
    let
      contrastDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> color_array : array<u32>;
@group(2) @binding(0) var<storage, read_write> contrast_ixs : array<u32>;
@group(2) @binding(1) var<storage, read_write> contrast_counter : atomic<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var i_x = global_id.x * 4;
  var i_y = global_id.y * 4;
  if (i_x >= rendering_info.real_canvas_width || i_y >= rendering_info.canvas_height) {
    return;
  }
  var c0ix =   i_x + ((i_y + 0) * rendering_info.overshot_canvas_width);
  var c1ix = 1 + c0ix;
  var c2ix = 2 + c0ix;
  var c3ix = 3 + c0ix;
  var c4ix =   i_x + ((i_y + 1) * rendering_info.overshot_canvas_width);
  var c5ix = 1 + c4ix;
  var c6ix = 2 + c4ix;
  var c7ix = 3 + c4ix;
  var c8ix =   i_x + ((i_y + 2) * rendering_info.overshot_canvas_width);
  var c9ix = 1 + c8ix;
  var c10ix = 2 + c8ix;
  var c11ix = 3 + c8ix;
  var c12ix =   i_x + ((i_y + 3) * rendering_info.overshot_canvas_width);
  var c13ix = 1 + c12ix;
  var c14ix = 2 + c12ix;
  var c15ix = 3 + c12ix;
  var c0 = unpack4x8unorm(color_array[c0ix]);
  var c1 = unpack4x8unorm(color_array[c1ix]);
  var c2 = unpack4x8unorm(color_array[c2ix]);
  var c3 = unpack4x8unorm(color_array[c3ix]);
  var c4 = unpack4x8unorm(color_array[c4ix]);
  var c5 = unpack4x8unorm(color_array[c5ix]);
  var c6 = unpack4x8unorm(color_array[c6ix]);
  var c7 = unpack4x8unorm(color_array[c7ix]);
  var c8 = unpack4x8unorm(color_array[c8ix]);
  var c9 = unpack4x8unorm(color_array[c9ix]);
  var c10 = unpack4x8unorm(color_array[c10ix]);
  var c11 = unpack4x8unorm(color_array[c11ix]);
  var c12 = unpack4x8unorm(color_array[c12ix]);
  var c13 = unpack4x8unorm(color_array[c13ix]);
  var c14 = unpack4x8unorm(color_array[c14ix]);
  var c15 = unpack4x8unorm(color_array[c15ix]);
  var mn = min(c0, min(c1, min(c2, min(c3, min(c4, min(c5, min(c6, min(c7, min(c8, min(c9, min(c10, min(c11, min(c12, min(c13, min(c14, c15)))))))))))))));
  var mx = max(c0, max(c1, max(c2, max(c3, max(c4, max(c5, max(c6, max(c7, max(c8, max(c9, max(c10, max(c11, max(c12, max(c13, max(c14, c15)))))))))))))));
  if (distance(vec3(mx.r,mx.g,mx.b),vec3(mn.r,mn.g,mn.b)) > """
              , show contrastThreshold
              , """) {
    var ix = atomicAdd(&contrast_counter, 1u);
    contrast_ixs[ix] = i_x | (i_y << 16);
  }
}"""
              ]
        }
    contrastModule <- liftEffect $ createShaderModule device contrastDesc
    let
      (contrastStage :: GPUProgrammableStage) = x
        { "module": contrastModule
        , entryPoint: "main"
        }
    let
      mainComputeDesc = x
        { code: fold
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
            , calcColors
            , mainComputeBody
                { kX: kernelX
                , kY: kernelY
                , arrL: arrLength
                , sqS: squareSide
                , sqA: squareArea
                , aaP: 1
                , usesContrast: false
                }
            ]
        }
    mainComputeModule <- liftEffect $ createShaderModule device mainComputeDesc
    let
      (mainComputeStage :: GPUProgrammableStage) = x
        { "module": mainComputeModule
        , entryPoint: "main"
        }
    let
      antiAliasDesc = x
        { code: fold
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
            , calcColors
            , mainComputeBody
                { kX: 64
                , kY: 1
                , arrL: arrLength
                , sqS: 4
                , sqA: 16
                , aaP: 4
                , usesContrast: true
                }
            ]
        }
    antiAliasModule <- liftEffect $ createShaderModule device antiAliasDesc
    let
      (antiAliasStage :: GPUProgrammableStage) = x
        { "module": antiAliasModule
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
          , label: "readerBindGroupLayout"
          }
    rBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "rBindGroupLayout"
          }
    wBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "wBindGroupLayout"
          }
    contrastBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "contrastBindGroupLayout"
          } -- for when we are reading from a context and writing to a buffer
    readOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout ]

      , label: "readOPipelineLayout"
      }
    contrastPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout, contrastBindGroupLayout ]
      , label: "contrastPipelineLayout"
      }
    antiAliasPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout, rBindGroupLayout ]
      , label: "contrastPipelineLayout"
      }
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
      , label: "readerBindGroup"
      }
    wCanvasBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      , label: "wCanvasBindGroup"
      }
    wContrastCounterBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: contrastCounter } :: GPUBufferBinding)
          ]
      , label: "wContrastCounterBindGroup"
      }
    contrastBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: contrastBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: contrastIxs } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: contrastCounter } :: GPUBufferBinding)
          ]
      , label: "contrastBindGroup"
      }
    wContrastIxsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: contrastIxs } :: GPUBufferBinding)
          ]
      , label: "wContrastIxsBindGroup"
      }
    rContrastIxsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: contrastIxs } :: GPUBufferBinding)
          ]
      , label: "rContrastIxsBindGroup"
      }
    zeroOutBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: zeroOutBufferStage
      , label: "zeroOutBufferPipeline"
      }
    resetContrastBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: resetContrastBufferStage
      , label: "resetContrastBufferPipeline"
      }
    mainComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: mainComputeStage
      , label: "mainComputePipeline"
      }
    contrastPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: contrastPipelineLayout
      , compute: contrastStage
      , label: "contrastPipeline"
      }
    antiAliasPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: antiAliasPipelineLayout
      , compute: antiAliasStage
      , label: "antiAliasPipeline"
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
        let contrastX = ceil (toNumber canvasWidth / toNumber (16 * 4)) -- 16 is the workgroup x size, 4 is the kernel size for each thread
        let contrastY = ceil (toNumber canvasHeight / toNumber (16 * 4)) -- 16 is the workgroup x size, 4 is the kernel size for each thread
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
        GPUComputePassEncoder.setPipeline computePassEncoder zeroOutBufferPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wCanvasBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupOvershootX workgroupOvershootY 1
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wContrastIxsBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wContrastCounterBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder resetContrastBufferPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
        ------------------------
        -- do bvh, color mapping and anti-aliasing
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wCanvasBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder
          mainComputePipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -----------------------------------
        -----------------------------------
        GPUComputePassEncoder.setBindGroup computePassEncoder 2
          contrastBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder
          contrastPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder contrastX contrastY 1
        -----------------------------------
        -----------------------------------
        GPUComputePassEncoder.setBindGroup computePassEncoder 2
          rContrastIxsBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder
          antiAliasPipeline
        GPUComputePassEncoder.dispatchWorkgroupsIndirect computePassEncoder contrastCounter 0
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