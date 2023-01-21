module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Gen (elements)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.Array (intercalate, length, replicate, (..))
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
import Web.GPU.GPUCommandEncoder (beginComputePass, copyBufferToBuffer, copyBufferToTexture, finish)
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

readWriteAtLevel :: String
readWriteAtLevel =
  """
fn read_at_level(bitmask: u32, level: u32, lr: bool) -> bool {
  var spot = level * 2u + select(1u, 0u, lr);
  return (bitmask & (1u << spot)) != 0;
}
fn read_at_left(bitmask: u32, level: u32) -> bool {
  return read_at_level(bitmask, level, true);
}
fn read_at_right(bitmask: u32, level: u32) -> bool {
  return read_at_level(bitmask, level, false);
}
fn write_at_level(bitmask: u32, level: u32, lr: bool, val: bool) -> u32 {
  var spot = level * 2u + select(1u, 0u, lr);
  return select(bitmask & ~ (1u << spot), bitmask | (1u << spot), val);
}
fn write_at_left(bitmask: u32, level: u32, val: bool) -> u32 {
  return write_at_level(bitmask, level, true, val);
}
fn write_at_right(bitmask: u32, level: u32, val: bool) -> u32 {
  return write_at_level(bitmask, level, false, val);
}
fn read_stack_at_bitmask(bitmask: u32) -> u32 {
  return bitmask >> 24;
}
fn write_stack_at_bitmask(bitmask: u32, stack: u32) -> u32 {
  return (bitmask & 0xFFFFFF) | (stack << 24);
}
fn read_x_at_bitmask(xyz: u32) -> u32 {
  return xyz & 0x3fff; // range [0,14)
}
fn read_y_at_bitmask(xyz: u32) -> u32 {
  return (xyz >> 14u) & 0x3fff; // range [14,28)
}
fn read_z_at_bitmask(xyz: u32) -> u32 {
  return xyz >> 28u; // range [28,32)
}
fn write_x_at_bitmask(xyz: u32, x: u32) -> u32 {
  return (xyz & 0xFFFFC000) | x;
}
fn write_y_at_bitmask(xyz: u32, y: u32) -> u32 {
  return (xyz & 0xFFFC3FFF) | (y << 14u);
}
fn write_z_at_bitmask(xyz: u32, z: u32) -> u32 {
  return (xyz & 0x3FFFFFFF) | (z << 28u);
}
  """

getTAndIx :: String
getTAndIx =
  """
struct t_and_ix {
  t: f32,
  ix: u32
}

fn u32_to_t_and_ix(i: u32, p: ptr<function, t_and_ix>) -> bool {
  var out = unpack2x16float(i);
  (*p).t = out.y;
  (*p).ix = u32(out.x);
  return true;
}

fn t_and_ix_to_u32(p: ptr<function, t_and_ix>) -> u32 {
  // offset by 0.1 to prevent rounding errors when stored
  return pack2x16float(vec2(f32((*p).ix)+0.1, (*p).t));
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

fn bvh_node_bounding_box(node:bvh_node, box: ptr<function,aabb>) -> bool
{
  (*box).aabb_min = vec3<f32>((node).aabb_min_x, (node).aabb_min_y, (node).aabb_min_z);
  (*box).aabb_max = vec3<f32>((node).aabb_max_x, (node).aabb_max_y, (node).aabb_max_z);
  return true;
}

"""

newtype HitBVHInfo = HitBVHInfo
  { hitTName :: String
  , nodesName :: String
  , rName :: String
  , spheresName :: String
  , startNodeIx :: String
  , tMaxName :: String
  , tMinName :: String
  }

hitBVHNode :: HitBVHInfo -> String
hitBVHNode (HitBVHInfo { startNodeIx, nodesName, spheresName, rName, tMinName, tMaxName, hitTName }) = intercalate "\n"
  [ "  var bvh__namespaced__node_ix = " <> startNodeIx <> ";"
  , "  let bvh__namespaced__nodes = &" <> nodesName <> ";"
  , "  let bvh__namespaced__spheres = &" <> spheresName <> ";"
  , "  let bvh__namespaced__r = &" <> rName <> ";"
  , "  var bvh__namespaced__t_min = " <> tMinName <> ";"
  , "  var bvh__namespaced__t_max = " <> tMaxName <> ";"
  , "  let bvh__namespaced_t = &" <> hitTName <> ";"
  , """

  // needed in array
  var bvh__namespaced__t_sphere = pack2x16float(vec2(0.0, 10000.f));
  // needed in array
  var bvh__namespaced__bitmask = 0u;
  // bvh__namespaced__node_ix needed in array

  var bvh__namespaced__tmp_box: aabb;

  var my_id = (rendering_info.canvas_height * rendering_info.real_canvas_width) - 444u;
  var dbg_cond = select(false, true, dbg_id == my_id);

  //////////////
  // as branching causes bugs in windows (and perhaps other platforms), we run this entirely on select statements
  loop {
    var t_ix: t_and_ix;
    u32_to_t_and_ix(bvh__namespaced__t_sphere, &t_ix);
    var bvh__namespaced__stack = read_stack_at_bitmask(bvh__namespaced__bitmask);
    ///
    var sphere_ix = ((*bvh__namespaced__nodes)[bvh__namespaced__node_ix]).left * 4u;
    var sphere_hit = hit_sphere(
      (*bvh__namespaced__spheres)[sphere_ix],
      (*bvh__namespaced__spheres)[sphere_ix+1],
      (*bvh__namespaced__spheres)[sphere_ix+2],
      (*bvh__namespaced__spheres)[sphere_ix+3],
      bvh__namespaced__r,
      bvh__namespaced__t_min,
      bvh__namespaced__t_max,
      bvh__namespaced_t);
    ///
    bvh_node_bounding_box((*bvh__namespaced__nodes)[bvh__namespaced__node_ix], &bvh__namespaced__tmp_box);
    ///
    var was_aabb_hit = aabb_hit(&bvh__namespaced__tmp_box, bvh__namespaced__r, bvh__namespaced__t_min, bvh__namespaced__t_max);
    var obj_is_sphere = ((*bvh__namespaced__nodes)[bvh__namespaced__node_ix]).is_sphere == 1u;
    var not_left = !read_at_left(bvh__namespaced__bitmask, bvh__namespaced__stack);
    var loop_completed = read_at_left(bvh__namespaced__bitmask, bvh__namespaced__stack) && read_at_right(bvh__namespaced__bitmask, bvh__namespaced__stack);
    var stack_is_0 = bvh__namespaced__stack == 0u;
    var up_1_on_right = read_at_right(bvh__namespaced__bitmask, bvh__namespaced__stack - 1);
    var up_1_on_left = !up_1_on_right;
    ///
    var i_plus_1 = ((*bvh__namespaced__nodes)[bvh__namespaced__node_ix]).left + 1u;
    ///////////////
    ///////////////
    ///////////////
    ///////////////
    var old_t = t_ix.t;
    t_ix.t =
      select(
        t_ix.t,
        select(t_ix.t, select(t_ix.t, *bvh__namespaced_t, *bvh__namespaced_t < t_ix.t), sphere_hit),
        obj_is_sphere
      );
    ///
    t_ix.ix =
      select(
        t_ix.ix,
        select(t_ix.ix, select(t_ix.ix, i_plus_1, old_t != t_ix.t), sphere_hit),
        obj_is_sphere
      );
    /// first draft done
    var parent_node = ((*bvh__namespaced__nodes)[bvh__namespaced__node_ix]).parent;
    bvh__namespaced__node_ix =
      select(
        select(
          select(
            // we're on the right branch, so focus on right
            ((*bvh__namespaced__nodes)[bvh__namespaced__node_ix]).right,
            // when completed focus on parent
            parent_node,
            loop_completed
          ),
          // if the box was hit, focus on the left node
          // otherwise focus on the parent
          select(
            parent_node,
            ((*bvh__namespaced__nodes)[bvh__namespaced__node_ix]).left,
            was_aabb_hit),
          not_left
        ),
        parent_node,
        obj_is_sphere
      );
    /// first draft done
    // don't worry about underflow here as 0 checks are based on old value
    // if we are ever below 0, one of the break checks below should trigger
    var old_bvh__namespaced__stack = bvh__namespaced__stack;
    bvh__namespaced__stack =
      select(
        select(
          select(
            // always increment on right as we've tested already
            old_bvh__namespaced__stack + 1,
            // always go up a level if we've completed a loop
            old_bvh__namespaced__stack - 1,
            loop_completed
          ),
          // increment if we have a hit, decrement if we don't
          select(old_bvh__namespaced__stack - 1, old_bvh__namespaced__stack + 1, was_aabb_hit),
          not_left
        ),
        // always decrease when hitting sphere
        old_bvh__namespaced__stack - 1,
        obj_is_sphere
      );
    /////////// first draft completed
    // always anchor left/right on old_bvh__namespaced__stack
    bvh__namespaced__bitmask = write_at_left(bvh__namespaced__bitmask, old_bvh__namespaced__stack, select(
        select(
          select(
            true,
            false,
            loop_completed
          ),
          // if we haven't started the left yet, we only enter if aabb has been hit
          was_aabb_hit,
          not_left
        ),
        false,
        obj_is_sphere
      ));
    /////// first draft completed
    // always anchor left/right on old_bvh__namespaced__stack
    bvh__namespaced__bitmask = write_at_right(bvh__namespaced__bitmask, old_bvh__namespaced__stack,
      select(
        select(
          select(
            true,
            false,
            loop_completed
          ),
          false,
          not_left
        ),
        false,
        obj_is_sphere
      ));
    bvh__namespaced__bitmask = write_stack_at_bitmask(bvh__namespaced__bitmask, bvh__namespaced__stack);
    ///// first draft done
    *bvh__namespaced_t = t_ix.t;
    bvh__namespaced__t_sphere = t_and_ix_to_u32(&t_ix);
    if (stack_is_0 && obj_is_sphere) { break; }
    if (stack_is_0 && !was_aabb_hit) { break; }
    if (stack_is_0 && loop_completed) { break; }
    //break; // debug for testing
  }
"""
  ]

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
const fuzz_fac = 0.5;
const half_fuzz_fac = fuzz_fac / 2.0;
fn fuzz2(i: u32, n: u32, d: u32) -> f32
{
    var fi = f32(i);
    return fi; // + (fuzz_fac * pow(f32(n) / f32(d), 0.5) - half_fuzz_fac);
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
gpuMe showErrorMessage pushFrameInfo canvas = launchAff_ $ delay (Milliseconds 20.0) *> liftEffect do
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
    randos <- liftEffect $ sequence $ replicate 512 $ Sphere <$> ({ cx: _, cy: 0.25, cz: _, radius: 0.125 } <$> (random <#> \n -> n * 4.0 - 2.0) <*> (random <#> \n -> n * 4.0 - 2.0))
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
    rawColorBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    hitsBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    currentNodeBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    currentBitmaskBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    let
      zeroOutBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> result_array : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // assume that x is always w, y is always h
  // but z is variable
  result_array[global_id.z * rendering_info.real_canvas_width * rendering_info.canvas_height + global_id.y * rendering_info.real_canvas_width + global_id.x] = 0u;
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
      resetHitsBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> result_array : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // assume that x is always w, y is always h
  // but z is variable
  result_array[global_id.z * rendering_info.real_canvas_width * rendering_info.canvas_height + global_id.y * rendering_info.real_canvas_width + global_id.x] = pack2x16float(vec2(0.f, 10000.f));
}"""
              ]
        }
    resetHitsBufferModule <- liftEffect $ createShaderModule device resetHitsBufferDesc
    let
      (resetHitsBufferStage :: GPUProgrammableStage) = x
        { "module": resetHitsBufferModule
        , entryPoint: "main"
        }
    let
      resetNodesBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> result_array : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // assume that x is always w, y is always h
  // but z is variable
  result_array[global_id.z * rendering_info.real_canvas_width * rendering_info.canvas_height + global_id.y * rendering_info.real_canvas_width + global_id.x] = rendering_info.n_spheres - 1; 
}"""
              ]
        }
    resetNodesBufferModule <- liftEffect $ createShaderModule device resetNodesBufferDesc
    let
      (resetNodesBufferStage :: GPUProgrammableStage) = x
        { "module": resetNodesBufferModule
        , entryPoint: "main"
        }
    let
      hitDesc = x
        { code: spy "hitDesc" $ intercalate "\n"
            [ lerp
            , lerpv
            , inputData
            , ray
            , antiAliasFuzzing
            , pointAtParameter
            , hitSphere
            , aabb
            , getTAndIx
            , readWriteAtLevel
            , bvhNode
            , sphereBoundingBox
            , usefulConsts
            , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read> sphere_info : array<f32>;
@group(0) @binding(2) var<storage, read> bvh_info : array<bvh_node>;
@group(1) @binding(0) var<storage, read_write> result_array : array<u32>;
@group(1) @binding(1) var<storage, read_write> current_node_array : array<u32>;
@group(1) @binding(2) var<storage, read_write> current_bitmask_array : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width  || global_id.y >= rendering_info.canvas_height || global_id.z >  rendering_info.anti_alias_passes) {
    return;
  }
  var dbg_id = global_id.y * rendering_info.real_canvas_width + global_id.x;
  var cwch = rendering_info.real_canvas_width * rendering_info.canvas_height;
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var ambitus_x = select(2.0 * aspect, 2.0, aspect < 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var alias_pass = global_id.z;
  var p_x = fuzz2(global_id.x, alias_pass, rendering_info.anti_alias_passes) / f32(rendering_info.real_canvas_width);
  var p_y = 1. - fuzz2(global_id.y, alias_pass, rendering_info.anti_alias_passes) / f32(rendering_info.canvas_height);
  var r: ray;
  r.origin = origin;
  r.direction = lower_left_corner + vec3(p_x * ambitus_x, p_y * ambitus_y, 0.0);
  var hit_t: f32 = 0.42424242424242;
  """
            , hitBVHNode
                ( HitBVHInfo
                    { startNodeIx: "rendering_info.n_bvh_nodes - 1"
                    , nodesName: "bvh_info"
                    , spheresName: "sphere_info"
                    , rName: "r"
                    , tMinName: "0.0001"
                    , tMaxName: "1000.f"
                    , hitTName: "hit_t"
                    }
                )
            , """ 
  var idx = (global_id.y * rendering_info.real_canvas_width + global_id.x) + (cwch * global_id.z);
  result_array[idx] = bvh__namespaced__t_sphere;
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
      colorFillDesc = x
        { code:
            intercalate "\n"
              [ lerp
              , lerpv
              , inputData
              , ray
              , antiAliasFuzzing
              , pointAtParameter
              , hitRecord
              , getTAndIx
              , makeHitRec
              , usefulConsts
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
@group(0) @binding(1) var<storage, read> sphere_info : array<f32>;
@group(1) @binding(0) var<storage, read> hit_info : array<u32>;
@group(2) @binding(0) var<storage, read_write> result_array : array<atomic<u32>>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width  || global_id.y >= rendering_info.canvas_height || global_id.z >= rendering_info.anti_alias_passes) {
    return;
  }
  var cwch = rendering_info.real_canvas_width * rendering_info.canvas_height;
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var ambitus_x = select(2.0 * aspect, 2.0, aspect < 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var alias_pass = global_id.z;
  var p_x = fuzz2(global_id.x, alias_pass, rendering_info.anti_alias_passes) / f32(rendering_info.real_canvas_width);
  var p_y = 1. - fuzz2(global_id.y, alias_pass, rendering_info.anti_alias_passes) / f32(rendering_info.canvas_height);
  var r: ray;
  r.origin = origin;
  r.direction = lower_left_corner + vec3(p_x * ambitus_x, p_y * ambitus_y, 0.0);
  var hit_idx = (global_id.y * rendering_info.real_canvas_width + global_id.x) + (cwch * global_id.z);
  var t_ix: t_and_ix;
  u32_to_t_and_ix(hit_info[hit_idx], &t_ix);
  var was_i_hit = t_ix.ix > 0;
  var my_color = vec3(0.0,0.0,0.0);
  if (!was_i_hit) {
    my_color = sky_color(&r);
  } else {
    var sphere_idx = t_ix.ix - 1;
    var sphere_offset = sphere_idx * 4;
    var norm_t = t_ix.t;
    var rec: hit_record;
    _ = make_hit_rec(sphere_info[sphere_offset], sphere_info[sphere_offset + 1], sphere_info[sphere_offset + 2], sphere_info[sphere_offset + 3], norm_t, &r, &rec);
    my_color = hit_color(&r, &rec);
  }
  var idx = (global_id.y * rendering_info.real_canvas_width + global_id.x) * 3;
  _ = atomicAdd(&result_array[idx], u32(my_color.b * color_mult));
  _ = atomicAdd(&result_array[idx + 1],  u32(my_color.g * color_mult));
  _ = atomicAdd(&result_array[idx + 2], u32(my_color.r * color_mult));
}
"""
              ]
        }
    colorFillModule <- liftEffect $ createShaderModule device colorFillDesc
    let
      (colorFillStage :: GPUProgrammableStage) = x
        { "module": colorFillModule
        , entryPoint: "main"
        }
    let
      antiAliasDesc = x
        { code:
            intercalate "\n"
              [ lerp
              , lerpv
              , inputData
              , ray
              , antiAliasFuzzing
              , pointAtParameter
              , hitRecord
              , makeHitRec
              , usefulConsts
              , """
// average the anti-aliasing
fn cc(c: u32, aap: u32) -> f32 {
  return max(0.0, min(1.0, f32(c) / f32(color_mult * aap)));
}
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read> sphere_info : array<f32>;
@group(1) @binding(0) var<storage, read> color_info : array<u32>;
@group(2) @binding(0) var<storage, read_write> result_array : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  if (global_id.x >= rendering_info.real_canvas_width  || global_id.y >= rendering_info.canvas_height) {
    return;
  }
  var idx = (global_id.y * rendering_info.real_canvas_width + global_id.x) * 3;
  var overshot_idx = global_id.x + (global_id.y * rendering_info.overshot_canvas_width);
  result_array[overshot_idx] = pack4x8unorm(vec4(cc(color_info[idx], rendering_info.anti_alias_passes), cc(color_info[idx + 1], rendering_info.anti_alias_passes), cc(color_info[idx + 2], rendering_info.anti_alias_passes), 1.f));
}
"""
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
    wBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    hitsBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              (0 .. 2) <#> \i ->
                gpuBindGroupLayoutEntry i GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
          }
    -- for when we are reading from a context and writing to a buffer
    readOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout ] }
    hitsPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, hitsBindGroupLayout ] }
    -- for when we are reading from a context, taking an input, and transforming it
    -- to an output
    readIOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, rBindGroupLayout, wBindGroupLayout ] }
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
    clearHitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: hitsBuffer } :: GPUBufferBinding)
          ]
      }
    hitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: hitsBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: hitsBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: currentNodeBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: currentBitmaskBuffer } :: GPUBufferBinding)
          ]
      }
    rHitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: hitsBuffer } :: GPUBufferBinding)
          ]
      }
    wColorsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rawColorBuffer } :: GPUBufferBinding)
          ]
      }
    rColorsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rawColorBuffer } :: GPUBufferBinding)
          ]
      }
    wCanvasBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      }
    zeroOutBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: zeroOutBufferStage
      }
    hitComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: hitsPipelineLayout
      , compute: hitStage
      }
    colorFillComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readIOPipelineLayout
      , compute: colorFillStage
      }
    antiAliasComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readIOPipelineLayout
      , compute: antiAliasStage
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
        let antiAliasPasses = 1 -- min 16 $ floor (toNumber maxStorageBufferBindingSize / (toNumber (canvasWidth * canvasHeight * nSpheres * 4)))
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
        whole asBuffer >>= \(x :: Float32Array) -> void $ set x (Just 6) [ fromNumber' tn ]
        writeBuffer queue canvasInfoBuffer 0 (fromUint32Array cinfo)
        -- not necessary in the loop, but useful as a stress test for animating positions
        computePassEncoder <- beginComputePass commandEncoder (x {})
        -- set reader for all computations
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          readerBindGroup
        -- clear hits/spheres as they're subject to an atomic operation
        GPUComputePassEncoder.setPipeline computePassEncoder zeroOutBufferPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          clearHitsBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -- clear colors as they're subject to an atomic operation
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wColorsBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 3
        ------------------------
        let
          work n = do
            -- get hits
            GPUComputePassEncoder.setPipeline computePassEncoder
              hitComputePipeline
            let
              workwork m = do
                GPUComputePassEncoder.setBindGroup computePassEncoder 1
                  hitsBindGroup
                GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder (workgroupX / (n * m)) (workgroupY / (n * m)) antiAliasPasses
            foreachE (1 .. 1) workwork
            -- colorFill
            GPUComputePassEncoder.setBindGroup computePassEncoder 1
              rHitsBindGroup
            GPUComputePassEncoder.setBindGroup computePassEncoder 2
              wColorsBindGroup
            GPUComputePassEncoder.setPipeline computePassEncoder
              colorFillComputePipeline
            GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder (workgroupX / n) (workgroupY / n) antiAliasPasses
        foreachE (1 .. 1) work
        -- antiAlias
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          rColorsBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 2
          wCanvasBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder
          antiAliasComputePipeline
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
              bfr <- getMappedRange debugOutputBuffer
              buffy <- (Typed.whole bfr :: Effect Uint32Array) >>= Typed.toArray
              let _ = spy "buffy" buffy
              unmap debugOutputBuffer
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