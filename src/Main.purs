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

hitMain :: String
hitMain =
  """
const workgroup_y_size = 16u;
const dispatch_y_size = 4u;
const y_stride = workgroup_y_size * dispatch_y_size;
  // main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read> sphere_info : array<f32>;
@group(0) @binding(2) var<storage, read> bvh_info : array<bvh_node>;
@group(1) @binding(0) var<storage, read_write> bvh__namespaced__hit_sphere_min: array<u32>;
@group(1) @binding(1) var<storage, read_write> bvh__namespaced__node_ix: array<u32>;
@group(1) @binding(2) var<storage, read_write> bvh__namespaced__node_xycoord: array<u32>;
@group(1) @binding(3) var<storage, read_write> bvh__namespaced__branch_bitmask: array<u32>;
@group(2) @binding(0) var<storage, read_write> bvh__namespaced__workgroups: array<atomic<u32>>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {return;
  // for now, we don't use a guard as we have no way of knowing how many workgroups we have
  // make sure to copy this to a dedicated workgroup buffer before running again!
  // get the current node we are exploring
  var my_ix = (y_stride * rendering_info.anti_alias_passes) * global_id.x + (global_id.y * rendering_info.anti_alias_passes) + rendering_info.anti_alias_passes;
  var x_coord = read_x_at_bitmask(bvh__namespaced__node_xycoord[my_ix]);
  var y_coord = read_y_at_bitmask(bvh__namespaced__node_xycoord[my_ix]);
  var z_coord = read_z_at_bitmask(bvh__namespaced__node_xycoord[my_ix]);
  var t = 0.0;
  let bvh__namespaced_t = &t;
  var current_bvh_node_ix = bvh__namespaced__node_ix[my_ix];
  // create a struct to hold the min_t and sphere index
  var current_min_t_and_sphere: t_and_ix;
  // populate the struct with the current min_t and sphere index
  // importantly, here we _don't_ use `my_ix`
  // `my_ix` is a throwaway variable for computations
  // but `bvh__namespaced__hit_sphere_min` needs to track the actual x/y position
  // uh-oh, this won't work with anti-aliasing built in
  // anti-aliasing isn't guaranteed to have the same hits for every node
  // and this is particularly important for diffuse materials where the anti-alias passes
  // will bounce all over the place
  // so we need for the atomics below to account for this
  var cwch = rendering_info.real_canvas_width * rendering_info.canvas_height;
  u32_to_t_and_ix(bvh__namespaced__hit_sphere_min[(y_coord * rendering_info.real_canvas_width + x_coord) + (cwch * z_coord)], &current_min_t_and_sphere);
  ////////
  var aspect = f32(rendering_info.real_canvas_width) / f32(rendering_info.canvas_height);
  var ambitus_x = select(2.0 * aspect, 2.0, aspect < 1.0);
  var ambitus_y = select(2.0 * aspect, 2.0, aspect >= 1.0);
  var lower_left_corner = vec3(-ambitus_x / 2.0, -ambitus_y / 2.0, -1.0);
  var alias_pass = z_coord;
  var p_x = fuzz2(x_coord, alias_pass, rendering_info.anti_alias_passes) / f32(rendering_info.real_canvas_width);
  var p_y = 1. - fuzz2(y_coord, alias_pass, rendering_info.anti_alias_passes) / f32(rendering_info.canvas_height);
  // get the bitmask that stores LR traversal and current stack information
  var current_bvh_lr_bitmask = bvh__namespaced__branch_bitmask[my_ix];
  // create a struct to hold the min_t and sphere index
  // get a temporary box where we store the bounding box of the current node
  var bvh__namespaced__tmp_box: aabb;
  // get the bounding box of the curernt object
  bvh_node_bounding_box(bvh_info[current_bvh_node_ix], &bvh__namespaced__tmp_box);
  // get a ray pointing from this coordinate
  var bvh__namespaced__r: ray;
  bvh__namespaced__r.origin = origin;
  bvh__namespaced__r.direction = lower_left_corner + vec3(p_x * ambitus_x, p_y * ambitus_y, 0.0);

  ////////////////////////////
  /// Branches in GPUs are sort of fictions, meaning that you can't count on your computation
  /// being faster by branching.
  /// To that end, we do all of the computations, even if they are spurious, to get an upper bound.
  /// It's fine to attempt introducing branches later as an optimizing step, but it's much more important
  /// to get a dense pipeline with simple, uniform shaders rather than a sparse pipeline with complex shaders.
  /// tl;dr a gpu is not a cpu
  ////////////////////////////
  // get the index of the current sphere (there may not even be a sphere, but see the above note for why we do this)
  var sphere_ix = (bvh_info[current_bvh_node_ix]).left * 4u;
  // calculate if we hit the sphere at this index
  var bvh__namespaced__t_min = 0.0001f;
  var bvh__namespaced__t_max = 10000.f;
  var sphere_hit = hit_sphere(
    sphere_info[sphere_ix],
    sphere_info[sphere_ix+1],
    sphere_info[sphere_ix+2],
    sphere_info[sphere_ix+3],
    &bvh__namespaced__r,
    bvh__namespaced__t_min,
    bvh__namespaced__t_max,
    bvh__namespaced_t);
  // get the level in the stack we're currently at
  // a bvh algorithm proceeds in levels, so this shows how deep we are in the tree
  var current_bvh_stack = read_stack_at_bitmask(current_bvh_lr_bitmask);
  // are we focusing on the right of the tree
  var right_at_current_stack = read_at_right(current_bvh_lr_bitmask, current_bvh_stack);
  // are we focusing on the left of the tree
  var left_at_current_stack = read_at_left(current_bvh_lr_bitmask, current_bvh_stack);
  // are we focusing on the right of the tree at the previous level
  var right_at_prev_stack = read_at_right(current_bvh_lr_bitmask, current_bvh_stack - 1);
  // are we focusing on the left of the tree at the previous level
  var left_at_prev_stack = !right_at_prev_stack;
  // hit detection for the bounding box
  var was_aabb_hit = aabb_hit(&bvh__namespaced__tmp_box, &bvh__namespaced__r, bvh__namespaced__t_min, bvh__namespaced__t_max);
  // is this object a sphere
  var obj_is_sphere = (bvh_info[current_bvh_node_ix]).is_sphere == 1u;
  // have we started on this level yet
  var have_not_started_yet = !left_at_current_stack;
  // is the loop done at this level
  var loop_completed = left_at_current_stack && right_at_current_stack;
  // are we at the top-most level
  var stack_is_0 = current_bvh_stack == 0u;

  /// This `if`` is important because it gates the atomic operations
  /// Even though the GPU may run this code, it won't do the atomic writes in a conditional branch
  /// so it's still important to branch it.
  if (
       !((stack_is_0 && obj_is_sphere) ||
        (stack_is_0 && !was_aabb_hit) ||
        (stack_is_0 && loop_completed))
     ) {
    var i_plus_1 = (bvh_info[current_bvh_node_ix]).left + 1u;
    ///////////////
    ///////////////
    ///////////////
    ///////////////
    var new_hit_t =
      select(
        current_min_t_and_sphere.t,
        select(current_min_t_and_sphere.t, *bvh__namespaced_t, sphere_hit && *bvh__namespaced_t < current_min_t_and_sphere.t),
        obj_is_sphere
      );
    var new_sphere =
      select(
        current_min_t_and_sphere.ix,
        select(current_min_t_and_sphere.ix, i_plus_1, sphere_hit && *bvh__namespaced_t < current_min_t_and_sphere.t),
        obj_is_sphere
      );
    var new_node_x =
      select(
        select(
          select(
            // we're on the right branch, so focus on right
            (bvh_info[current_bvh_node_ix]).right,
            // when completed focus on parent
            (bvh_info[current_bvh_node_ix]).parent,
            loop_completed
          ),
          // if the box was hit, focus on the left node
          // otherwise focus on the parent
          select(
            (bvh_info[current_bvh_node_ix]).parent,
            (bvh_info[current_bvh_node_ix]).left,
            was_aabb_hit),
          have_not_started_yet
        ),
        (bvh_info[current_bvh_node_ix]).parent,
        obj_is_sphere
      );
    var new_bvh_stack =
      select(
        select(
          select(
            // always increment on right as we've tested already
            current_bvh_stack + 1,
            // always go up a level if we've completed a loop
            current_bvh_stack - 1,
            loop_completed
          ),
          // increment if we have a hit, decrement if we don't
          select(current_bvh_stack - 1, current_bvh_stack + 1, was_aabb_hit),
          have_not_started_yet
        ),
        // always decrease when hitting sphere
        current_bvh_stack - 1,
        obj_is_sphere
      );
    ///////////////////////////////
    /// atomics
    // get a fresh index
    var fresh_ix = atomicAdd(&bvh__namespaced__workgroups[3], 1u);
    // what x do we need to encapsulate this
    // from the idea that 4u * 16u for y size is a 64 unit stride multiplied by the number of anti-alias passes
    // which with 2**16 as the max in the x dimension is more than enough
    // for any canvas
    var needed_x = ceil(f32(fresh_ix) / f32(4u*rendering_info.anti_alias_passes));
    // atomically set the maximum x work dimension for the next pass
    _ = atomicMax(&bvh__namespaced__workgroups[0], u32(needed_x));
    //////////////////////////////
    /// setters
    // set t and ix
    var new_min_t_and_sphere: t_and_ix;
    new_min_t_and_sphere.t = new_hit_t;
    new_min_t_and_sphere.ix = new_sphere;
    bvh__namespaced__hit_sphere_min[(y_coord * rendering_info.real_canvas_width + x_coord) + (cwch * z_coord)] = t_and_ix_to_u32(&new_min_t_and_sphere);
    // set xy coord
    bvh__namespaced__node_xycoord[fresh_ix] = write_z_at_bitmask(write_y_at_bitmask(write_x_at_bitmask(0u, x_coord), y_coord), z_coord);
    // set the branch, which contains stack and branching info
    bvh__namespaced__branch_bitmask[fresh_ix] =
      write_stack_at_bitmask(
        write_at_right(
          write_at_left(
            current_bvh_lr_bitmask, current_bvh_stack,
            select(
              select(
                select(
                  true,
                  false,
                  loop_completed
                ),
                // if we haven't started the left yet, we only enter if aabb has been hit
                was_aabb_hit,
                have_not_started_yet
              ),
              false,
              obj_is_sphere
            )), 
            // second write at level
            current_bvh_stack,
              select(
                select(
                  select(
                    true,
                    false,
                    loop_completed
                  ),
                  false,
                  have_not_started_yet
                ),
                false,
                obj_is_sphere
              ))
        , new_bvh_stack);
  }
}
"""

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
    debugBuffer <- liftEffect $ createBuffer device $ x
      { size: 65536
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    debugOutputBuffer <- liftEffect $ createBuffer device $ x
      { size: 65536
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.mapRead
      }
    seed <- liftEffect $ randomInt 42 42424242
    randos <- liftEffect $ sequence $ replicate 500
      ( Sphere <$>
          ( { cx: _
            , cy: _
            , cz: _
            , radius: _
            } <$> (random <#> \x -> x * 10.0 - 2.5) <*> pure 0.25 <*> (random <#> \x -> x * -10.0 - 1.0) <*> pure 0.125
          )
      )
    let
      spheres =
        cons' (Sphere { cx: 0.0, cy: 0.0, cz: -1.0, radius: 0.5 })
          ( [ Sphere { cx: 0.0, cy: -100.5, cz: -1.0, radius: 100.0 }
            ] <> randos
          )
      bvhNodes = spheresToBVHNodes seed spheres
      rawSphereData = map fromNumber' (spheresToFlatRep spheres)
    -- logShow bvhNodes
    -- logShow spheres
    bvhNodeData <- liftEffect $ bvhNodesToFloat32Array bvhNodes
    let nSpheres = NEA.length spheres
    let nBVHNodes = NEA.length bvhNodes
    let maxCanvasWidth = 2048
    let maxCanvasHeight = 1024
    let maxAntiAliasPasses = 16
    let maxBufferSize = min deviceLimits.maxStorageBufferBindingSize $ maxCanvasWidth * maxCanvasHeight * maxAntiAliasPasses * 4
    let maxColorBufferSize = min deviceLimits.maxStorageBufferBindingSize $ maxCanvasWidth * maxCanvasHeight * maxAntiAliasPasses * 4 * 4 -- rgba
    sphereData :: Float32Array <- liftEffect $ fromArray rawSphereData
    sphereBuffer <- liftEffect $ createBufferF device sphereData GPUBufferUsage.storage
    bvhNodeBuffer <- liftEffect $ createBufferF device bvhNodeData GPUBufferUsage.storage
    let
      buffMe = liftEffect $ createBuffer device $ x
        { size: maxBufferSize
        , usage: GPUBufferUsage.storage
        }
    --- buffers for hits
    bvh__namespaced__hit_sphere_min <- buffMe -- holds the min hit distance for branches in 16 bits, sphere ix in other 16
    bvh__namespaced__node_ix <- buffMe
    bvh__namespaced__node_xycoord <- buffMe
    bvh__namespaced__branch_bitmask <- buffMe -- arr, 0/1 for tf, alternating l/r
    bvh__namespaced__workgroups__a <- liftEffect $ createBuffer device $ x
      { size: 16
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    bvh__namespaced__workgroups__b <- liftEffect $ createBuffer device $ x
      { size: 16
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    rawColorBuffer <- liftEffect $ createBuffer device $ x
      { size: maxColorBufferSize
      , usage: GPUBufferUsage.storage
      }
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: maxBufferSize
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    let
      clearBufferDesc = x
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
    clearBufferModule <- liftEffect $ createShaderModule device clearBufferDesc
    let
      (clearBufferStage :: GPUProgrammableStage) = x
        { "module": clearBufferModule
        , entryPoint: "main"
        }
    let
      clearWorkgroupsDesc = x
        { code: intercalate "\n"
            [ """
  // main
@group(2) @binding(0) var<storage, read_write> bvh__namespaced__workgroups: array<u32>;
@compute @workgroup_size(32, 1, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  bvh__namespaced__workgroups[global_id.x] = 4u;
}
"""
            ]
        }
    clearWorkgroupsModule <- liftEffect $ createShaderModule device clearWorkgroupsDesc
    let
      (clearWorkgroupsStage :: GPUProgrammableStage) = x
        { "module": clearWorkgroupsModule
        , entryPoint: "main"
        }
    let
      hitDesc = x
        { code: intercalate "\n"
            [ lerp
            , lerpv
            , inputData
            , ray
            , antiAliasFuzzing
            , pointAtParameter
            , hitSphere
            , aabb
            , readWriteAtLevel
            , getTAndIx
            , bvhNode
            , sphereBoundingBox
            , usefulConsts
            , hitMain
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
  var was_i_hit = hit_info[hit_idx];
  var my_color = vec3(0.0,0.0,0.0);
  // my_color = sky_color(&r);
  if (was_i_hit == 0u) {
    my_color = sky_color(&r);
  } else {
    var unpacked = unpack2x16float(was_i_hit);
    var sphere_idx = u32(unpacked[0]);
    var sphere_offset = sphere_idx * 4;
    var norm_t = unpacked[1];
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
    hitsStackBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              ( (0 .. 3) <#> \n -> gpuBindGroupLayoutEntry n GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              )
              , label: "hitsStackBindGroupLayout"
          }
    hitsWorkgroupBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              ( (0 .. 0) <#> \n -> gpuBindGroupLayoutEntry n GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              )
              , label: "hitsWorkgroupBindGroupLayout"
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
    let debugBindGroupLayout = wBindGroupLayout
    -- for when we are reading from a context and writing to a buffer
    readOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout ] }
    hitsPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts:
          [ readerBindGroupLayout
          , hitsStackBindGroupLayout
          , hitsWorkgroupBindGroupLayout
          ]
      }
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
      , label: "readerBindGroup"
      }
    rHitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: bvh__namespaced__hit_sphere_min } :: GPUBufferBinding)
          ]
      , label: "rHitsBindGroup"
      }
    wColorsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rawColorBuffer } :: GPUBufferBinding)
          ]
      , label: "wColorsBindGroup"
      }
    rColorsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rawColorBuffer } :: GPUBufferBinding)
          ]
      , label: "rColorsBindGroup"
      }
    wCanvasBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      , label: "wCanvasBindGroup"
      }
    debugBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: debugBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: debugBuffer } :: GPUBufferBinding)
          ]
          , label: "debugBindGroup"
      }
    hitsStackBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: hitsStackBindGroupLayout
      , entries:
          ( [ bvh__namespaced__hit_sphere_min
            , bvh__namespaced__node_ix
            , bvh__namespaced__node_xycoord
            , bvh__namespaced__branch_bitmask
            ] # mapWithIndex \i n -> gpuBindGroupEntry i
              (x { buffer: n } :: GPUBufferBinding)
          )
      , label: "hitsStackBindGroup"
      }
    workgroupBindGroupA <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          ( [ bvh__namespaced__workgroups__a
            ] # mapWithIndex \i n -> gpuBindGroupEntry i
              (x { buffer: n } :: GPUBufferBinding)
          )
      , label: "workgroupBindGroupA"
      }
    workgroupBindGroupB <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          ( [ bvh__namespaced__workgroups__b
            ] # mapWithIndex \i n -> gpuBindGroupEntry i
              (x { buffer: n } :: GPUBufferBinding)
          )
      , label: "workgroupBindGroupB"
      }
    clearBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: clearBufferStage
      , label: "clearBufferPipeline"
      }
    clearWorkgroupsPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: hitsPipelineLayout
      , compute: clearWorkgroupsStage
      , label: "clearWorkgroupsPipeline"
      }
    hitComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: hitsPipelineLayout
      , compute: hitStage
      , label: "hitComputePipeline"
      }
    colorFillComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readIOPipelineLayout
      , compute: colorFillStage
      , label: "colorFillComputePipeline"
      }
    antiAliasComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readIOPipelineLayout
      , compute: antiAliasStage
      , label: "antiAliasComputePipeline"
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
        -- clear hits as they're subject to an atomic operation
        GPUComputePassEncoder.setPipeline computePassEncoder clearBufferPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          readerBindGroup
        -- clear colors as they're subject to an atomic operation
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wColorsBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 3
        -- set this and leave it there, as this won't change
        -- comment me back in once stuff is sorted out
        let
          work n = do
            -- get hits
            let
              workwork m = do
                --let readA = m `mod` 2 == 0
                --let indirectRead = if readA then bvh__namespaced__workgroups__a else bvh__namespaced__workgroups__b
                --let indirectWrite = if readA then workgroupBindGroupB else workgroupBindGroupA
                --GPUComputePassEncoder.setBindGroup computePassEncoder 2 indirectWrite
                -- GPUComputePassEncoder.setPipeline computePassEncoder
                --   clearWorkgroupsPipeline
                -- GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
                -- GPUComputePassEncoder.setPipeline computePassEncoder
                --   hitComputePipeline
                --GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
                GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 12 12 12
                --GPUComputePassEncoder.dispatchWorkgroupsIndirect computePassEncoder bvh__namespaced__workgroups__a 0
                pure unit
            GPUComputePassEncoder.setPipeline computePassEncoder
                  hitComputePipeline
            GPUComputePassEncoder.setBindGroup computePassEncoder 1 hitsStackBindGroup
            GPUComputePassEncoder.setBindGroup computePassEncoder 2 workgroupBindGroupA
            foreachE (1 .. 128) workwork -- steps in computation 
            -- colorFill
            GPUComputePassEncoder.setBindGroup computePassEncoder 1
              rHitsBindGroup
            GPUComputePassEncoder.setBindGroup computePassEncoder 2
              wColorsBindGroup
            GPUComputePassEncoder.setPipeline computePassEncoder
              colorFillComputePipeline
            GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder (workgroupX / (n)) (workgroupY / (n)) (antiAliasPasses)
        foreachE (1 .. 32) work -- bounces
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
        copyBufferToBuffer commandEncoder debugBuffer 0 debugOutputBuffer 0 65536
        toSubmit <- finish commandEncoder
        submit queue [ toSubmit ]
        --tnf <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
        launchAff_ do
          let debugCondition = whichLoop == 100
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
            klass_ "absolute p-3 text-slate-500"
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
