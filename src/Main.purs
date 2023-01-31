module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Gen (elements)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.Array (fold, intercalate, length, replicate, (..))
import Data.Array as A
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
import Data.Traversable (sequence, sum, traverse)
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
  cwch: u32, // current width times current height
  ambitus_x: f32, // ambitus of the scene in x direction
  ambitus_y: f32, // ambitus of the scene in y direction
  lower_left_x: f32, // lower left corner of the scene in x direction
  lower_left_y: f32, // lower left corner of the scene in y direction
  lower_left_z: f32, // lower left corner of the scene in z direction
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

hitInfo :: String
hitInfo =
  """
struct sky_hit_info {
  ix: u32
}
struct sphere_hit_info {
  t: f32,
  sphere_ix: u32,
  ix: u32
}
struct hit_outcome {
  was_hit: u32,
  sphere_ix: u32,
  ix: u32
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

fuzzable :: String
fuzzable =
  """
const fuzz_fac = 0.5;
fn fuzzable(i: u32, n: u32) -> f32
{
    
    var fi = f32(i);
    var new_n = (f32((i32(n + 2) % 5) - 2)) / 2.0;
    return fi + (new_n * fuzz_fac);
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

pseudoRandom :: String
pseudoRandom =
  """
fn pseudo_random(co:vec2<f32>) -> f32{
    return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
}
const pi = 3.141592653589793;
const two_pi = 2.0 * pi;
fn random_in_unit_sphere(r0: vec2<f32>, r1: vec2<f32>) -> vec3<f32> {
  var phi = two_pi * pseudo_random(r0);
  var theta = two_pi * pseudo_random(r1);
  var sinphi = sin(phi);
  var cosphi = cos(phi);
  var sintheta = sin(theta);
  var costheta = cos(theta);
  var x = sinphi * costheta;
  var y = sinphi * sintheta;
  var z = cosphi;
  return vec3(x, y, z);
}
"""

bvhInfoAtomic :: String
bvhInfoAtomic =
  """
struct bvh_info_atomic {
  n_total: u32,
  n_hits: atomic<u32>,
  n_misses: atomic<u32>,
  n_redos: atomic<u32>,
  n_next_bounce: u32,
  real_canvas_width: u32
}
"""

bvhInfoNonAtomic :: String
bvhInfoNonAtomic =
  """
struct bvh_info_non_atomic {
  n_total: u32,
  n_hits: u32,
  n_misses: u32,
  n_redos: u32,
  n_next_bounce: u32,
  real_canvas_width: u32
}
struct bvh_info_semi_atomic {
  n_total: u32,
  n_hits: u32,
  n_misses: u32,
  n_redos: u32,
  n_next_bounce: atomic<u32>,
  real_canvas_width: u32
}
"""

yAxisStride = "\nconst y_axis_stride = 256u;\n" :: String

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

aliasedColor :: String
aliasedColor =
  """
struct aliased_color {
  r0: f32,
  g0: f32,
  b0: f32,
  r1: f32,
  g1: f32,
  b1: f32,
  r2: f32,
  g2: f32,
  b2: f32,
  r3: f32,
  g3: f32,
  b3: f32,
  r4: f32,
  g4: f32,
  b4: f32
}
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

partitionInitialRaysAndXYZs :: String
partitionInitialRaysAndXYZs =
  """
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> rays : array<ray>;
@group(1) @binding(1) var<storage, read_write> xyzs : array<vec3<u32>>;
@group(1) @binding(2) var<storage, read_write> ixs : array<u32>;
@group(1) @binding(3) var<storage, read_write> raw_colors : array<aliased_color>;
//@group(2) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 64, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var lookup = global_id.x * y_axis_stride + global_id.y;
  if (lookup >= rendering_info.cwch) {
    return;
  }
  // we want each group of 64 threads to work on an 8x8 grid for the initial partition
  var current_grid = lookup / 64;
  var base_lookup = current_grid * 64;
  var place_in_64 = lookup - base_lookup;
  var cg8 = current_grid * 8;
  var running_index = cg8 + (place_in_64 % 8);
  var current_grid_x = running_index % rendering_info.real_canvas_width;
  var current_grid_y = (running_index / rendering_info.real_canvas_width) * 8 + (place_in_64 / 8);
  var px = fuzzable(current_grid_x, 0) / f32(rendering_info.real_canvas_width);
  var py = 1. - fuzzable(current_grid_y, 0) / f32(rendering_info.canvas_height);
  var r: ray;
  r.origin = origin;
  r.direction = vec3(-rendering_info.ambitus_x / 2.0, -rendering_info.ambitus_y / 2.0, -1.0) + vec3(px * rendering_info.ambitus_x, py * rendering_info.ambitus_y, 0.0);
  rays[lookup] = r;
  xyzs[lookup] = vec3<u32>(current_grid_x, current_grid_y, 0u);
  ixs[lookup] = lookup;
  var ac: aliased_color;
  ac.r0 = 1.f;
  ac.g0 = 1.f;
  ac.b0 = 1.f;
  ac.r1 = 1.f;
  ac.g1 = 1.f;
  ac.b1 = 1.f;
  ac.r2 = 1.f;
  ac.g2 = 1.f;
  ac.b2 = 1.f;
  ac.r3 = 1.f;
  ac.g3 = 1.f;
  ac.b3 = 1.f;
  ac.r4 = 1.f;
  ac.g4 = 1.f;
  ac.b4 = 1.f;
  raw_colors[lookup] = ac;
  //if (lookup == 65) {
  //  debug[0] = f32(ixs[lookup]);
  //  debug[1] = f32(current_grid_x);
  //  debug[2] = f32(current_grid_y);
  //  debug[3] = f32(rays[lookup].direction.x);
  //  debug[4] = f32(place_in_64);
  //}
}
  """

setFirstIndirectBuffer :: String
setFirstIndirectBuffer =
  """
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> indirection : vec3<u32>;
@group(2) @binding(0) var<storage, read_write> bvh_info : bvh_info_non_atomic;
//@group(3) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 1, 1)
fn main() {
  // we multiply the stride by 16 because we are only considering the outermost 4 pixels
  // of an 8x8 square
  var divisor = y_axis_stride * 16;
  var xv = rendering_info.cwch / divisor;
  indirection.x = select(xv + 1, xv, (xv * divisor) == rendering_info.cwch);
  indirection.y = 4u;
  indirection.z = 1u;
  bvh_info.n_total = rendering_info.cwch;
  bvh_info.n_hits = 0u;
  bvh_info.n_misses = 0u;
  bvh_info.n_redos = 0u;
  bvh_info.n_next_bounce = 0u;
  bvh_info.real_canvas_width = rendering_info.real_canvas_width;
}
  """

setSecondIndirectBuffer :: String
setSecondIndirectBuffer =
  """
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> indirection : vec3<u32>;
@group(2) @binding(0) var<storage, read_write> bvh_info : bvh_info_non_atomic;
//@group(3) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 1, 1)
fn main() {
  var xv = bvh_info.n_redos / y_axis_stride;
  indirection.x = select(xv + 1, xv, (xv * y_axis_stride) == bvh_info.n_redos);
  indirection.y = 4u;
  indirection.z = 1u;
  bvh_info.n_total = bvh_info.n_redos;
  bvh_info.n_hits = 0u;
  bvh_info.n_misses = 0u;
  bvh_info.n_redos = 0u;
  // we don't reset bvh_info.n_next_bounce because we want to
  // hold onto the number of bounces from the previous pass
  bvh_info.real_canvas_width = rendering_info.real_canvas_width;
}
  """

setThirdIndirectBuffer :: String
setThirdIndirectBuffer =
  """
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> indirection : vec3<u32>;
@group(2) @binding(0) var<storage, read_write> bvh_info : bvh_info_non_atomic;
//@group(3) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 1, 1)
fn main() {
  var xv = bvh_info.n_next_bounce / y_axis_stride;
  indirection.x = select(xv + 1, xv, (xv * y_axis_stride) == bvh_info.n_next_bounce);
  indirection.y = 4u;
  indirection.z = 1u;
  bvh_info.n_total = bvh_info.n_next_bounce;
  bvh_info.n_hits = 0u;
  bvh_info.n_misses = 0u;
  bvh_info.n_redos = 0u;
  bvh_info.n_next_bounce = 0u;
  bvh_info.real_canvas_width = rendering_info.real_canvas_width;
}
  """

assembleSkyAndSphereHitIndirectBuffers :: String
assembleSkyAndSphereHitIndirectBuffers =
  """
@group(0) @binding(0) var<storage, read> bvh_info : bvh_info_non_atomic;
@group(1) @binding(0) var<storage, read_write> sky_hit : vec3<u32>;
@group(2) @binding(0) var<storage, read_write> sphere_hit : vec3<u32>;
@group(3) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 1, 1)
fn main() {
  var x_sky = bvh_info.n_misses / y_axis_stride;
  sky_hit.x = select(x_sky + 1, x_sky, x_sky * y_axis_stride == bvh_info.n_misses);
  sky_hit.y = 4u;
  sky_hit.z = 1u;
  var x_sphere = bvh_info.n_hits / y_axis_stride;
  sphere_hit.x = select(x_sphere + 1, x_sphere, x_sphere * y_axis_stride == bvh_info.n_hits);
  sphere_hit.y = 4u;
  sphere_hit.z = 1u;
  debug[0] = f32(sky_hit.x);
  debug[1] = f32(sky_hit.y);
  debug[2] = f32(sky_hit.z);
  debug[3] = f32(sphere_hit.x);
  debug[4] = f32(sphere_hit.y);
  debug[5] = f32(sphere_hit.z);
}
  """

data ShaderRun = BatchedHits | ScatteredHits

skyHitShader :: { shaderRun :: ShaderRun } -> String
skyHitShader { shaderRun } = fold
  [ """
@group(0) @binding(0) var<storage, read_write> bvh_info : bvh_info_non_atomic;
@group(0) @binding(1) var<storage, read> rays : array<ray>;
@group(0) @binding(2) var<storage, read> xyzs : array<vec3<u32>>;
@group(1) @binding(0) var<storage, read_write> colors : array<aliased_color>;
@group(2) @binding(0) var<storage, read> sky_hits : array<sky_hit_info>;
@group(3) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 64, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>, @builtin(local_invocation_id) local_id : vec3<u32>) {
  var ix = global_id.x * y_axis_stride + global_id.y;
  if (ix > bvh_info.n_misses) {
    return;
  }
  """
  , case shaderRun of
      BatchedHits ->
        """
  var sky_hit = sky_hits[ix / 64];
  var real_ix = sky_hit.ix + local_id.y;
  var ray = rays[real_ix];
  var xyz = xyzs[real_ix];
  """
      ScatteredHits ->
        """
  var sky_hit = sky_hits[ix];
  var ray = rays[sky_hit.ix];
  var xyz = xyzs[sky_hit.ix];
    """
  , """
  
  var color = sky_color(&ray);
  var adjusted_ix = bvh_info.real_canvas_width * xyz.y + xyz.x;
  switch xyz.z {
    case 0u: {
      colors[adjusted_ix].r0 *= color.r;
      colors[adjusted_ix].g0 *= color.g;
      colors[adjusted_ix].b0 *= color.b;
    }
    case 1u: {
      colors[adjusted_ix].r1 *= color.r;
      colors[adjusted_ix].g1 *= color.g;
      colors[adjusted_ix].b1 *= color.b;
    }
    case 2u: {
      colors[adjusted_ix].r2 *= color.r;
      colors[adjusted_ix].g2 *= color.g;
      colors[adjusted_ix].b2 *= color.b;
    }
    case 3u: {
      colors[adjusted_ix].r3 *= color.r;
      colors[adjusted_ix].g3 *= color.g;
      colors[adjusted_ix].b3 *= color.b;
    }
    case 4u: {
      colors[adjusted_ix].r4 *= color.r;
      colors[adjusted_ix].g4 *= color.g;
      colors[adjusted_ix].b4 *= color.b;
    }
    default: {}
  }
}
  """
  ]

sphereHitShader :: { shaderRun :: ShaderRun } -> String
sphereHitShader { shaderRun } = fold
  [ """
const t_min = 0.0001;
const t_max = 10000.f;

@group(0) @binding(0) var<storage, read_write> bvh_info : bvh_info_semi_atomic;
@group(0) @binding(1) var<storage, read> rays : array<ray>;
@group(0) @binding(2) var<storage, read> xyzs : array<vec3<u32>>;
@group(1) @binding(0) var<storage, read_write> colors : array<aliased_color>;
@group(2) @binding(0) var<storage, read> sphere_info : array<f32>;
@group(2) @binding(1) var<storage, read> sphere_hits : array<sphere_hit_info>;
@group(3) @binding(0) var<storage, read_write> rays_write : array<ray>;
@group(3) @binding(1) var<storage, read_write> xyzs_write : array<vec3<u32>>;
    """
  , """
@compute @workgroup_size(1, 64, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>, @builtin(local_invocation_id) local_id : vec3<u32>) {
  var ix = global_id.x * y_axis_stride + global_id.y;
  if (ix > bvh_info.n_hits) {
    return;
  }
  """
  , case shaderRun of
      BatchedHits ->
        """
  var sphere_hit = sphere_hits[ix / 64];
  var real_ix = sphere_hit.ix + local_id.y;
  var r = rays[real_ix];
  var xyz = xyzs[real_ix];
  """
      ScatteredHits ->
        """
  var sphere_hit = sphere_hits[ix];
  var r = rays[sphere_hit.ix];
  var xyz = xyzs[sphere_hit.ix];
    """
  , """
  var sphere_offset = sphere_hit.sphere_ix * 4;
  var h0 = sphere_info[sphere_offset];
  var h1 = sphere_info[sphere_offset + 1];
  var h2 = sphere_info[sphere_offset + 2];
  var h3 = sphere_info[sphere_offset + 3];
  """
  , case shaderRun of
      BatchedHits ->
        """
  // in batched mode, we do not know where the hit happens
  // so we need to calculate it here
  var norm_t: f32;
  hit_sphere(
        h0, h1, h2, h3,
        &r,
        t_min,
        t_max,
        &norm_t);
  """
      ScatteredHits ->
        """
  var norm_t = sphere_hit.t;  
    """
  , """
  var rec: hit_record;
  _ = make_hit_rec(h0, h1, h2, h3, norm_t, &r, &rec);
  var color = vec3(0.5f, 0.5f, 0.5f);//hit_color(&r, &rec);
  ////////////////////////
  // bounce!
  var tgt = rec.p + rec.normal + random_in_unit_sphere(vec2(rec.p.x, rec.p.y), vec2(rec.p.y, rec.p.z));
  //my_color *= 0.5;
  //colors[locxy] = my_color;
  var out_ray: ray;
  out_ray.origin = rec.p;
  out_ray.direction = tgt - rec.p;
  var new_ix = atomicAdd(&bvh_info.n_next_bounce, 1u);
  rays_write[new_ix] = out_ray;
  xyzs_write[new_ix] = xyz;
  ///////////////////////
  // end bounce
  var adjusted_ix = bvh_info.real_canvas_width * xyz.y + xyz.x;
  switch xyz.z {
    case 0u: {
      colors[adjusted_ix].r0 *= color.r;
      colors[adjusted_ix].g0 *= color.g;
      colors[adjusted_ix].b0 *= color.b;
    }
    case 1u: {
      colors[adjusted_ix].r1 *= color.r;
      colors[adjusted_ix].g1 *= color.g;
      colors[adjusted_ix].b1 *= color.b;
    }
    case 2u: {
      colors[adjusted_ix].r2 *= color.r;
      colors[adjusted_ix].g2 *= color.g;
      colors[adjusted_ix].b2 *= color.b;
    }
    case 3u: {
      colors[adjusted_ix].r3 *= color.r;
      colors[adjusted_ix].g3 *= color.g;
      colors[adjusted_ix].b3 *= color.b;
    }
    case 4u: {
      colors[adjusted_ix].r4 *= color.r;
      colors[adjusted_ix].g4 *= color.g;
      colors[adjusted_ix].b4 *= color.b;
    }
    default: {}
  }
}
  """
  ]

assembleRawColorsShader :: String
assembleRawColorsShader =
  """
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read> raw_color_arary : array<aliased_color>;
@group(1) @binding(0) var<storage, read_write> color_arary : array<u32>;
@group(2) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var idx = global_id.x + (global_id.y * rendering_info.real_canvas_width);
  //if (idx >= rendering_info.cwch) {
  //  return;
  //}
  var overshot_idx = global_id.x + (global_id.y * rendering_info.overshot_canvas_width);
  var rc = raw_color_arary[idx];
  // random test for anti-aliasing, should work...

  if (rc.b1 == 1.f && rc.g2 == 1.f && rc.r3 == 1.f) {
      color_arary[overshot_idx] = pack4x8unorm(vec4(
        rc.b0,
        rc.g0,
        rc.r0,
        1.f));
  } else {
  debug[22] = 55565.f;
  color_arary[overshot_idx] = pack4x8unorm(vec4(
    (rc.b0 + rc.b1 + rc.b2 + rc.b3 + rc.b4) / 5.f, 
    (rc.g0 + rc.g1 + rc.g2 + rc.g3 + rc.g4) / 5.f,
    (rc.r0 + rc.r1 + rc.r2 + rc.r3 + rc.r4) / 5.f, 1.f));
  }
}
  """

data BVHRun = QuickBVH | SlowBVH | BounceBVH

bvhComputeBody :: { bvhRun :: BVHRun } -> String
bvhComputeBody { bvhRun } = fold
  [ """
// main

"""

  , case bvhRun of
      QuickBVH ->
        """
// we store outcomes in a workgroup variable and check them at the end
var<workgroup> outcomes: array<hit_outcome, 64>;
  """
      _ -> ""
  , """

const t_min = 0.0001;
const t_max = 10000.f;
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(0) @binding(1) var<storage, read> sphere_info : array<f32>;
@group(0) @binding(2) var<storage, read> bvh_nodes : array<bvh_node>;
@group(1) @binding(0) var<storage, read> rays : array<ray>;
@group(1) @binding(1) var<storage, read_write> bvh_info: bvh_info_atomic;
@group(1) @binding(2) var<storage, read_write> sphere_hit : array<sphere_hit_info>;
@group(1) @binding(3) var<storage, read_write> sky_hit : array<sky_hit_info>;
"""

  , case bvhRun of
      QuickBVH ->
        """
@group(1) @binding(4) var<storage, read_write> ixs : array<u32>; // write in quick bvh
  """
      SlowBVH ->
        """
@group(1) @binding(4) var<storage, read> ixs : array<u32>;
"""
      BounceBVH -> ""
  , """
@compute @workgroup_size(1, 64, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>, @builtin(local_invocation_id) local_id : vec3<u32>) {
  ////////////////////////////////////////
  ////////////////////////////////////////
  var lookup = global_id.x * y_axis_stride + global_id.y;
  var lookup_mod4 = local_id.y % 4;
  var ix: u32;
  if (lookup < bvh_info.n_total) {
  """
  , case bvhRun of
      QuickBVH ->
        """
    // in quick bvh, we look at the four corners only
    var initial_lookup = lookup;
    lookup = ((initial_lookup / 4) * 64) + select(select(select(63u, 56u, lookup_mod4 == 2u), 7u, lookup_mod4 == 1u) , 0u, lookup_mod4 == 0u);
    // we're reading directly into a buffer that has been pre-sorted in units of 64, so we can do a direct lookup to get the corners
    ix = lookup;
  """
      SlowBVH ->
        """
    // in slow bvh, we're doing an entire bloc of 64
    // so we first get the overall index and then add the correct thread position to it
    ix = ixs[lookup / 64] + local_id.y;
        """
      BounceBVH ->
        """
    // the data is unsorted, so we need to lookup the correct index
    ix = lookup;
    """
  , """
    var ray = rays[ix];
    var hit: f32;
    var sphere_ix: u32;
    var bvh_read_playhead: u32;
    var bvh_write_playhead: u32;
    var sphere_read_playhead: u32;
    var sphere_write_playhead: u32;
    var bvh_ixs: array<u32, 64>;
    var sphere_ixs: array<u32, 64>;

    ////////////////////////////////////////
    ////////////////////////////////////////
    var last_node_ix = rendering_info.n_bvh_nodes - 1;

    ////////////////////////////////////////
    var last_node = bvh_nodes[last_node_ix];
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
        var bloc = bvh_ixs[bvh_read_playhead % 64];
        var node = bvh_nodes[bloc];
        var bbox: aabb;
        bvh_node_bounding_box(&node, &bbox);
        var was_aabb_hit = aabb_hit(&bbox, &ray, t_min, t_max);
        /////
        if (was_aabb_hit) {
          if (bvh_nodes[node.left].is_sphere == 1u) {
            sphere_ixs[sphere_write_playhead % 64] = node.left;
            sphere_write_playhead++;
          } else {
            bvh_ixs[(bvh_write_playhead) % 64] = node.left;
            bvh_write_playhead++;
          }
          if (bvh_nodes[node.right].is_sphere == 1u) {
            sphere_ixs[(sphere_write_playhead) % 64] = node.right;
            sphere_write_playhead++;
          } else {
            bvh_ixs[(bvh_write_playhead) % 64] = node.right;
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
        var sloc = sphere_ixs[(sphere_read_playhead) % 64];
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
        var old_ix = sphere_ix;
        sphere_ix =
          select(old_ix, select(old_ix, i_plus_1, new_t != old_t), sphere_hit);
        sphere_read_playhead += 1;
      }
      is_done = (sphere_write_playhead == sphere_read_playhead) && (bvh_write_playhead == bvh_read_playhead);
    }

    var i_was_hit  = sphere_ix > 0u;
  """
  , case bvhRun of
      QuickBVH ->
        """
    if (i_was_hit) {
      var outcome: hit_outcome;
      outcome.sphere_ix = sphere_ix - 1;
      outcome.ix = ix;
      outcome.was_hit = 1u;
      outcomes[local_id.y] = outcome;
    } else {
      var outcome: hit_outcome;
      outcome.sphere_ix = 0xffffffffu;
      outcome.ix = ix;
      outcome.was_hit = 0u;
      outcomes[local_id.y] = outcome;
    }
    // we put a workgroup barrier in place to make sure that
    // the computation is complete up until this point, as we're about to triage based on the results
    // one of three things is possible here:
    // - we hit a sphere
    // - we hit the sky
    // - we hit both and we need to rerun stuff
  }
  workgroupBarrier();
  if (lookup < bvh_info.n_total) {
    // only mod4 threads will actually write to arrays
    if (lookup_mod4 != 0u) { return; }
    var res0 = outcomes[local_id.y];
    var res1 = outcomes[local_id.y + 1];
    var res2 = outcomes[local_id.y + 2];
    var res3 = outcomes[local_id.y + 3];
    if (res0.was_hit == 0u && res1.was_hit == 0u && res2.was_hit == 0u && res3.was_hit == 0u) {
      var i = atomicAdd(&bvh_info.n_misses, 64u);
      var hit_info: sky_hit_info;
      hit_info.ix = ix; // ix will always be the head of a workgroup of 64
      sky_hit[i / 64u] = hit_info;
    } else if (res0.was_hit == 1u && res0.sphere_ix == res1.sphere_ix && res0.sphere_ix == res2.sphere_ix && res0.sphere_ix == res3.sphere_ix) {
      var i = atomicAdd(&bvh_info.n_hits, 64u);
      var hit_info: sphere_hit_info;
      hit_info.t = -1.f; // we do not record hits in quick bvh, as they'll need to be computed in the coloring shader
      hit_info.sphere_ix = res0.sphere_ix;
      hit_info.ix = ix; // ix will always be the head of a workgroup of 64
      sphere_hit[i / 64u] = hit_info;
    } else {
      var i = atomicAdd(&bvh_info.n_redos, 64u);
      ixs[i / 64u] = ix;
    }
  }
}
  """
      _ ->
        """
    if (i_was_hit) {
      var i = atomicAdd(&bvh_info.n_hits, 1u);
      var hit_info: sphere_hit_info;
      hit_info.t = hit;
      hit_info.sphere_ix = sphere_ix - 1;
      hit_info.ix = ix;
      sphere_hit[i] = hit_info;
    } else {
      var i = atomicAdd(&bvh_info.n_misses, 1u);
      var hit_info: sky_hit_info;
      hit_info.ix = ix;
      sky_hit[i] = hit_info;
    }
  }
}
"""
  ]

makeBVHStage :: GPUDevice -> BVHRun -> Effect GPUProgrammableStage
makeBVHStage device bvhRun = do
  let
    bvhDesc = x
      { code:
          intercalate "\n"
            [ inputData
            , fuzzable
            , ray
            , yAxisStride
            , usefulConsts
            , bvhInfoAtomic
            , hitInfo
            , bvhNode
            , aabb
            , hitSphere
            , bvhComputeBody { bvhRun }
            ]
      }
  bvhModule <- createShaderModule device bvhDesc
  pure $ x
    { "module": bvhModule
    , entryPoint: "main"
    }

makeSkyHitStage :: GPUDevice -> ShaderRun -> Effect GPUProgrammableStage
makeSkyHitStage device shaderRun = do
  let
    skyHitDesc = x
      { code:
          intercalate "\n"
            [ inputData
            , fuzzable
            , ray
            , bvhInfoNonAtomic
            , lerp
            , lerpv
            , yAxisStride
            , usefulConsts
            , hitInfo
            , hitRecord
            , calcColors
            , aliasedColor
            , skyHitShader { shaderRun }
            ]
      }
  skyHitModule <- liftEffect $ createShaderModule device skyHitDesc
  pure $ x
    { "module": skyHitModule
    , entryPoint: "main"
    }

makeSphereHitStage :: GPUDevice -> ShaderRun -> Effect GPUProgrammableStage
makeSphereHitStage device shaderRun = do
  let
    sphereHitDesc = x
      { code:
          intercalate "\n"
            [ inputData
            , fuzzable
            , bvhInfoNonAtomic
            , ray
            , lerp
            , lerpv
            , yAxisStride
            , usefulConsts
            , hitInfo
            , hitRecord
            , calcColors
            , aliasedColor
            , makeHitRec
            , hitSphere
            , pointAtParameter
            , pseudoRandom
            , sphereHitShader { shaderRun }
            ]
      }
  sphereHitModule <- liftEffect $ createShaderModule device sphereHitDesc
  pure $ x
    { "module": sphereHitModule
    , entryPoint: "main"
    }

eightify :: Int -> Int
eightify i = if x == i then i else x + 8
  where
  x = (i / 8) * 8

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

averager :: Effect (Number -> Effect Number)
averager = do
  ct <- Ref.new []
  pure \v -> do
    ct' <- Ref.modify (\x -> if length x >= 10 then A.drop 1 x else x <> [v] ) ct
    pure $ sum ct' / toNumber (length ct')

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

data PipelineRun = QuickRun | SlowRun | BounceRun

derive instance Eq PipelineRun

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
      { size: 48 -- align(4) size(52)
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
    randos <- liftEffect $ sequence $ replicate 512 $ Sphere <$> ({ cx: _, cy: _, cz: _, radius: 0.125 } <$> (random <#> \n -> n * 16.0 - 8.0) <*> (random <#> \n -> n * 3.0 + 0.25) <*> (random <#> \n -> n * 16.0 - 8.0))
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
    sphereData :: Float32Array <- liftEffect $ fromArray rawSphereData
    sphereBuffer <- liftEffect $ createBufferF device sphereData GPUBufferUsage.storage
    bvhNodeBuffer <- liftEffect $ createBufferF device bvhNodeData GPUBufferUsage.storage
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    ixBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize / 16
      , usage: GPUBufferUsage.storage
      }
    xyzEvenBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize / 4
      , usage: GPUBufferUsage.storage
      }
    rayEvenBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize / 4
      , usage: GPUBufferUsage.storage
      }
    xyzOddBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize / 4
      , usage: GPUBufferUsage.storage
      }
    rayOddBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize / 4
      , usage: GPUBufferUsage.storage
      }
    skyHits <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize / 16
      , usage: GPUBufferUsage.storage
      }
    sphereHits <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize / 16
      , usage: GPUBufferUsage.storage
      }
    indirectBuffer <- liftEffect $ createBuffer device $ x
      { size: 12
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    rawColorBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    skyHitIndirectBuffer <- liftEffect $ createBuffer device $ x
      { size: 12
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    sphereHitIndirectBuffer <- liftEffect $ createBuffer device $ x
      { size: 12
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    bvhInfo <- liftEffect $ createBuffer device $ x
      { size: 24
      , usage: GPUBufferUsage.storage
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
      resetFirstIndirectBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , bvhInfoNonAtomic
              , yAxisStride
              , setFirstIndirectBuffer
              ]
        }
    resetFirstIndirectBufferModule <- liftEffect $ createShaderModule device resetFirstIndirectBufferDesc
    let
      (resetFirstIndirectBufferStage :: GPUProgrammableStage) = x
        { "module": resetFirstIndirectBufferModule
        , entryPoint: "main"
        }
    let
      resetSecondIndirectBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , bvhInfoNonAtomic
              , yAxisStride
              , setSecondIndirectBuffer
              ]
        }
    resetSecondIndirectBufferModule <- liftEffect $ createShaderModule device resetSecondIndirectBufferDesc
    let
      (resetSecondIndirectBufferStage :: GPUProgrammableStage) = x
        { "module": resetSecondIndirectBufferModule
        , entryPoint: "main"
        }
    let
      resetThirdIndirectBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , bvhInfoNonAtomic
              , yAxisStride
              , setThirdIndirectBuffer
              ]
        }
    resetThirdIndirectBufferModule <- liftEffect $ createShaderModule device resetThirdIndirectBufferDesc
    let
      (resetThirdIndirectBufferStage :: GPUProgrammableStage) = x
        { "module": resetThirdIndirectBufferModule
        , entryPoint: "main"
        }
    let
      assembleSkyAndSphereHitDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , bvhInfoNonAtomic
              , yAxisStride
              , assembleSkyAndSphereHitIndirectBuffers
              ]
        }
    assembleSkyAndSphereHitModule <- liftEffect $ createShaderModule device assembleSkyAndSphereHitDesc
    let
      (assembleSkyAndSphereHitStage :: GPUProgrammableStage) = x
        { "module": assembleSkyAndSphereHitModule
        , entryPoint: "main"
        }
    let
      partitionInitialRaysAndXYZsDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , fuzzable
              , ray
              , aliasedColor
              , yAxisStride
              , usefulConsts
              , partitionInitialRaysAndXYZs
              ]
        }
    partitionInitialRaysAndXYZsModule <- liftEffect $ createShaderModule device partitionInitialRaysAndXYZsDesc
    let
      (partitionInitialRaysAndXYZsStage :: GPUProgrammableStage) = x
        { "module": partitionInitialRaysAndXYZsModule
        , entryPoint: "main"
        }
    let
      assembleRawColorsDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , fuzzable
              , ray
              , aliasedColor
              , yAxisStride
              , usefulConsts
              , assembleRawColorsShader
              ]
        }
    assembleRawColorsModule <- liftEffect $ createShaderModule device assembleRawColorsDesc
    let
      (assembleRawColorsStage :: GPUProgrammableStage) = x
        { "module": assembleRawColorsModule
        , entryPoint: "main"
        }
    --     let
    --       contrastDesc = x
    --         { code:
    --             intercalate "\n"
    --               [ inputData
    --               , """
    -- // main
    -- @group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
    -- @group(1) @binding(0) var<storage, read_write> color_array : array<u32>;
    -- @group(2) @binding(0) var<storage, read_write> contrast_ixs : array<u32>;
    -- @group(2) @binding(1) var<storage, read_write> contrast_counter : atomic<u32>;
    -- @compute @workgroup_size(16, 16, 1)
    -- fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
    --   var i_x = global_id.x * 4;
    --   var i_y = global_id.y * 4;
    --   if (i_x >= rendering_info.real_canvas_width || i_y >= rendering_info.canvas_height) {
    --     return;
    --   }
    --   var c0ix =   i_x + ((i_y + 0) * rendering_info.overshot_canvas_width);
    --   var c1ix = 1 + c0ix;
    --   var c2ix = 2 + c0ix;
    --   var c3ix = 3 + c0ix;
    --   var c4ix =   i_x + ((i_y + 1) * rendering_info.overshot_canvas_width);
    --   var c5ix = 1 + c4ix;
    --   var c6ix = 2 + c4ix;
    --   var c7ix = 3 + c4ix;
    --   var c8ix =   i_x + ((i_y + 2) * rendering_info.overshot_canvas_width);
    --   var c9ix = 1 + c8ix;
    --   var c10ix = 2 + c8ix;
    --   var c11ix = 3 + c8ix;
    --   var c12ix =   i_x + ((i_y + 3) * rendering_info.overshot_canvas_width);
    --   var c13ix = 1 + c12ix;
    --   var c14ix = 2 + c12ix;
    --   var c15ix = 3 + c12ix;
    --   var c0 = unpack4x8unorm(color_array[c0ix]);
    --   var c1 = unpack4x8unorm(color_array[c1ix]);
    --   var c2 = unpack4x8unorm(color_array[c2ix]);
    --   var c3 = unpack4x8unorm(color_array[c3ix]);
    --   var c4 = unpack4x8unorm(color_array[c4ix]);
    --   var c5 = unpack4x8unorm(color_array[c5ix]);
    --   var c6 = unpack4x8unorm(color_array[c6ix]);
    --   var c7 = unpack4x8unorm(color_array[c7ix]);
    --   var c8 = unpack4x8unorm(color_array[c8ix]);
    --   var c9 = unpack4x8unorm(color_array[c9ix]);
    --   var c10 = unpack4x8unorm(color_array[c10ix]);
    --   var c11 = unpack4x8unorm(color_array[c11ix]);
    --   var c12 = unpack4x8unorm(color_array[c12ix]);
    --   var c13 = unpack4x8unorm(color_array[c13ix]);
    --   var c14 = unpack4x8unorm(color_array[c14ix]);
    --   var c15 = unpack4x8unorm(color_array[c15ix]);
    --   var mn = min(c0, min(c1, min(c2, min(c3, min(c4, min(c5, min(c6, min(c7, min(c8, min(c9, min(c10, min(c11, min(c12, min(c13, min(c14, c15)))))))))))))));
    --   var mx = max(c0, max(c1, max(c2, max(c3, max(c4, max(c5, max(c6, max(c7, max(c8, max(c9, max(c10, max(c11, max(c12, max(c13, max(c14, c15)))))))))))))));
    --   if (distance(vec3(mx.r,mx.g,mx.b),vec3(mn.r,mn.g,mn.b)) > """
    --               , show contrastThreshold
    --               , """) {
    --     var ix = atomicAdd(&contrast_counter, 1u);
    --     contrast_ixs[ix] = i_x | (i_y << 16);
    --   }
    -- }"""
    --               ]
    --         }
    --     contrastModule <- liftEffect $ createShaderModule device contrastDesc

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
    r2BindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              (0 .. 1) <#> \j -> gpuBindGroupLayoutEntry j GPUShaderStage.compute
                ( x { type: GPUBufferBindingType.readOnlyStorage }
                    :: GPUBufferBindingLayout
                )
          , label: "r2BindGroupLayout"
          }
    w1r2BindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              (0 .. 2) <#> \j -> gpuBindGroupLayoutEntry j GPUShaderStage.compute
                ( x { type: if j == 0 then GPUBufferBindingType.storage else GPUBufferBindingType.readOnlyStorage }
                    :: GPUBufferBindingLayout
                )
          , label: "w1r2BindGroupLayout"
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
    w2BindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries: (0 .. 1) <#> \j -> gpuBindGroupLayoutEntry j GPUShaderStage.compute
              ( x { type: GPUBufferBindingType.storage }
                  :: GPUBufferBindingLayout
              )
          , label: "w2BindGroupLayout"
          }
    w4BindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              (0 .. 3) <#> \j -> gpuBindGroupLayoutEntry j GPUShaderStage.compute
                ( x { type: GPUBufferBindingType.storage }
                    :: GPUBufferBindingLayout
                )
          , label: "w4BindGroupLayout"
          }
    slowBvhBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.readOnlyStorage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 3 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 4 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "slowBvhBindGroupLayout"
          }
    bounceBvhBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.readOnlyStorage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 3 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              ]
          , label: "bounceBvhBindGroupLayout"
          }
    quickBvhBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.readOnlyStorage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 3 GPUShaderStage.compute
                  (x { type: GPUBufferBindingType.storage } :: GPUBufferBindingLayout)
              , gpuBindGroupLayoutEntry 4 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "quickBvhBindGroupLayout"
          }
    let debugBindGroupLayout = wBindGroupLayout
    readOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout, debugBindGroupLayout ]
      , label: "readOPipelineLayout"
      }
    readOOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout, wBindGroupLayout, debugBindGroupLayout ]
      , label: "readOOPipelineLayout"
      }
    skyAndSpherePipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ rBindGroupLayout, wBindGroupLayout, wBindGroupLayout, debugBindGroupLayout ]
      , label: "skyAndSpherePipelineLayout"
      }
    partitionInitialRaysAndXYZsBindGroupLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, w4BindGroupLayout, debugBindGroupLayout ]
      , label: "partitionInitialRaysAndXYZsBindGroupLayout"
      }
    quickBvhPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, quickBvhBindGroupLayout ]
      , label: "quickBvhPipelineLayout"
      }
    slowBvhPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, slowBvhBindGroupLayout ]
      , label: "slowBvhPipelineLayout"
      }
    bounceBvhPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, bounceBvhBindGroupLayout ]
      , label: "bounceBvhPipelineLayout"
      }
    -- technically the sky shader doesn't need to write to bvh info as we're done with bounces
    -- but as changing it to read-only would require a layout switch we keep as write for now
    -- makes code easier to maintain
    skyShadingPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ w1r2BindGroupLayout, wBindGroupLayout, rBindGroupLayout, debugBindGroupLayout ]
      , label: "shadingPipelineLaout"
      }
    sphereShadingPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ w1r2BindGroupLayout, wBindGroupLayout, r2BindGroupLayout, w2BindGroupLayout ]
      , label: "sphereShadingPipelineLayout"
      }
    assembleRawColorsPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ r2BindGroupLayout, wBindGroupLayout, debugBindGroupLayout ]
      , label: "assembleRawColorsPipelineLayout"
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
    wIndirectBufferBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: indirectBuffer } :: GPUBufferBinding)
          ]
      , label: "wIndirectBufferBindGroup"
      }
    wBvhInfoBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          ]
      , label: "wBvhInfoBindGroup"
      }
    wRawColorBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rawColorBuffer } :: GPUBufferBinding)
          ]
      , label: "wRawColorBindGroup"
      }
    shaderReaderEvenBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: w1r2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: rayEvenBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: xyzEvenBuffer } :: GPUBufferBinding)
          ]
      , label: "shaderReaderEvenBindGroup"
      }
    shaderReaderOddBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: w1r2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: rayOddBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: xyzOddBuffer } :: GPUBufferBinding)
          ]
      , label: "shaderReaderEvenBindGroup"
      }
    shaderWriterOddBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: w2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rayOddBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: xyzOddBuffer } :: GPUBufferBinding)
          ]
      , label: "shaderWriterOddBindGroup"
      }
    shaderWriterEvenBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: w2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rayEvenBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: xyzEvenBuffer } :: GPUBufferBinding)
          ]
      , label: "shaderWriterEvenBindGroup"
      }
    wInitialPartitionsEvenBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: w4BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rayEvenBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: xyzEvenBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: ixBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: rawColorBuffer } :: GPUBufferBinding)
          ]
      , label: "wInitialPartitionsBindGroup"
      }
    rBVHInfoBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          ]
      , label: "rBVHInfoBindGroup"
      }
    quickBvhBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: quickBvhBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rayEvenBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: sphereHits } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: skyHits } :: GPUBufferBinding)
          , gpuBindGroupEntry 4
              (x { buffer: ixBuffer } :: GPUBufferBinding)
          ]
      , label: "quickBvhBindGroup"
      }
    slowBvhBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: slowBvhBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rayEvenBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: sphereHits } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: skyHits } :: GPUBufferBinding)
          , gpuBindGroupEntry 4
              (x { buffer: ixBuffer } :: GPUBufferBinding)
          ]
      , label: "slowBvhBindGroup"
      }
    bounceBvhOddBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: bounceBvhBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rayOddBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: sphereHits } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: skyHits } :: GPUBufferBinding)
          ]
      , label: "bounceBvhOddBindGroup"
      }
    bounceBvhEvenBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: bounceBvhBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rayEvenBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: bvhInfo } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: sphereHits } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: skyHits } :: GPUBufferBinding)
          ]
      , label: "bounceBvhEvenBindGroup"
      }
    wSkyHitIndirectBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: skyHitIndirectBuffer } :: GPUBufferBinding)
          ]
      , label: "wSkyHitIndirectBindGroup"
      }
    wSphereHitIndirectBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: sphereHitIndirectBuffer } :: GPUBufferBinding)
          ]
      , label: "wSphereHitIndirectBindGroup"
      }
    readSkyInfoBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: skyHits } :: GPUBufferBinding)
          ]
      , label: "readSkyInfoBindGroup"
      }
    readSphereInfoBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: r2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: sphereBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: sphereHits } :: GPUBufferBinding)
          ]
      , label: "readSphereInfoBindGroup"
      }
    assembleRawColorsReaderBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: r2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: canvasInfoBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: rawColorBuffer } :: GPUBufferBinding)
          ]
      , label: "assembleRawColorsReaderBindGroup"
      }
    debugBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: debugBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: debugBuffer } :: GPUBufferBinding)
          ]
      , label: "debugBindGroup"
      }
    --
    zeroOutBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: zeroOutBufferStage
      , label: "zeroOutBufferPipeline"
      }
    resetFirstIndirectBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOOPipelineLayout
      , compute: resetFirstIndirectBufferStage
      , label: "resetFirstIndirectBufferPipeline"
      }
    resetSecondIndirectBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOOPipelineLayout
      , compute: resetSecondIndirectBufferStage
      , label: "resetSecondIndirectBufferPipeline"
      }
    resetThirdIndirectBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOOPipelineLayout
      , compute: resetThirdIndirectBufferStage
      , label: "resetThirdIndirectBufferPipeline"
      }
    partitionInitialRaysAndXYZsPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: partitionInitialRaysAndXYZsBindGroupLayout
      , compute: partitionInitialRaysAndXYZsStage
      , label: "partitionInitialRaysAndXYZsPipeline"
      }
    bvhQuickStage <- liftEffect $ makeBVHStage device QuickBVH
    bvhQuickPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: quickBvhPipelineLayout
      , compute: bvhQuickStage
      , label: "bvhQuickPipeline"
      }
    bvhSlowStage <- liftEffect $ makeBVHStage device SlowBVH
    bvhSlowPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: slowBvhPipelineLayout
      , compute: bvhSlowStage
      , label: "bvhSlowPipeline"
      }
    bvhBounceStage <- liftEffect $ makeBVHStage device BounceBVH
    bvhBouncePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: bounceBvhPipelineLayout
      , compute: bvhBounceStage
      , label: "bvhBouncePipeline"
      }
    assembleSkyAndSphereHitPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: skyAndSpherePipelineLayout
      , compute: assembleSkyAndSphereHitStage
      , label: "assembleSkyAndSphereHitPipeline"
      }
    skyHitBatchedStage <- liftEffect $ makeSkyHitStage device BatchedHits
    skyHitBatchedPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: skyShadingPipelineLayout
      , compute: skyHitBatchedStage
      , label: "skyHitBatchedPipeline"
      }
    skyHitScatteredStage <- liftEffect $ makeSkyHitStage device ScatteredHits
    skyHitScatteredPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: skyShadingPipelineLayout
      , compute: skyHitScatteredStage
      , label: "skyHitScatteredPipeline"
      }
    sphereHitBatchedStage <- liftEffect $ makeSphereHitStage device BatchedHits
    sphereHitBatchedPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: sphereShadingPipelineLayout
      , compute: sphereHitBatchedStage
      , label: "sphereHitBatchedPipeline"
      }
    sphereHitScatteredStage <- liftEffect $ makeSphereHitStage device ScatteredHits
    sphereHitScatteredPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: sphereShadingPipelineLayout
      , compute: sphereHitScatteredStage
      , label: "sphereHitScatteredStage"
      }
    assembleRawColorsPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: assembleRawColorsPipelineLayout
      , compute: assembleRawColorsStage
      , label: "assembleRawColorsPipeline"
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
        canvasWidth' <- width canvas
        canvasHeight' <- height canvas
        let canvasWidth = eightify canvasWidth'
        let canvasHeight = eightify canvasHeight'
        let bufferWidth = ceil (toNumber canvasWidth * 4.0 / 256.0) * 256
        let overshotWidth = bufferWidth / 4
        tn <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
        cf <- Ref.read currentFrame
        Ref.write (cf + 1) currentFrame
        commandEncoder <- createCommandEncoder device (x {})
        let workgroupOvershootX = ceil (toNumber overshotWidth / 16.0)
        let workgroupOvershootY = ceil (toNumber canvasHeight / 16.0)
        let workgroupXForIndirectPartition = ceil (toNumber (canvasWidth * canvasHeight) / toNumber (4 * 64))
        let workgroupX = ceil (toNumber canvasWidth / 16.0)
        let workgroupY = ceil (toNumber canvasHeight / 16.0)
        cinfo <- fromArray $ map fromInt
          [ canvasWidth
          , overshotWidth
          , canvasHeight
          , nSpheres
          , nBVHNodes
          , canvasWidth * canvasHeight
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0
          ]
        let aspect = toNumber canvasWidth / toNumber canvasHeight
        let ambitus_x = if aspect < 1.0 then 2.0 else 2.0 * aspect
        let ambitus_y = if aspect >= 1.0 then 2.0 else 2.0 * aspect
        let lower_left_x = -ambitus_x / 2.0
        let lower_left_y = -ambitus_y / 2.0
        let lower_left_z = -1.0
        let asBuffer = buffer cinfo
        whole asBuffer >>= \(x :: Float32Array) -> void $ set x (Just 6)
          [ fromNumber' ambitus_x
          , fromNumber' ambitus_y
          , fromNumber' lower_left_x
          , fromNumber' lower_left_y
          , fromNumber' lower_left_z
          , fromNumber' tn
          ]
        -- logShow { canvasWidth, canvasHeight, workgroupXForIndirectPartition, ambitus_x, ambitus_y }
        writeBuffer queue canvasInfoBuffer 0 (fromUint32Array cinfo)
        -- not necessary in the loop, but useful as a stress test for animating positions
        computePassEncoder <- beginComputePass commandEncoder (x {})
        -- set reader for all computations
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          readerBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 2
          debugBindGroup
        -----------------------
        -- clear canvas
        GPUComputePassEncoder.setPipeline computePassEncoder zeroOutBufferPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wCanvasBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupOvershootX workgroupOvershootY 1
        -----------------------
        -- set ray & xyz buffer
        -- debugged
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wInitialPartitionsEvenBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder partitionInitialRaysAndXYZsPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupXForIndirectPartition 4 1
        foreachE (mapWithIndex Tuple [ QuickRun, SlowRun, BounceRun, BounceRun, BounceRun, BounceRun, BounceRun ]) \(Tuple idx runType) -> do
          -----------------------
          -- set indirect buffer
          when (runType /= QuickRun) do
            GPUComputePassEncoder.setBindGroup computePassEncoder 0
              readerBindGroup

          GPUComputePassEncoder.setBindGroup computePassEncoder 1
            wIndirectBufferBindGroup
          GPUComputePassEncoder.setBindGroup computePassEncoder 2
            wBvhInfoBindGroup
          GPUComputePassEncoder.setBindGroup computePassEncoder 3
            debugBindGroup
          GPUComputePassEncoder.setPipeline computePassEncoder $ case runType of
            QuickRun -> resetFirstIndirectBufferPipeline
            SlowRun -> resetSecondIndirectBufferPipeline
            BounceRun -> resetThirdIndirectBufferPipeline
          GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
          -----------------------
          -- do bvh
          GPUComputePassEncoder.setBindGroup computePassEncoder 2
            debugBindGroup
          GPUComputePassEncoder.setBindGroup computePassEncoder 1 $ case runType of
            QuickRun -> quickBvhBindGroup
            SlowRun -> slowBvhBindGroup
            BounceRun -> case idx `mod` 2 of
              0 -> bounceBvhOddBindGroup
              _ -> bounceBvhEvenBindGroup
          GPUComputePassEncoder.setPipeline computePassEncoder $ case runType of
            QuickRun -> bvhQuickPipeline
            SlowRun -> bvhSlowPipeline
            BounceRun -> bvhBouncePipeline
          GPUComputePassEncoder.dispatchWorkgroupsIndirect computePassEncoder indirectBuffer 0
          -----------------------
          -- assemble coloring info
          GPUComputePassEncoder.setBindGroup computePassEncoder 0
            rBVHInfoBindGroup
          GPUComputePassEncoder.setBindGroup computePassEncoder 1
            wSkyHitIndirectBindGroup
          GPUComputePassEncoder.setBindGroup computePassEncoder 2
            wSphereHitIndirectBindGroup
          GPUComputePassEncoder.setBindGroup computePassEncoder 3
            debugBindGroup
          GPUComputePassEncoder.setPipeline computePassEncoder
            assembleSkyAndSphereHitPipeline
          GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
          -----------------------
          -- reader for all shading
          GPUComputePassEncoder.setBindGroup computePassEncoder 0 $ case runType of
            QuickRun -> shaderReaderEvenBindGroup
            SlowRun -> shaderReaderEvenBindGroup
            -- bounces need to start with the odd bind group
            -- because our first pass is actually two passes, quick bvh then slow
            BounceRun -> case idx `mod` 2 of
              0 -> shaderReaderOddBindGroup
              _ -> shaderReaderEvenBindGroup
          GPUComputePassEncoder.setBindGroup computePassEncoder 1
            wRawColorBindGroup
          -----------------------
          -- color sky
          GPUComputePassEncoder.setBindGroup computePassEncoder 2
            readSkyInfoBindGroup
          GPUComputePassEncoder.setPipeline computePassEncoder $ case runType of
            QuickRun -> skyHitBatchedPipeline
            SlowRun -> skyHitScatteredPipeline
            BounceRun -> skyHitScatteredPipeline
          GPUComputePassEncoder.dispatchWorkgroupsIndirect computePassEncoder skyHitIndirectBuffer 0
          -- color spheres
          GPUComputePassEncoder.setBindGroup computePassEncoder 2
            readSphereInfoBindGroup
          case runType of
            QuickRun -> GPUComputePassEncoder.setBindGroup computePassEncoder 3 shaderWriterOddBindGroup
            SlowRun -> GPUComputePassEncoder.setBindGroup computePassEncoder 3 shaderWriterOddBindGroup
            -- on stage 2 (the first bounce stage, we are writing to even)
            BounceRun -> GPUComputePassEncoder.setBindGroup computePassEncoder 3 $ case idx `mod` 2 of
              0 -> shaderWriterEvenBindGroup
              _ -> shaderWriterOddBindGroup
          GPUComputePassEncoder.setPipeline computePassEncoder $ case runType of
            QuickRun -> sphereHitBatchedPipeline
            SlowRun -> sphereHitScatteredPipeline
            BounceRun -> sphereHitScatteredPipeline
          GPUComputePassEncoder.dispatchWorkgroupsIndirect computePassEncoder sphereHitIndirectBuffer 0
        -----------------------
        -- merge anti-alias passes
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          assembleRawColorsReaderBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wCanvasBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 2
          debugBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder
          assembleRawColorsPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -----------------------
        -- fin
        GPUComputePassEncoder.end computePassEncoder
        copyBufferToTexture
          commandEncoder
          (x { buffer: wholeCanvasBuffer, bytesPerRow: bufferWidth })
          (x { texture: colorTexture })
          (gpuExtent3DWH canvasWidth' canvasHeight')
        copyBufferToBuffer commandEncoder debugBuffer 0 debugOutputBuffer 0 65536
        toSubmit <- finish commandEncoder
        submit queue [ toSubmit ]
        let debugCondition = false -- whichLoop == 100
        launchAff_ do
          toAffE $ convertPromise <$> if debugCondition then mapAsync debugOutputBuffer GPUMapMode.read else onSubmittedWorkDone queue
          liftEffect do
            when debugCondition do
              bfr <- getMappedRange debugOutputBuffer
              buffy <- (Typed.whole bfr :: Effect Float32Array) >>= Typed.toArray
              let _ = spy "buffy" buffy
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