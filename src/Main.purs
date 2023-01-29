module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Gen (elements)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.Array (fold, intercalate, length, replicate, (..))
import Data.Array.NonEmpty (NonEmptyArray, cons', drop, fromNonEmpty, snoc, snoc', sortBy, take, toArray, uncons)
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (class TypedArray, buffer, fromArray, set, setTyped, whole)
import Data.ArrayBuffer.Typed as Typed
import Data.ArrayBuffer.Types (ArrayView, Uint32Array, Float32Array)
import Data.Float32 (fromNumber')
import Data.Float32 as F
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (ceil, floor, toNumber)
import Data.Int.Bits (complement, shr, (.&.))
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
import Deku.DOM (label)
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

maxCanvasWidth = 2048 :: Int
maxCanvasHeight = 1024 :: Int

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

counterDefs :: String
counterDefs =
  """
struct non_atomic_counter_struct {
  primary_write_index: u32,
  y: u32,
  z: u32,
  secondary_write_index: u32,
  total_readable: u32
}

struct counter_struct {
  primary_write_index: atomic<u32>,
  y: u32,
  z: u32,
  secondary_write_index: atomic<u32>,
  total_readable: u32
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

pixVol :: String
pixVol =
  """
struct pix_vol {
  pix: u32,
  vol: u32
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

colorSpheresBasedOnHitsShader :: String
colorSpheresBasedOnHitsShader =
  """
@group(0) @binding(0) var<storage, read> sphere_hit_buffer : array<u32>;
@group(1) @binding(0) var<storage, read> rendering_info: rendering_info_struct;
@group(1) @binding(1) var<storage, read> rays: array<ray>;
@group(1) @binding(2) var<storage, read> sphere_info: array<f32>;
@group(1) @binding(3) var<storage, read_write> color_buffer: array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.y * rendering_info.real_canvas_width + global_id.x;
  var ray = rays[ix];
  var sphere_hit = unpack2x16float(sphere_hit_buffer[ix]);
  var hit = sphere_hit.y;
  var sphere_ix = u32(sphere_hit.x);
  var was_i_hit = sphere_ix > 0u; 
  var my_color = vec3(1.f,0.f,0.f);
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
  var overshot_idx = global_id.x + (global_id.y * rendering_info.overshot_canvas_width);
  color_buffer[overshot_idx] = pack4x8unorm(vec4(my_color.b, my_color.g, my_color.r, 1.f));
}
  """

sphereHitsShader :: String
sphereHitsShader =
  """
const t_min = 0.0001;
const t_max = 10000.f;

@group(0) @binding(0) var<storage, read_write> sphere_hit_buffer : array<atomic<u32>>;
@group(1) @binding(0) var<storage, read> bvh_nodes: array<bvh_node>;
@group(1) @binding(1) var<storage, read> sphere_info: array<f32>;
@group(1) @binding(2) var<storage, read> rays: array<ray>;
@group(1) @binding(3) var<storage, read> sphere_data_buffer: array<pix_vol>;
@group(1) @binding(4) var<storage, read_write> total_readable : u32;
@group(2) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 64, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.x * 64u * 4u + global_id.y;
  if (ix >= total_readable) {
    return;
  }
  var sdb = sphere_data_buffer[ix];
  var ray = rays[sdb.pix];
  var node = bvh_nodes[sdb.vol];
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
  if (sphere_hit) {
    var i_plus_1 = node.left + 1u;
    // add 0.05 to avoid rounding error
    _ = atomicMin(&sphere_hit_buffer[sdb.pix], pack2x16float(vec2(f32(i_plus_1)+0.05, hit_t)));
  }
}
"""

bvhComputeShader0 :: { even :: Boolean } -> String
bvhComputeShader0 { even } = fold
  [ """
const t_min = 0.0001;
const t_max = 10000.f;

@group(0) @binding(0) var<storage, read> bvh_nodes : array<bvh_node>;
@group(0) @binding(1) var<storage, read> rays: array<ray>;
@group(0) @binding(2) var<storage, read_write> spheres : array<pix_vol>;
@group(0) @binding(3) var<storage, read_write> bvh0 : array<pix_vol>;
@group(0) @binding(4) var<storage, read_write> bvh1 : array<pix_vol>;
@group(1) @binding(0) var<storage, read_write> counter : counter_struct;
@group(2) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 64, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.x * 64u * 4u + global_id.y;
  if (ix >= counter.total_readable) {
    return;
  }
  var pv = """
  , if even then "bvh0" else "bvh1"
  , """[ix];
  var ray = rays[pv.pix];
  var node = bvh_nodes[pv.vol];
  var bbox: aabb;
  bvh_node_bounding_box(&node, &bbox);
  var i_was_hit = aabb_hit(&bbox, &ray, t_min, t_max);
  if (i_was_hit) {
    var bvnl = bvh_nodes[node.left];
    var bvnr = bvh_nodes[node.right];
    if (bvnl.is_sphere == 1u && bvnr.is_sphere == 1u) {
      var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
      var to_writeL: pix_vol;
      to_writeL.pix = pv.pix;
      to_writeL.vol = node.left;
      spheres[sphere_ix] = to_writeL;
      var to_writeR: pix_vol;
      to_writeR.pix = pv.pix;
      to_writeR.vol = node.right;
      spheres[sphere_ix + 1] = to_writeR;
    } else if (bvnl.is_sphere == 0u && bvnr.is_sphere == 0u) {
      bvh_node_bounding_box(&bvnl, &bbox);
      var i_was_hit_l = aabb_hit(&bbox, &ray, t_min, t_max);
      bvh_node_bounding_box(&bvnr, &bbox);
      var i_was_hit_r = aabb_hit(&bbox, &ray, t_min, t_max);
      ////////////
      if (i_was_hit_l && i_was_hit_r) {

        //@*@*@*@*@**@

          var bvnll = bvh_nodes[bvnl.left];
          var bvnlr = bvh_nodes[bvnl.right];
          var bvnrl = bvh_nodes[bvnr.left];
          var bvnrr = bvh_nodes[bvnr.right];
          if (bvnll.is_sphere == 1u && bvnlr.is_sphere == 1u && bvnrl.is_sphere == 1u && bvnrr.is_sphere == 1u) {
            var sphere_ix = atomicAdd(&counter.secondary_write_index, 4u);
            var to_writeLL: pix_vol;
            to_writeLL.pix = pv.pix;
            to_writeLL.vol = bvnl.left;
            spheres[sphere_ix] = to_writeLL;
            var to_writeLR: pix_vol;
            to_writeLR.pix = pv.pix;
            to_writeLR.vol = bvnl.right;
            spheres[sphere_ix + 1] = to_writeLR;
            var to_writeRL: pix_vol;
            to_writeRL.pix = pv.pix;
            to_writeRL.vol = bvnr.left;
            spheres[sphere_ix + 2] = to_writeRL;
            var to_writeRR: pix_vol;
            to_writeRR.pix = pv.pix;
            to_writeRR.vol = bvnr.right;
            spheres[sphere_ix + 3] = to_writeRR;
          } else if (bvnll.is_sphere == 0u && bvnlr.is_sphere == 0u && bvnrl.is_sphere == 0u && bvnrr.is_sphere == 0u) {
            var bvh_ix = atomicAdd(&counter.primary_write_index, 4u);
            var to_writeLL: pix_vol;
            to_writeLL.pix = pv.pix;
            to_writeLL.vol = bvnl.left;
            """
        , if even then "bvh1" else "bvh0"
        , """[bvh_ix] = to_writeLL;
            var to_writeLR: pix_vol;
            to_writeLR.pix = pv.pix;
            to_writeLR.vol = bvnl.right;
            """
        , if even then "bvh1" else "bvh0"
        , """[bvh_ix + 1] = to_writeLR;
            var to_writeRL: pix_vol;
            to_writeRL.pix = pv.pix;
            to_writeRL.vol = bvnr.left;
            """
        , if even then "bvh1" else "bvh0"
        , """[bvh_ix + 2] = to_writeRL;
            var to_writeRR: pix_vol;
            to_writeRR.pix = pv.pix;
            to_writeRR.vol = bvnr.right;
            """
        , if even then "bvh1" else "bvh0"
        , """[bvh_ix + 3] = to_writeRR;
          } else if (bvnll.is_sphere == 0u && bvnlr.is_sphere == 1u && bvnrl.is_sphere == 1u && bvnrr.is_sphere == 1u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 3u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLL;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              spheres[sphere_ix] = to_writeLR;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              spheres[sphere_ix + 1] = to_writeRL;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix + 2] = to_writeRR;
            } else if (bvnlr.is_sphere == 0u && bvnll.is_sphere == 1u && bvnrl.is_sphere == 1u && bvnrr.is_sphere == 1u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 3u);
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLR;
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              spheres[sphere_ix] = to_writeLL;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              spheres[sphere_ix + 1] = to_writeRL;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix + 2] = to_writeRR;
            } else if (bvnrl.is_sphere == 0u && bvnll.is_sphere == 1u && bvnlr.is_sphere == 1u && bvnrr.is_sphere == 1u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 3u);
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeRL;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeRR;
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              spheres[sphere_ix + 1] = to_writeLL;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              spheres[sphere_ix + 2] = to_writeLR;
            } else if (bvnrr.is_sphere == 0u && bvnll.is_sphere == 1u && bvnlr.is_sphere == 1u && bvnrl.is_sphere == 1u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 3u);
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeRR;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              spheres[sphere_ix] = to_writeRL;
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              spheres[sphere_ix + 1] = to_writeLL;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              spheres[sphere_ix + 2] = to_writeLR;
            } else if (bvnll.is_sphere == 1u && bvnlr.is_sphere == 0u && bvnrl.is_sphere == 0u && bvnrr.is_sphere == 0u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 3u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              spheres[sphere_ix] = to_writeLL;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLR;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeRL;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 2] = to_writeRR;
            } else if (bvnlr.is_sphere == 1u && bvnll.is_sphere == 0u && bvnrl.is_sphere == 0u && bvnrr.is_sphere == 0u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 3u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              spheres[sphere_ix] = to_writeLR;
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLL;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeRL;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 2] = to_writeRR;
            } else if (bvnrl.is_sphere == 1u && bvnll.is_sphere == 0u && bvnlr.is_sphere == 0u && bvnrr.is_sphere == 0u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 3u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              spheres[sphere_ix] = to_writeRL;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeRR;
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeLL;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 2] = to_writeLR;
            } else if (bvnrr.is_sphere == 1u && bvnll.is_sphere == 0u && bvnlr.is_sphere == 0u && bvnrl.is_sphere == 0u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 3u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeRR;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeRL;
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeLL;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 2] = to_writeLR;
            } else if (bvnll.is_sphere == 1u && bvnlr.is_sphere == 1u && bvnrl.is_sphere == 0u && bvnrr.is_sphere == 0u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeLL;
              spheres[sphere_ix + 1] = to_writeLR;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeRL;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeRR;
            } else if (bvnll.is_sphere == 0u && bvnlr.is_sphere == 0u && bvnrl.is_sphere == 1u && bvnrr.is_sphere == 1u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeRL;
              spheres[sphere_ix + 1] = to_writeRR;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLL;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeLR;
            }  else if (bvnll.is_sphere == 1u && bvnlr.is_sphere == 0u && bvnrl.is_sphere == 1u && bvnrr.is_sphere == 0u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeLL;
              spheres[sphere_ix + 1] = to_writeRL;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLR;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeRR;
            } else if (bvnll.is_sphere == 0u && bvnlr.is_sphere == 1u && bvnrl.is_sphere == 0u && bvnrr.is_sphere == 1u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeLR;
              spheres[sphere_ix + 1] = to_writeRR;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLL;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeRL;
            } else if (bvnll.is_sphere == 0u && bvnlr.is_sphere == 1u && bvnrl.is_sphere == 1u && bvnrr.is_sphere == 0u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeLR;
              spheres[sphere_ix + 1] = to_writeRL;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLL;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeRR;
            } else if (bvnll.is_sphere == 1u && bvnlr.is_sphere == 0u && bvnrl.is_sphere == 0u && bvnrr.is_sphere == 1u) {
              var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
              var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
              var to_writeLL: pix_vol;
              to_writeLL.pix = pv.pix;
              to_writeLL.vol = bvnl.left;
              var to_writeLR: pix_vol;
              to_writeLR.pix = pv.pix;
              to_writeLR.vol = bvnl.right;
              var to_writeRL: pix_vol;
              to_writeRL.pix = pv.pix;
              to_writeRL.vol = bvnr.left;
              var to_writeRR: pix_vol;
              to_writeRR.pix = pv.pix;
              to_writeRR.vol = bvnr.right;
              spheres[sphere_ix] = to_writeLL;
              spheres[sphere_ix + 1] = to_writeRR;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix] = to_writeLR;
              """
          , if even then "bvh1" else "bvh0"
          , """[bvh_ix + 1] = to_writeRL;
            } 

        //@*@*@*@*@**@

      } else if (i_was_hit_l) {
        var bvnll = bvh_nodes[bvnl.left];
        var bvnlr = bvh_nodes[bvnl.right];
        if (bvnll.is_sphere == 1u && bvnlr.is_sphere == 1u) {
          var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnl.left;
          spheres[sphere_ix] = to_writeL;
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnl.right;
          spheres[sphere_ix + 1] = to_writeR;
        } else if (bvnll.is_sphere == 0u && bvnlr.is_sphere == 0u) {
          var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnl.left;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix] = to_writeL;
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnl.right;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix + 1] = to_writeR;
        } else if (bvnll.is_sphere == 1u && bvnlr.is_sphere == 0u) {
          var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnl.left;
          spheres[sphere_ix] = to_writeL;
          var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnl.right;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix] = to_writeR;
        } else {
          var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnl.left;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix] = to_writeL;
          var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnl.right;
          spheres[sphere_ix] = to_writeR;
        }
      } else if (i_was_hit_r) {
        var bvnrl = bvh_nodes[bvnr.left];
        var bvnrr = bvh_nodes[bvnr.right];
        if (bvnrl.is_sphere == 1u && bvnrr.is_sphere == 1u) {
          var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnr.left;
          spheres[sphere_ix] = to_writeL;
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnr.right;
          spheres[sphere_ix + 1] = to_writeR;
        } else if (bvnrl.is_sphere == 0u && bvnrr.is_sphere == 0u) {
          var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnr.left;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix] = to_writeL;
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnr.right;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix + 1] = to_writeR;
        } else if (bvnrl.is_sphere == 1u && bvnrr.is_sphere == 0u) {
          var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnr.left;
          spheres[sphere_ix] = to_writeL;
          var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnr.right;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix] = to_writeR;
        } else {
          var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
          var to_writeL: pix_vol;
          to_writeL.pix = pv.pix;
          to_writeL.vol = bvnr.left;
          """
      , if even then "bvh1" else "bvh0"
      , """[bvh_ix] = to_writeL;
          var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
          var to_writeR: pix_vol;
          to_writeR.pix = pv.pix;
          to_writeR.vol = bvnr.right;
          spheres[sphere_ix] = to_writeR;
        }
      }
    } else if (bvnl.is_sphere == 1u && bvnr.is_sphere == 0u) {
      var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
      var to_writeL: pix_vol;
      to_writeL.pix = pv.pix;
      to_writeL.vol = node.left;
      spheres[sphere_ix] = to_writeL;
      var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
      var to_writeR: pix_vol;
      to_writeR.pix = pv.pix;
      to_writeR.vol = node.right;
      """
  , if even then "bvh1" else "bvh0"
  , """[bvh_ix] = to_writeR;
    } else {
      var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
      var to_writeL: pix_vol;
      to_writeL.pix = pv.pix;
      to_writeL.vol = node.left;
      """
  , if even then "bvh1" else "bvh0"
  , """[bvh_ix] = to_writeL;
      var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
      var to_writeR: pix_vol;
      to_writeR.pix = pv.pix;
      to_writeR.vol = node.right;
      spheres[sphere_ix] = to_writeR;
    }
  }
}
"""
  ]

bvhComputeShader :: { even :: Boolean } -> String
bvhComputeShader { even } = fold
  [ """
const t_min = 0.0001;
const t_max = 10000.f;

@group(0) @binding(0) var<storage, read> bvh_nodes : array<bvh_node>;
@group(0) @binding(1) var<storage, read> rays: array<ray>;
@group(0) @binding(2) var<storage, read_write> spheres : array<pix_vol>;
@group(0) @binding(3) var<storage, read_write> bvh0 : array<pix_vol>;
@group(0) @binding(4) var<storage, read_write> bvh1 : array<pix_vol>;
@group(1) @binding(0) var<storage, read_write> counter : counter_struct;
@group(2) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 64, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.x * 64u * 4u + global_id.y;
  if (ix >= counter.total_readable) {
    return;
  }
  var pv = """
  , if even then "bvh0" else "bvh1"
  , """[ix];
  var ray = rays[pv.pix];
  var node = bvh_nodes[pv.vol];
  var bbox: aabb;
  bvh_node_bounding_box(&node, &bbox);
  var i_was_hit = aabb_hit(&bbox, &ray, t_min, t_max);
  if (i_was_hit) {
    var bvnl = bvh_nodes[node.left];
    var bvnr = bvh_nodes[node.right];
    if (bvnl.is_sphere == 1u && bvnr.is_sphere == 1u) {
      var sphere_ix = atomicAdd(&counter.secondary_write_index, 2u);
      var to_writeL: pix_vol;
      to_writeL.pix = pv.pix;
      to_writeL.vol = node.left;
      spheres[sphere_ix] = to_writeL;
      var to_writeR: pix_vol;
      to_writeR.pix = pv.pix;
      to_writeR.vol = node.right;
      spheres[sphere_ix + 1] = to_writeR;
    } else if (bvnl.is_sphere == 0u && bvnr.is_sphere == 0u) {
      var bvh_ix = atomicAdd(&counter.primary_write_index, 2u);
      var to_writeL: pix_vol;
      to_writeL.pix = pv.pix;
      to_writeL.vol = node.left;
      """
  , if even then "bvh1" else "bvh0"
  , """[bvh_ix] = to_writeL;
      var to_writeR: pix_vol;
      to_writeR.pix = pv.pix;
      to_writeR.vol = node.right;
      """
  , if even then "bvh1" else "bvh0"
  , """[bvh_ix + 1] = to_writeR;
    } else if (bvnl.is_sphere == 1u && bvnr.is_sphere == 0u) {
      var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
      var to_writeL: pix_vol;
      to_writeL.pix = pv.pix;
      to_writeL.vol = node.left;
      spheres[sphere_ix] = to_writeL;
      var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
      var to_writeR: pix_vol;
      to_writeR.pix = pv.pix;
      to_writeR.vol = node.right;
      """
  , if even then "bvh1" else "bvh0"
  , """[bvh_ix] = to_writeR;
    } else {
      var bvh_ix = atomicAdd(&counter.primary_write_index, 1u);
      var to_writeL: pix_vol;
      to_writeL.pix = pv.pix;
      to_writeL.vol = node.left;
      """
  , if even then "bvh1" else "bvh0"
  , """[bvh_ix] = to_writeL;
      var sphere_ix = atomicAdd(&counter.secondary_write_index, 1u);
      var to_writeR: pix_vol;
      to_writeR.pix = pv.pix;
      to_writeR.vol = node.right;
      spheres[sphere_ix] = to_writeR;
    }
  }
}
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

unsignedLog2 :: Int -> Int
unsignedLog2 = go 0
  where
  go acc 0 = acc
  go acc i = go (acc + 1) (i `shr` 1)

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
      { size: 48 -- align(4) size(48)
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.storage
      }
    debugBuffer <- liftEffect $ createBuffer device $ x
      { size: 65536
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    noopBuffer <- liftEffect $ createBuffer device $ x
      { size: 65536
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    debugOutputBuffer <- liftEffect $ createBuffer device $ x
      { size: 65536
      , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.mapRead
      }
    seed <- liftEffect $ randomInt 42 42424242
    randos <- liftEffect $ sequence $ replicate 64 $ Sphere <$> ({ cx: _, cy: _, cz: _, radius: 0.125 } <$> (random <#> \n -> n * 16.0 - 8.0) <*> (random <#> \n -> n * 3.0 + 0.25) <*> (random <#> \n -> n * 16.0 - 8.0))
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
    let bvhDepth = ((unsignedLog2 nSpheres) / 2) + 1
    logShow {bvhDepth}
    sphereData :: Float32Array <- liftEffect $ fromArray rawSphereData
    sphereBuffer <- liftEffect $ createBufferF device sphereData GPUBufferUsage.storage
    bvhNodeBuffer <- liftEffect $ createBufferF device bvhNodeData GPUBufferUsage.storage
    indirectBufferEven <- liftEffect $ createBuffer device $ x
      { size: 4 * 8
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    indirectBufferOdd <- liftEffect $ createBuffer device $ x
      { size: 4 * 8
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    indirectBufferSphere <- liftEffect $ createBuffer device $ x
      { size: 4 * 8
      , usage: GPUBufferUsage.storage .|. GPUBufferUsage.indirect
      }
    sphereCountBuffer <- liftEffect $ createBuffer device $ x
      { size: 4
      , usage: GPUBufferUsage.storage
      }
    raysBuffer <- liftEffect $ createBuffer device $ x
      { size: maxCanvasWidth * maxCanvasHeight * 4 * 6 -- 6 thingees in a ray
      , usage: GPUBufferUsage.storage
      }
    xyVolEvenDataBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    xyVolOddDataBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    sphereDataBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.storage
      }
    sphereHitsDataBuffer <- liftEffect $ createBuffer device $ x
      { size: maxCanvasWidth * maxCanvasHeight * 4
      , usage: GPUBufferUsage.storage
      }
    wholeCanvasBuffer <- liftEffect $ createBuffer device $ x
      { size: deviceLimits.maxStorageBufferBindingSize
      , usage: GPUBufferUsage.copySrc .|. GPUBufferUsage.storage
      }
    -----
    -----
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
        , label: "zeroOutBuffer"
        }
    zeroOutBufferModule <- liftEffect $ createShaderModule device zeroOutBufferDesc
    let
      (zeroOutBufferStage :: GPUProgrammableStage) = x
        { "module": zeroOutBufferModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      bvhEvenDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , bvhComputeShader0 { even: true }
              ]
        , label: "bvhEven"
        }
    bvhEvenModule <- liftEffect $ createShaderModule device bvhEvenDesc
    let
      (bvhEvenStage :: GPUProgrammableStage) = x
        { "module": bvhEvenModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      bvhOddDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , bvhComputeShader0 { even: false }
              ]
        , label: "bvhOdd"
        }
    bvhOddModule <- liftEffect $ createShaderModule device bvhOddDesc
    let
      (bvhOddStage :: GPUProgrammableStage) = x
        { "module": bvhOddModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      resetXYVol0BufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> result_array : array<pix_vol>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.y * rendering_info.real_canvas_width + global_id.x;
  var pv: pix_vol;
  pv.pix = ix;
  pv.vol = rendering_info.n_bvh_nodes - 1;
  result_array[ix] = pv;
}"""
              ]
        , label: "resetXYVol0Buffer"
        }
    resetXYVol0BufferModule <- liftEffect $ createShaderModule device resetXYVol0BufferDesc
    let
      (resetXYVol0BufferStage :: GPUProgrammableStage) = x
        { "module": resetXYVol0BufferModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      resetRaysBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> rays : array<ray>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.y * rendering_info.real_canvas_width + global_id.x;
  var px = f32(global_id.x) / f32(rendering_info.real_canvas_width);
  var py = 1. - (f32(global_id.y) / f32(rendering_info.canvas_height));
  var ray: ray;
  ray.origin = origin;
  ray.direction = vec3(rendering_info.lower_left_x, rendering_info.lower_left_y, rendering_info.lower_left_z) + vec3(px * rendering_info.ambitus_x, py * rendering_info.ambitus_y, 0.0);
  rays[ix] = ray;
}"""
              ]
        , label: "resetRaysBuffer"
        }
    resetRaysBufferModule <- liftEffect $ createShaderModule device resetRaysBufferDesc
    let
      (resetRaysBufferStage :: GPUProgrammableStage) = x
        { "module": resetRaysBufferModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      resetSphereHitsBufferDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> spheres : array<u32>;
@compute @workgroup_size(16, 16, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ix = global_id.y * rendering_info.real_canvas_width + global_id.x;
  spheres[ix] = pack2x16float(vec2(0.f, 10000.f));
}"""
              ]
        , label: "resetSphereHitsBuffer"
        }
    resetSphereHitsBufferModule <- liftEffect $ createShaderModule device resetSphereHitsBufferDesc
    let
      (resetSphereHitsBufferStage :: GPUProgrammableStage) = x
        { "module": resetSphereHitsBufferModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      resetIndirectBuffersDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , """
// main
@group(0) @binding(0) var<storage, read> rendering_info : rendering_info_struct;
@group(1) @binding(0) var<storage, read_write> ib_0 : non_atomic_counter_struct;
@group(1) @binding(1) var<storage, read_write> ib_1 : non_atomic_counter_struct;
@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  ib_0.primary_write_index = 0u;
  ib_0.secondary_write_index = 0u;
  ib_0.y = 4u;
  ib_0.z = 1u;
  ib_0.total_readable = rendering_info.real_canvas_width * rendering_info.canvas_height;
  ib_1.primary_write_index = ((rendering_info.real_canvas_width * rendering_info.canvas_height) / (64 * 4)) + 1u;
  ib_1.secondary_write_index = 0u;
  ib_1.y = 4u;
  ib_1.z = 1u;
  ib_1.total_readable = rendering_info.real_canvas_width * rendering_info.canvas_height;
}"""
              ]
        , label: "resetIndirectBuffers"
        }
    resetIndirectBuffersModule <- liftEffect $ createShaderModule device resetIndirectBuffersDesc
    let
      (resetIndirectBuffersStage :: GPUProgrammableStage) = x
        { "module": resetIndirectBuffersModule
        , entryPoint: "main"
        }

    -----
    -----
    let
      shiftIndirectBuffersDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , """
// main
@group(1) @binding(0) var<storage, read_write> next_indirect : non_atomic_counter_struct;
@group(1) @binding(1) var<storage, read_write> next_counter : non_atomic_counter_struct;
@group(2) @binding(0) var<storage, read_write> debug : array<f32>;
@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  debug[0] = f32(next_indirect.total_readable);
  debug[1] = f32(next_indirect.primary_write_index);
  debug[2] = f32(next_indirect.y);
  debug[3] = f32(next_indirect.z);
  debug[4] = f32(next_indirect.secondary_write_index);
  debug[5] = f32(next_counter.total_readable);
  debug[6] = f32(next_counter.primary_write_index);
  debug[7] = f32(next_counter.y);
  debug[8] = f32(next_counter.z);
  debug[9] = f32(next_counter.secondary_write_index);
  if (next_indirect.primary_write_index > (134217728 / (4*2))) {
    debug[10] = 55.f;
  }
  if (next_counter.primary_write_index > (134217728 / (4*2))) {
    debug[11] = 83.f;
  }
  next_counter.total_readable = next_indirect.primary_write_index;
  next_counter.primary_write_index = 0u;
  next_counter.secondary_write_index = next_indirect.secondary_write_index;
  next_indirect.primary_write_index = (next_indirect.primary_write_index / (64 * 4)) + 1; // + 1 could have more finesse for edge case of exact division
}"""
              ]
        , label: "shiftIndirectBuffers"
        }
    shiftIndirectBuffersModule <- liftEffect $ createShaderModule device shiftIndirectBuffersDesc
    let
      (shiftIndirectBuffersStage :: GPUProgrammableStage) = x
        { "module": shiftIndirectBuffersModule
        , entryPoint: "main"
        }

    -----
    -----
    let
      assembleSphereHitDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , """
// main
@group(0) @binding(0) var<storage, read> ctr_1 : non_atomic_counter_struct;
@group(0) @binding(1) var<storage, read> ctr_2 : non_atomic_counter_struct;
@group(0) @binding(2) var<storage, read_write> sphere_hit_indirect : non_atomic_counter_struct;
@group(0) @binding(3) var<storage, read_write> sphere_count : u32;
@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  sphere_count = max(ctr_1.secondary_write_index, ctr_2.secondary_write_index);
  sphere_hit_indirect.primary_write_index = (sphere_count / (64 * 4)) + 1; // + 1 could have more finesse for edge case of exact division
  sphere_hit_indirect.y = 4u;
  sphere_hit_indirect.z = 1u;
}"""
              ]
        , label: "assembleSphereHit"
        }
    assembleSphereHitModule <- liftEffect $ createShaderModule device assembleSphereHitDesc
    let
      (assembleSphereHitStage :: GPUProgrammableStage) = x
        { "module": assembleSphereHitModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      sphereHitsDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , hitSphere
              , sphereHitsShader
              ]
        , label: "sphereHits"
        }
    sphereHitsModule <- liftEffect $ createShaderModule device sphereHitsDesc
    let
      (sphereHitsStage :: GPUProgrammableStage) = x
        { "module": sphereHitsModule
        , entryPoint: "main"
        }
    -----
    -----
    let
      colorSpheresBasedOnHitsDesc = x
        { code:
            intercalate "\n"
              [ inputData
              , pixVol
              , usefulConsts
              , bvhNode
              , counterDefs
              , ray
              , aabb
              , lerp
              , lerpv
              , pointAtParameter
              , hitRecord
              , makeHitRec
              , calcColors
              , colorSpheresBasedOnHitsShader
              ]
        , label: "colorSpheresBasedOnHits"
        }
    colorSpheresBasedOnHitsModule <- liftEffect $ createShaderModule device colorSpheresBasedOnHitsDesc
    let
      (colorSpheresBasedOnHitsStage :: GPUProgrammableStage) = x
        { "module": colorSpheresBasedOnHitsModule
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
    bvhComputationBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 3 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 4 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "bvhComputationBindGroupLayout"
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
    w2BindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              (0 .. 1) <#> \j -> gpuBindGroupLayoutEntry j GPUShaderStage.compute
                ( x { type: GPUBufferBindingType.storage }
                    :: GPUBufferBindingLayout
                )

          , label: "w2BindGroupLayout"
          }
    w3BindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              (0 .. 2) <#> \j -> gpuBindGroupLayoutEntry j GPUShaderStage.compute
                ( x { type: GPUBufferBindingType.storage }
                    :: GPUBufferBindingLayout
                )

          , label: "w3BindGroupLayout"
          }
    assembleSphereHitIndirectBufferBindGroupLayout <- liftEffect $ createBindGroupLayout device $ x
      { entries:
          [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
              ( x { type: GPUBufferBindingType.readOnlyStorage }
                  :: GPUBufferBindingLayout
              )
          , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
              ( x { type: GPUBufferBindingType.readOnlyStorage }
                  :: GPUBufferBindingLayout
              )
          , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
              ( x { type: GPUBufferBindingType.storage }
                  :: GPUBufferBindingLayout
              )
          , gpuBindGroupLayoutEntry 3 GPUShaderStage.compute
              ( x { type: GPUBufferBindingType.storage }
                  :: GPUBufferBindingLayout
              )
          ]
      , label: "assembleSphereHitIndirectBufferBindGroupLayout"
      }
    sphereHitsContextBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 3 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 4 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "sphereHitsContextBindGroupLayout"
          }
    colorSpheresBasedOnHitsBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 3 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          , label: "colorSpheresBasedOnHitsBindGroupLayout"
          }
    let debugBindGroupLayout = wBindGroupLayout
    ------------
    ------------
    ------------
    assembleSphereHitPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts:
          [ assembleSphereHitIndirectBufferBindGroupLayout ]
      , label: "assembleSphereHitPipelineLayout"
      }
    sphereHitsPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts:
          [ wBindGroupLayout, sphereHitsContextBindGroupLayout, debugBindGroupLayout ]
      , label: "sphereHitsPipelineLayout"
      }
    readOPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout ]
      , label: "readOPipelineLayout"
      }
    readODebugPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, wBindGroupLayout, debugBindGroupLayout ]
      , label: "readOPipelineLayout"
      }
    resetIndirectBuffersPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ readerBindGroupLayout, w2BindGroupLayout ]
      , label: "resetIndirectBuffersPipelineLayout"
      }
    shiftComputePipelineLayout <- liftEffect $ createPipelineLayout device $ x
      {
        -- bvhComputationBindGroupLayout not needed, but kept to avoid too much switching
        -- which theoretically speeds up the pipeline
        bindGroupLayouts: [ bvhComputationBindGroupLayout, w2BindGroupLayout, debugBindGroupLayout ]
      , label: "shiftComputePipelineLayout"
      }
    bvhPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts:
          [ bvhComputationBindGroupLayout, wBindGroupLayout, debugBindGroupLayout ]
      , label: "bvhPipelineLayout"
      }
    colorSpheresBasedOnHitsPipelineLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts:
          [ rBindGroupLayout, colorSpheresBasedOnHitsBindGroupLayout ]
      , label: "colorSpheresBasedOnHitsPipelineLayout"
      }
    ------------
    ------------
    debugBufferBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: debugBuffer } :: GPUBufferBinding)
          ]
      , label: "debugBufferBindGroup"
      }
    noopBufferBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: noopBuffer } :: GPUBufferBinding)
          ]
      , label: "noopBufferBindGroup"
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
    wSphereDataBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: sphereDataBuffer } :: GPUBufferBinding)
          ]
      , label: "wSphereDataBindGroup"
      }
    wXYVolEvenDataBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: xyVolEvenDataBuffer } :: GPUBufferBinding)
          ]
      , label: "wXYVolEvenDataBindGroup"
      }
    wRaysBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: raysBuffer } :: GPUBufferBinding)
          ]
      , label: "wRaysBindGroup"
      }
    wSphereHitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: sphereHitsDataBuffer } :: GPUBufferBinding)
          ]
      , label: "wSphereHitsBindGroup"
      }
    rSphereHitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: rBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: sphereHitsDataBuffer } :: GPUBufferBinding)
          ]
      , label: "rSphereHitsBindGroup"
      }
    assembleSphereHitIndirectBufferBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: assembleSphereHitIndirectBufferBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: indirectBufferEven } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: indirectBufferOdd } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: indirectBufferSphere } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: sphereCountBuffer } :: GPUBufferBinding)
          ]
      , label: "assembleSphereHitIndirectBufferBindGroup" 
      }
    indirectBuffersBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: w2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: indirectBufferEven } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: indirectBufferOdd } :: GPUBufferBinding)
          ]
      , label: "indirectBuffersBindGroup"
      }
    bvhComputationBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: bvhComputationBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: bvhNodeBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: raysBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: sphereDataBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: xyVolEvenDataBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 4
              (x { buffer: xyVolOddDataBuffer } :: GPUBufferBinding)
          ]
      , label: "mainDepthLevelEvenReadBindGroup"
      }
    shiftComputeBindGroupEven <- liftEffect $ createBindGroup device $ x
      { layout: w2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: indirectBufferEven } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: indirectBufferOdd } :: GPUBufferBinding)
          ]
      , label: "shiftComputeBindGroupEven"
      }
    shiftComputeBindGroupOdd <- liftEffect $ createBindGroup device $ x
      { layout: w2BindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: indirectBufferOdd } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: indirectBufferEven } :: GPUBufferBinding)
          ]
      , label: "shiftComputeBindGroupEven"
      }
    setComputeBindGroupEven <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: indirectBufferEven } :: GPUBufferBinding)
          ]
      , label: "setComputeBindGroupEven"
      }
    setComputeBindGroupOdd <- liftEffect $ createBindGroup device $ x
      { layout: wBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: indirectBufferOdd } :: GPUBufferBinding)
          ]
      , label: "setComputeBindGroupEven"
      }
    sphereHitsContextBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: sphereHitsContextBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: bvhNodeBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: sphereBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: raysBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: sphereDataBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 4
              (x { buffer: sphereCountBuffer } :: GPUBufferBinding)
          ]
      , label: "sphereHitsContextBindGroup"
      }
    colorSpheresBasedOnHitsBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: colorSpheresBasedOnHitsBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: canvasInfoBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: raysBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: sphereBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 3
              (x { buffer: wholeCanvasBuffer } :: GPUBufferBinding)
          ]
      , label: "colorSpheresBasedOnHitsBindGroup"
      }
    ------
    ------
    zeroOutBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: zeroOutBufferStage
      , label: "zeroOutBufferPipeline"
      }
    resetXYVol0BufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: resetXYVol0BufferStage
      , label: "resetXYVol0BufferPipeline"
      }
    resetIndirectBuffersPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: resetIndirectBuffersPipelineLayout
      , compute: resetIndirectBuffersStage
      , label: "resetIndirectBuffersStage"
      }
    resetRaysBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: resetRaysBufferStage
      , label: "resetRaysBufferPipeline"
      }
    resetSphereHitsBufferPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: readOPipelineLayout
      , compute: resetSphereHitsBufferStage
      , label: "resetSphereHitsBufferPipeline"
      }
    bvhEvenPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: bvhPipelineLayout
      , compute: bvhEvenStage
      , label: "bvhEvenPipeline"
      }
    bvhOddPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: bvhPipelineLayout
      , compute: bvhOddStage
      , label: "bvhEvenPipeline"
      }
    shiftComputePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: shiftComputePipelineLayout
      , compute: shiftIndirectBuffersStage
      , label: "shiftComputePipeline"
      }
    assembleSphereHitPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: assembleSphereHitPipelineLayout
      , compute: assembleSphereHitStage
      , label: "assembleSphereHitPipeline"
      }
    sphereHitsPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: sphereHitsPipelineLayout
      , compute: sphereHitsStage
      , label: "sphereHitsPipeline"
      }
    colorSpheresBasedOnHitsPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: colorSpheresBasedOnHitsPipelineLayout
      , compute: colorSpheresBasedOnHitsStage
      , label: "colorSpheresBasedOnHitsPipeline"
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
        writeBuffer queue canvasInfoBuffer 0 (fromUint32Array cinfo)
        computePassEncoder <- beginComputePass commandEncoder (x {})
        -------------------------
        -------------------------
        -- set reader for all computations
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          readerBindGroup
        -------------------------
        -------------------------
        ------- clears
        -- clear colors
        GPUComputePassEncoder.setPipeline computePassEncoder zeroOutBufferPipeline
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wCanvasBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -- clear spheres
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wSphereDataBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -- initialize bvh data 1 with end node
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wXYVolEvenDataBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder resetXYVol0BufferPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -- make rays
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wRaysBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder resetRaysBufferPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -- make rays
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          wSphereHitsBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder resetSphereHitsBufferPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder workgroupX workgroupY 1
        -- set all indirect buffers to 0 1 1
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          indirectBuffersBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder resetIndirectBuffersPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
        -------------------------
        -------------------------
        ------- bvh
        GPUComputePassEncoder.setBindGroup computePassEncoder 0 bvhComputationBindGroup
        foreachE (0 .. (bvhDepth - 1)) \depthLevel -> do
          GPUComputePassEncoder.setBindGroup computePassEncoder 1 (if (depthLevel `mod` 2 == 0) then setComputeBindGroupEven else setComputeBindGroupOdd)
          GPUComputePassEncoder.setBindGroup computePassEncoder 2 (if true then debugBufferBindGroup else noopBufferBindGroup)
          GPUComputePassEncoder.setPipeline
            computePassEncoder
            (if (depthLevel `mod` 2 == 0) then bvhEvenPipeline else bvhOddPipeline)
          GPUComputePassEncoder.dispatchWorkgroupsIndirect
            computePassEncoder
            -- huom! even and odd are flipped here!
            (if (depthLevel `mod` 2 == 0) then indirectBufferOdd else indirectBufferEven)
            0
          unless (depthLevel == (bvhDepth - 1)) do
            GPUComputePassEncoder.setBindGroup
              computePassEncoder
              1
              (if (depthLevel `mod` 2 == 0) then shiftComputeBindGroupEven else shiftComputeBindGroupOdd)
            GPUComputePassEncoder.setPipeline computePassEncoder
              shiftComputePipeline
            GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
        GPUComputePassEncoder.setBindGroup computePassEncoder 2 debugBufferBindGroup
        GPUComputePassEncoder.setBindGroup
          computePassEncoder
          0
          assembleSphereHitIndirectBufferBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder
          assembleSphereHitPipeline
        GPUComputePassEncoder.dispatchWorkgroupsXYZ computePassEncoder 1 1 1
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          wSphereHitsBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          sphereHitsContextBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder sphereHitsPipeline
        GPUComputePassEncoder.dispatchWorkgroupsIndirect computePassEncoder indirectBufferSphere 0
        -------- todo
        -------- this just flips w to r in index 0
        -------- but does it incur a cost?
        -------- if so, optimize
        GPUComputePassEncoder.setBindGroup computePassEncoder 0
          rSphereHitsBindGroup
        GPUComputePassEncoder.setBindGroup computePassEncoder 1
          colorSpheresBasedOnHitsBindGroup
        GPUComputePassEncoder.setPipeline computePassEncoder colorSpheresBasedOnHitsPipeline
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
        let debugCondition = true -- whichLoop == 100
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
        -- uncomment when debugged 
        -- window >>= void <<< requestAnimationFrame (f unit)

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