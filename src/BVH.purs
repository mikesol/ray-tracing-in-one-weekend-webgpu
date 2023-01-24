module BVH where

import Prelude

import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Writer (execWriter, tell)
import Data.Array (sortBy)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray, toNonEmpty)
import Data.Filterable (filter)
import Data.Foldable (maximum, minimum)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Number (abs, pow, sqrt)

newtype BVH = BVH
  { min_x :: Number
  , min_y :: Number
  , min_z :: Number
  , max_x :: Number
  , max_y :: Number
  , max_z :: Number
  , p0 :: Int
  , p1 :: Int
  , p2 :: Int
  , p3 :: Int
  , p4 :: Int
  , p5 :: Int
  , p6 :: Int
  , p7 :: Int
  , parent :: Maybe Int
  , ix :: Int
  , isSphere :: Boolean
  }

derive instance Newtype BVH _

data BVHRep
  = BVHR
      { min_x :: Number
      , min_y :: Number
      , min_z :: Number
      , max_x :: Number
      , max_y :: Number
      , max_z :: Number
      , p0 :: Maybe BVHRep
      , p1 :: Maybe BVHRep
      , p2 :: Maybe BVHRep
      , p3 :: Maybe BVHRep
      , p4 :: Maybe BVHRep
      , p5 :: Maybe BVHRep
      , p6 :: Maybe BVHRep
      , p7 :: Maybe BVHRep
      , isSphere :: Boolean
      }
  | SphereR { center :: { x :: Number, y :: Number, z :: Number }, radius :: Number }

newtype Sphere = Sphere
  { center :: { x :: Number, y :: Number, z :: Number }
  , radius :: Number
  }

derive instance Newtype Sphere _

-- sphereSurfaceIntersectsBox :: Number -> Number -> Number -> Number -> Number -> Number -> Sphere -> Boolean
-- sphereSurfaceIntersectsBox mnx mny mnz mxx mxy mxz (Sphere s) = do
--   let
--     x = s.center.x
--     y = s.center.y
--     z = s.center.z
--     r = s.radius
--   x + r >= mnx && x - r <= mxx && y + r >= mny && y - r <= mxy && z + r >= mnz && z - r <= mxz

sphereAndRectIntersect :: Number -> Number -> Number -> Number -> Number -> Number -> Sphere -> Boolean
sphereAndRectIntersect mnx mny mnz mxx mxy mxz (Sphere s) = o
  where
  c = s.center
  r = s.radius
  c' = { x: (mxx + mnx) / 2.0, y: (mxy + mny) / 2.0, z: (mxz + mnz) / 2.0 }
  dx = abs (c.x - c'.x)
  dy = abs (c.y - c'.y)
  dz = abs (c.z - c'.z)
  r' = { x: (mxx - mnx) / 2.0, y: (mxy - mny) / 2.0, z: (mxz - mnz) / 2.0 }
  x = r'.x + s.radius
  y = r'.y + s.radius
  z = r'.z + s.radius
  o = x - r <= dx && y - r <= dy && z - r <= dz

rectInSphere :: Number -> Number -> Number -> Number -> Number -> Number -> Sphere -> Boolean
rectInSphere mnx mny mnz mxx mxy mxz (Sphere s) = o
  where
  c = s.center
  distance = sqrt $ ((c.x - mnx) `pow` 2.0) + ((c.y - mny) `pow` 2.0) + ((c.z - mnz) `pow` 2.0)
  diagonal = sqrt $ ((mxx - mnx) `pow` 2.0) + ((mxy - mny) `pow` 2.0) + ((mxz - mnz) `pow` 2.0)
  o = distance + diagonal / 2.0 <= s.radius

surfaceIn :: Number -> Number -> Number -> Number -> Number -> Number -> Sphere -> Boolean
surfaceIn mnx mny mnz mxx mxy mxz s = sphereAndRectIntersect mnx mny mnz mxx mxy mxz s && not (rectInSphere mnx mny mnz mxx mxy mxz s)

toBVHRs :: NonEmptyArray Sphere -> BVHRep
toBVHRs i = out
  where

  out = go bottom bottom bottom top top top i
  go mnx mny mnz mxx mxy mxz a = o
    where
    (Sphere h) :| t = toNonEmpty i
    o = case t of
      [] -> SphereR h
      _ -> do
        let
          f v b z = fromMaybe v $ (if b then max else min) v <$> (if b then minimum else maximum) (map (\(Sphere s) -> z s.center + ((if b then -1.0 else 1.0) * s.radius)) a)
          mnx' = f mnx true _.x
          mny' = f mny true _.y
          mnz' = f mnz true _.z
          mxx' = f mxx false _.x
          mxy' = f mxy false _.y
          mxz' = f mxz false _.z
        BVHR
          { min_x: mnx'
          , min_y: mny'
          , min_z: mnz'
          , max_x: mxx'
          , max_y: mxy'
          , max_z: mxz'
          , p0: go mnx' mny' mnz' ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0) <$> fromArray (filter (surfaceIn mnx' mny' mnz' ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0)) $ toArray a)
          , p1: go ((mxx' + mnx') / 2.0) mny' mnz' mxx' ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0) <$> fromArray (filter (surfaceIn ((mxx' + mnx') / 2.0) mny' mnz' mxx' ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0)) $ toArray a)
          , p2: go mnx' ((mxy' + mny') / 2.0) mnz' ((mxx' + mnx') / 2.0) mxy' ((mxz' + mnz') / 2.0) <$> fromArray (filter (surfaceIn mnx' ((mxy' + mny') / 2.0) mnz' ((mxx' + mnx') / 2.0) mxy' ((mxz' + mnz') / 2.0)) $ toArray a)
          , p3: go ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) mnz' mxx' mxy' ((mxz' + mnz') / 2.0) <$> fromArray (filter (surfaceIn ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) mnz' mxx' mxy' ((mxz' + mnz') / 2.0)) $ toArray a)
          , p4: go mnx' mny' ((mxz' + mnz') / 2.0) ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) mxz' <$> fromArray (filter (surfaceIn mnx' mny' ((mxz' + mnz') / 2.0) ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) mxz') $ toArray a)
          , p5: go ((mxx' + mnx') / 2.0) mny' ((mxz' + mnz') / 2.0) mxx' ((mxy' + mny') / 2.0) mxz' <$> fromArray (filter (surfaceIn ((mxx' + mnx') / 2.0) mny' ((mxz' + mnz') / 2.0) mxx' ((mxy' + mny') / 2.0) mxz') $ toArray a)
          , p6: go mnx' ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0) ((mxx' + mnx') / 2.0) mxy' mxz' <$> fromArray (filter (surfaceIn mnx' ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0) ((mxx' + mnx') / 2.0) mxy' mxz') $ toArray a)
          , p7: go ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0) mxx' mxy' mxz' <$> fromArray (filter (surfaceIn ((mxx' + mnx') / 2.0) ((mxy' + mny') / 2.0) ((mxz' + mnz') / 2.0) mxx' mxy' mxz') $ toArray a)
          , isSphere: false
          }

toBVHArray :: NonEmptyArray Sphere -> Array BVH
toBVHArray i = sortBy (compare `on` (unwrap >>> _.ix)) $ execWriter $ evalStateT (s b) { parent: Nothing, ix: 0 }
  where
  b = toBVHRs i
  s x' = do
    { parent, ix } <- get
    case x' of
      BVHR x@{ p0, p1, p2, p3, p4, p5, p6, p7 } -> do
        p0' <- case p0 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        p1' <- case p1 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        p2' <- case p2 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        p3' <- case p3 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        p4' <- case p4 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        p5' <- case p5 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        p6' <- case p6 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        p7' <- case p7 of
          Nothing -> pure Nothing
          Just y -> do
            { ix: ix' } <- get
            put { parent: Just ix, ix: ix' + 1 }
            s y
            pure (Just $ ix' + 1)
        tell
          [ BVH
              { min_x: x.min_x
              , min_y: x.min_y
              , min_z: x.min_z
              , max_x: x.max_x
              , max_y: x.max_y
              , max_z: x.max_z
              , p0: fromMaybe (-1) p0'
              , p1: fromMaybe (-1) p1'
              , p2: fromMaybe (-1) p2'
              , p3: fromMaybe (-1) p3'
              , p4: fromMaybe (-1) p4'
              , p5: fromMaybe (-1) p5'
              , p6: fromMaybe (-1) p6'
              , p7: fromMaybe (-1) p7'
              , ix
              , parent: parent
              , isSphere: false
              }
          ]
        pure unit
      SphereR ss -> do
        { ix: ix' } <- get
        put { parent: Just ix, ix: ix' + 1 }
        tell
          [ BVH
              { min_x: ss.center.x - ss.radius
              , min_y: ss.center.y - ss.radius
              , min_z: ss.center.z - ss.radius
              , max_x: ss.center.x + ss.radius
              , max_y: ss.center.y + ss.radius
              , max_z: ss.center.z + ss.radius
              , p0: -1
              , p1: -1
              , p2: -1
              , p3: -1
              , p4: -1
              , p5: -1
              , p6: -1
              , p7: -1
              , parent: parent
              , ix
              , isSphere: true
              }
          ]
        pure unit