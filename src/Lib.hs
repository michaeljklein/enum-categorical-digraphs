{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Lib
-- Copyright   :  (C) 2018 Michael J. Klein
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
--
-- So we have a directed graph (digraph) that we want to represent a category.
--
-- - All vertices must have at least one self-loop (id)
-- - Composition of arcs is an arc, i.e. @e(a,b) && e(b, c) => e(a, c)@
--
-- 0 is no arc, 1 is arc, 2 is anti-arc
--
-- @forall i. e(i, i)@
--
-- Final Result:
--  @allDGraphs :: Int -> BTree (IntMap Neighborhood, Matrix Int)@
--  Binary tree of choices for @uncons@: either arc or anti arc
--
----------------------------------------------------------------------------
module Lib where

import Data.Maybe
import GHC.Generics
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Control.Monad.State
import Control.Monad.Except

-- | Triangular numbers
triNum :: Int -> Int
triNum n = (n * (n + 1)) `div` 2


-- | A digraph arc
data Arc = Arc { arcHead :: !Int, arcTail :: !Int } deriving (Eq, Ord, Read, Show, Generic)

-- | The neighborhood of a vertex
data Neighborhood = Neighborhood { inAdjacent :: IntSet, outAdjacent :: IntSet } deriving (Eq, Ord, Show, Generic)

instance Semigroup Neighborhood where
  ~(Neighborhood inX outX) <> ~(Neighborhood inY outY) = Neighborhood (inX <> inY) (outX <> outY)

instance Monoid Neighborhood where
  mempty = Neighborhood IS.empty IS.empty

-- | `IS.\\` set-wise
(\\\) :: Neighborhood -> Neighborhood -> Neighborhood
~(Neighborhood inX outX) \\\ ~(Neighborhood inY outY) = Neighborhood (inX IS.\\ inY) (outX IS.\\ outY)

-- | Just the given vertex in the `outAdjacent`
pureIn :: Int -> Neighborhood
pureIn v = Neighborhood IS.empty (IS.singleton v)

-- | Just the given vertex in the `inAdjacent`
pureOut :: Int -> Neighborhood
pureOut v = Neighborhood (IS.singleton v) IS.empty

-- | Add to `inAdjacent`
addIn :: Int -> Neighborhood -> Neighborhood
addIn v Neighborhood{..} = Neighborhood { inAdjacent = IS.insert v inAdjacent, ..}

-- | Add to `outAdjacent`
addOut :: Int -> Neighborhood -> Neighborhood
addOut v Neighborhood{..} = Neighborhood { outAdjacent = IS.insert v outAdjacent, ..}

-- | Add an `Arc` to an `IntMap` of `Neighborhood`s
consNeighborhoods :: Arc -> IntMap Neighborhood -> IntMap Neighborhood
consNeighborhoods ~(Arc from to) = IM.adjust (addOut to) from . IM.adjust (addIn from) to

-- | Partially determined graph state
type PdState = StateT PdGraph Maybe

runPdState :: Int -> PdState a -> Maybe a
runPdState = flip evalStateT . initPdGraph

getPdState :: Int -> PdState a -> Maybe PdGraph
getPdState = flip execStateT . initPdGraph

-- | Partially determined dighraphs
data PdGraph = PdGraph
  { numVertices :: !Int -- ^ The number of vertices
  , adjMatrix :: !(Matrix Int) -- ^ Diagonal is 1, 0 -> undetermined, 1 -> arc, 2 -> anti-arc
  , undeterminedArcs :: Set Arc
  , neighborhoods :: IntMap Neighborhood -- ^ The neighborhoods of each vertex
  } deriving (Eq, Show, Generic)

-- | Initial `PdGraph` with the given number of vertices
initPdGraph :: Int -> PdGraph
initPdGraph numVertices'
  | numVertices' <= 0 =
    error $ unwords ["initPdGraph:", show numVertices', "<= 0"]
initPdGraph numVertices' =
  PdGraph
    { numVertices = numVertices'
    , adjMatrix = M.identity numVertices'
    , undeterminedArcs =
        S.fromDistinctAscList
          [ Arc i j
          | i <- [1 .. numVertices']
          , j <- [1 .. numVertices']
          , i /= j
          ]
    , neighborhoods = IM.fromDistinctAscList $ fmap (, mempty) [1 .. numVertices']
    }

-- | Get the next undetermined `Arc` or `Nothing`
unconsUndetermined :: PdState (Maybe Arc)
unconsUndetermined = do
  PdGraph {..} <- get
  case S.minView undeterminedArcs of
    Nothing -> return Nothing
    ~(Just (undeterminedArc, undeterminedArcs')) -> do
      put $ PdGraph {undeterminedArcs = undeterminedArcs', ..}
      return $ Just undeterminedArc

-- | Both possible next determinations
bothChoices :: PdState (PdGraph, PdGraph)
bothChoices = do
  currentGraph@PdGraph{..} <- get
  Arc from' to' <- unconsUndetermined >>= lift
  x <- lift $ chooseArc from' to' `execStateT` currentGraph
  y <- lift $ chooseAnti from' to' `execStateT` currentGraph
  return (x, y)

-- | Make a list of choices
makeChoices :: [Bool] -> PdState ()
makeChoices [] = return ()
makeChoices ~(x:xs) = do
  mArc <- unconsUndetermined
  case mArc of
    Nothing -> return ()
    ~(Just (Arc from' to')) -> do
      if x
        then chooseArc from' to'
        else chooseAnti from' to'
      makeChoices xs

-- | Convert to list of bits (most significant bit last)
toBin :: Int -> [Bool]
toBin 0 = [False]
toBin 1 = [True]
toBin x = if even x
             then False : toBin (x `div` 2)
             else True  : toBin (x `div` 2)

-- | Pad with a value up to the given length
padWith :: Int -> a -> [a] -> [a]
padWith 0 _ [] = []
padWith n _ [] | n < 0 = error "n < 0"
padWith n x [] = loop n
  where
    loop 0 = []
    loop i = x : loop (i - 1)
padWith n x ~(y:ys) = y : padWith (n - 1) x ys

-- | Indexed choices based on `toBin`
indexedChoices :: Int -> Int -> PdState ()
indexedChoices n = makeChoices . padWith n False . toBin

-- | All choices for a `PdGraph`
allChoices :: PdState [PdGraph]
allChoices = do
  currentGraph@PdGraph {..} <- get
  let m = triNum numVertices + numVertices
  return
    [ x | i <- [0 .. 2 ^ m], x <- maybeToList $ indexedChoices m i `execStateT` currentGraph]

-- | All choices for a `PdGraph` with the given number of vertices
allChoicesN :: Int -> [PdGraph]
allChoicesN = fromMaybe [] . evalStateT allChoices . initPdGraph


-- | Assumes `Arc` gotten from `unconsUndetermined`
determineArc :: Int -> Int -> Int -> PdState ()
determineArc !x from' to' = modify $ \PdGraph{..} -> PdGraph
  { adjMatrix = M.setElem x (from', to') adjMatrix
  , neighborhoods = Arc from' to' `consNeighborhoods` neighborhoods
  , ..
  }

-- | Choose for an undetermined `Arc` to be an arc in the final digraph
chooseArc :: Int -> Int -> PdState ()
chooseArc from' to' = do
  determineArc 1 from' to'
  propagateChoseArc from' to'

-- | Choose for an undetermined `Arc` to _not_ be an arc in the final digraph
chooseAnti :: Int -> Int -> PdState ()
chooseAnti from' to' = do
  determineArc 2 from' to'
  propagateChoseAnti from' to'


-- Three ways of interpreting a pair e(a,b) in the "master formula":
-- not e(a, b) || not e(b, c) || e(a, c)


-- e(a, b) => not e(b, i) || e(a, i)
-- e(a, b) => (e(b, i) => e(a, i))
-- e(a, b) => e(b, i) => e(a, i)

-- not e(a, b) => True


-- not e(a, b) || not e(i, a) || e(i, b)

-- e(a, b) => not e(i, a) || e (i, b)
-- e(a, b) => e(i, a) => e (i, b)

-- not e(a, b) => True


-- e(a, b) || not e(a, i) || not e(i, b)

-- e(a, b) => True

-- not e(a, b) => not e(a, i) || not e(i, b)
-- not e(a, b) => not (e(a, i) && e(i, b))
-- not e(a, b) => e(a, i) => not e(i, b)
-- not e(a, b) => e(i, b) => not e(a, i)

-- (a => b) == (not a || b)


-- =>


-- e(a, b) => (e(b, i) => e(a, i)) && (e(i, a) => e(i, b))

-- not e(a, b) => e(i, b) => not e(a, i)

-- propagateChoseArc :: Arc -> PdState ()
-- propagateChoseArc = liftM2 (>>) propagateChoseArcL propagateChoseArcR

-- -- e(b, i) => e(a, i)
-- -- not e(a, i) => not e(b, i)
-- propagateChoseArcL :: Arc -> PdState ()
-- propagateChoseArcL ~(Arc from' to') = do
--   PdGraph {..} <- get
--   let e ~(x, y) = M.getElem x y adjMatrix
--   forM_ [i | i <- [1 .. numVertices], i /= from', i /= to'] $ \i -> do
--     case e (from', i) of
--       0 ->
--         case e (to', i) of
--           0 -> return ()
--           1 -> chooseArc $ Arc from' i
--           _ -> return ()
--       1 -> return ()
--       _ ->
--         case e (to', i) of
--           0 -> chooseAnti $ Arc to' i
--           1 -> error "contradiction: not e(from', i) => not e(to', i)"
--           _ -> return ()

-- -- e(i, a) => e(i, b)
-- -- not e(i, b) => not e(i, a)
-- propagateChoseArcR :: Arc -> PdState ()
-- propagateChoseArcR ~(Arc from' to') = do
--   PdGraph {..} <- get
--   let e ~(x, y) = M.getElem x y adjMatrix
--   forM_ [i | i <- [1 .. numVertices], i /= from', i /= to'] $ \i -> do
--     case e (i, from') of
--       0 -> return ()
--       1 ->
--         case e (i, to') of
--           0 -> chooseArc $ Arc i to'
--           1 -> return ()
--           _ -> error "contradiction: e(i, from') => e(i, to')"
--       _ -> return ()



-- Ok, now again, but using adjacents:

-- e(b, i) => e(a, i)
-- not e(i, b) => not e(i, a)

-- not e(a, i) => not e(b, i)
-- e(i, a) => e(i, b)

arcCase :: Int -> Int -> PdState a -> PdState a -> PdState a -> PdState a
arcCase !from' !to' zero' one' two' = do
  PdGraph{..} <- get
  case M.getElem from' to' adjMatrix of
    0 -> zero'
    1 -> one'
    _ -> two'

propagateChoseArc :: Int -> Int -> PdState ()
propagateChoseArc !from' !to' = do
  PdGraph{..} <- get
  let arcSet = [from', to'] :: IntSet
      adjFrom = neighborhoods IM.! from' \\\ join Neighborhood arcSet
      adjTo = neighborhoods IM.! to' \\\ join Neighborhood arcSet
  forM_IS (inAdjacent adjFrom) $ \i -> do
    -- e(i, a) => e(i, b)
    -- not e(i, b) => not e(i, a)
    arcCase i from'
      (arcCase i to' (return ()) (return ()) (chooseAnti i from')) -- _ =>
      (arcCase i to' (chooseArc i to') (return ()) (throwError ())) -- e(i, a) =>
      (return ()) -- not e(i, a) =>

  forM_IS (outAdjacent adjFrom) $ \i -> do
    -- e(b, i) => e(a, i)
    -- not e(a, i) => not e(b, i)
    arcCase from' i
      (arcCase to' i (return ()) (chooseArc from' i) (return ()))
      (return ())
      (arcCase to' i (chooseAnti to' i) (throwError ()) (return ()))

  forM_IS (inAdjacent adjTo) $ \i -> do
    -- e(i, a) => e(i, b)
    -- not e(i, b) => not e(i, a)
    arcCase i to'
      (arcCase i from' (return ()) (chooseArc i to') (return ()))
      (return ())
      (arcCase i from' (chooseAnti i from') (throwError ()) (return ()))

  forM_IS (outAdjacent adjTo) $ \i -> do
    -- e(b, i) => e(a, i)
    -- not e(a, i) => not e(b, i)
    arcCase to' i
      (arcCase from' i (return ()) (return ()) (chooseAnti to' i))
      (arcCase from' i (chooseArc from' i) (return ()) (throwError ()))
      (return ())


-- not e(a, b) => e(i, b) => not e(a, i)
propagateChoseAnti :: Int -> Int -> PdState ()
propagateChoseAnti !from' !to' = do
  PdGraph{..} <- get
  let arcSet = [from', to'] :: IntSet
      adjTo = inAdjacent (neighborhoods IM.! to') IS.\\ arcSet
  forM_IS adjTo $ \i -> do
    -- e(i, b) => not e(a, i)
    -- e(a, i) => not e(i, b)
    arcCase i to'
      (arcCase from' i (return ()) (chooseAnti i to') (return ()))
      (arcCase from' i (chooseAnti from' i) (throwError ()) (return ()))
      (return ())


-- | An implementation of `forM_` using `IS.foldl'`
forM_IS :: Applicative f => IntSet -> (Int -> f ()) -> f ()
forM_IS = flip mapM_IS

-- | An implementation of `mapM_` using `IS.foldl'`
mapM_IS :: Applicative f => (Int -> f ()) -> IntSet -> f ()
mapM_IS f = IS.foldl' (\memo k -> memo *> f k) (pure ())

-- propagateChoseArcL :: Arc -> PdState ()
-- propagateChoseArcL ~(Arc from' to') = do
--   PdGraph {..} <- get
--   let arcSet = [from', to'] :: IntSet
--       adjFrom = outAdjacent $ neighborhoods IM.! from' -- we know they're non-empty since we just added to them when we added the Arc
--       adjTo = inAdjacent $ neighborhoods IM.! to' -- we know they're non-empty since we just added to them when we added the Arc
--   forM_ (adjFrom IM.\\ arcSet) $ \i -> do
--     case M.getElem from' i adjMatrix of
--       2 -> case M.getElem to' i adjMatrix of
--              0 -> chooseAnti $ Arc to' i
--              1 -> undefined
--              _ -> return ()
--       _ -> return ()
--   forM_ (adjTo IM.\\ arcSet) $ \i -> do
--     case M.getElem to' i adjMatrix of
--       1 -> case M.getElem from' i adjMatrix of
--              0 -> chooseArc $ Arc from' i
--              1 -> return ()
--              _ -> undefined
--       _ -> return ()



-- Finally, we'll need to connect the propagation of the choice to the choice (to ensure that we fully propagate all choices)


-- identity :: Num a => Int -> Matrix a

-- initUndeterminedArcs :: Int -> Set (Int, Int)
-- initUndeterminedArcs n = [(i, j) | i <- init [0..n], j <- init [0..n], i /= j]

-- undeterminedArcs :: (Eq a, Num a) => Matrix a -> Set (Int, Int)
-- undeterminedArcs xs = S.fromDistinctAscList $ assertIsAsc [(i, j) | (i, ys) <- zip [0..] $ zip [0..] <$> M.toLists xs, (j, 0) <- ys]

-- | Throws an error if not ascending
assertIsAsc :: Ord a => [a] -> [a]
assertIsAsc [] = []
assertIsAsc [x] = [x]
assertIsAsc ~(x:y:zs) = if x < y
                           then x : assertIsAsc (y:zs)
                           else error "not (x < y)"


-- -- not e(a, b) || not e(b, c) || e(a, c)

-- applyRules :: (Eq a, Num a, Monad m) => Int -> Int -> StateT (Matrix a) m ()
-- applyRules i j = do
--   xs <- get
--   case M.getElem i j xs of
--     0 -> _

-- applyRule :: (Eq a, Num a) => Int -> Int -> Matrix a -> Matrix a
-- applyRule i j xs = case M.unsafeGet i j xs of
--                      0 -> xs
--                      1 -> (`execStateT` xs) $ forM (filter
--   not e(j, k) || e(i, k)
--   not e(k, i) || e(k, j)
--   True
--                      2 -> (`execStateT` xs) $ forM_ (filter (liftM2 (&&) (/= i) (/= j)) [0..n-1]) $ \k ->
--                        case M.unsafeGet i k xs of
--                          1 -> case M.unsafeGet k j of
--                                 0 -> modify $ M.unsafeSet 2 (k, j)
--                                 1 -> error "applyRule impossible case"
--                                 2 -> return ()
--                          _ -> return ()
--   True
--   True
--   not e(i, k) || not e(k, j)
--     not e(i, i) || not e(i, j) => True
--     not e(i, j) || not e(j, j) => True

--   So we check the row e(i, k) and the column e(k, j)
--   If one is an arc (1) then the other must be an anti-arc (2)
--   If one is an anti-arc: True

