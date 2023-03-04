module Space.Search
  ( Strategy (..)
  , unitSearchStrategy
  , hedgehogSearchStrategy
  , twoDimensionalSearchStrategy

  -- * Searching a space using a strategy
  , searchSequential
  , searchSequential_
  , searchParallel
  ) where

import Control.Monad (guard)
import qualified Control.Parallel.Strategies as Parallel
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import Space.Random

-- | Defines how to search an ordered space.
--
-- There may be arbitrarily-many minimally-more-complicated points, but the
-- strategy must pick a finite set of them to be searched next.
--
-- Dually, there may be arbitrarily-many maximally-less-compliacted points, but
-- the strategy must pick a finite set of them to be searched during the
-- shrinking phase.
data Strategy state space = Strategy
  { complicate :: state -> space -> [(state, space)]
  , simplify :: state -> space -> [(state, space)]
  }

-- TODO would it be worthwhile to allow for the strategy to use randomness?
-- Probably.
-- At the very least, runStrategy could be given a pseudorandom "pruning
-- function" which randomly filters the `complicate` list.

-- | The only sensible way to search the single-element space.
unitSearchStrategy :: Strategy state ()
unitSearchStrategy = Strategy
  { complicate = const (const [])
  , simplify = const (const [])
  }

-- | Increase by 1 up to some limit, then simplify by 1. Doesn't need any
-- state.
--
hedgehogSearchStrategy :: Strategy state Natural
hedgehogSearchStrategy = Strategy
  { complicate = \st n -> if n > upperBound then [] else [(st, n+1)]
  , simplify = \st n -> if n == 0 then [] else [(st, n-1)]
  }
  -- Hedgehog uses 99 as the upper bound.
  where upperBound = 99

-- | Increase uniformly up to some limit, then decrease either component.
twoDimensionalSearchStrategy :: Natural -> Strategy state (Natural, Natural)
twoDimensionalSearchStrategy upperBound = Strategy
  { complicate = \st (n,m) -> do
      -- Take all 3 complications, keep only those which don't break the
      -- upper bound.
      (n', m') <- complications n m
      guard $ n' < upperBound
      guard $ m' < upperBound
      pure (st, (n', m'))
  , simplify = \st (n,m) -> (,) st <$>
     (if n == 0 && m == 0
      then []
      else if n == 0
      then [(n, m-1)]
      else if m == 0
      then [(n-1, m)]
      else [(n-1, m), (n, m-1)])
  }
  where
    complications n m =
      [ (n+1, m+1)
      , (n+1, m)
      , (n, m+1)
      ]

-- | Tries to find a minimally-complex point in the space such that the function
-- gives Just. It will search "up" through the complications according to the
-- strategy, breadth-first, until either
-- - there are no more complications, according to the strategy
-- - a failing case is found and a depth-first search begins "down" through the
--   simplifications to find a minimal example
-- 
-- This searches locally, without randomess, but is used to define search
-- drivers that do use randomness, like 'searchParallel'.
runStrategy :: forall state space failure .
               Strategy state space
            -> state
            -> space
            -> (space -> Maybe failure)
            -> Maybe (state, space, failure)
runStrategy searchDefn initialState startSpace p = fmap maybeSimplify failingCase

  where

    -- TODO
    -- would it be possible to rework this definition so that if it's called
    -- with a p that doesn't use its argument, then GHC will rewrite it to
    -- immediately shrink to the minimal space?
    --
    --     runStrategcy strat state space (const r)
    --   = case r of
    --       Nothing -> Nothing
    --       Just failure -> simplifyPhase state space failure
    --
    -- and then furthermore, if the simplification is const (const []) then
    -- it gets rewritten to Just (state, space, failure) ?
    --

    failingCase = complicatePhase (Seq.singleton (initialState, startSpace))

    maybeSimplify (state, space, failure) = simplifyPhase state space failure
 
    -- TODO make it possible to opt-in to parallelism in the complication and
    -- simplification phases? Worth it?

    -- Uses a sequence as a queue so as to search breadth-first on increasing
    -- complexity.
    complicatePhase :: Seq (state, space) -> Maybe (state, space, failure)
    complicatePhase bfsQueue = case bfsQueue of
      Seq.Empty -> Nothing
      (state, space) Seq.:<| bfsQueue -> case p space of
        -- Found a failing case. We're done with the complicate phase. The rest
        -- of the bfs search queue is discarded.
        Just failure -> Just (state, space, failure)
        -- No failing case here. Continue the search, including the complications
        -- of this space.
        Nothing -> complicatePhase (bfsQueue <> Seq.fromList (complicate searchDefn state space))

    -- Always called at a failing space with the search state that made it.
    simplifyPhase :: state -> space -> failure -> (state, space, failure)
    simplifyPhase state space failure =
      let simplifications = simplify searchDefn state space
      -- Take the first simplification which still fails and then recurse on
      -- that.
      -- Another reasonable option would be to take _every_ simplification which
      -- still fails and recurse on all of them, concatenating results, but it's
      -- better to have one minimal failing example than a huge amount of them.
       in takeFirstFailing (state, space, failure) simplifications

    takeFirstFailing :: (state, space, failure) -> [(state, space)] -> (state, space, failure)
    takeFirstFailing base [] = base
    takeFirstFailing base ((state, space):others) = case p space of
      -- This one didnt' fail, so we'll move on.
      Nothing -> takeFirstFailing base others
      -- Found one that did fail. We'll continue to simplify.
      Just failure -> simplifyPhase state space failure

-- | Uses 'searchSpace' at each seed.
searchSequential :: forall state space failure .
                    Strategy state space
                 -> state
                 -> space
                 -> (Seed -> space -> Maybe failure)
                 -> [Seed]
                 -> [(Seed, state, space, failure)]
searchSequential searchDefn initialState startSpace p = mapMaybe (pickFailing . searchOne)
  where
    -- TODO investigate whether we can re-work this so that, if p does not use
    -- either of its parameters, then it could be rewritten to evaluate p only
    -- once: float it out, and then case match on it
    --
    --   Nothing -> []
    --   Just failure -> fmap (\(seed, state, space) -> (seed, state, space, failure)) lst
    --
    -- Would this require inlining/rewriting on the search strategy as well? I
    -- believe it would.
    --
    searchOne :: Seed -> (Seed, Maybe (state, space, failure))
    searchOne seed = (seed, runStrategy searchDefn initialState startSpace (p seed))
    pickFailing :: (Seed, Maybe (state, space, failure)) -> Maybe (Seed, state, space, failure)
    pickFailing (seed, check) = fmap (\(state, space, failure) -> (seed, state, space, failure)) check

-- | Uses 'searchSpace' at each seed, but only giving the _first_ minimal
-- failing case (so not every Seed is necessarily used).
searchSequential_ :: forall state space failure .
                     Strategy state space
                  -> state
                  -> space
                  -> (Seed -> space -> Maybe failure)
                  -> [Seed]
                  -> Maybe (Seed, state, space, failure)
searchSequential_ searchDefn initialState startSpace p = headMaybe . searchSequential searchDefn initialState startSpace p
  where
    headMaybe [] = Nothing
    headMaybe (x:_) = Just x

-- | Will search at every seed, using parListChunk with a given chunk size.
-- The result is a list rather than a Maybe, because there's no sense only
-- taking one of the results if we're going to evaluate them all in parallel.
searchParallel :: forall state space failure .
                  Strategy state space
               -> state
               -> space
               -> (Seed -> space -> Maybe failure)
               -> Natural -- ^ Parallel chunk size.
               -> [Seed]
               -> [(Seed, state, space, failure)]
searchParallel searchDefn initialState startSpace p chunkSize seeds =
    mapMaybe pickFailing (Parallel.withStrategy strategyAll (fmap searchOne seeds))

  where

    -- TODO should we try to shrink in parallel too? Probably not worth it.

    pickFailing :: (Seed, Maybe (state, space, failure)) -> Maybe (Seed, state, space, failure)
    pickFailing (_seed, Nothing) = Nothing
    pickFailing (seed, Just (state, space, failure)) = Just (seed, state, space, failure)

    searchOne :: Seed -> (Seed, Maybe (state, space, failure))
    searchOne seed = (seed, runStrategy searchDefn initialState startSpace (p seed))

    -- Evaluation strategy for each element? We only need to figure out whether
    -- it's Just or Nothing. No need to necessarily do the work of evalauting
    -- the entire space or search state (although they will probably be forced
    -- anyway).
    strategyOne :: Parallel.Strategy (Seed, Maybe (state, space, failure))
    strategyOne (seed, mresult) = do
      mresult' <- Parallel.rseq mresult
      pure (seed, mresult')

    strategyAll :: Parallel.Strategy [(Seed, Maybe (state, space, failure))]
    strategyAll = Parallel.parListChunk (fromIntegral chunkSize) strategyOne

