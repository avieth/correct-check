module Space.Search
  ( Strategy (..)
  , unitSearchStrategy
  , hedgehogSearchStrategy
  , twoDimensionalSearchStrategy

  , pickFailing
  , pickFirstFailing
  , complicateBreadthFirst
  , simplifyDepthFirst
  , runStrategy

  -- * Searching a space using a strategy
  , searchSequential
  , searchSequential_
  , searchParallel
  ) where

import Control.Monad (guard)
import qualified Control.Parallel.Strategies as Parallel
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe (mapMaybe)
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
-- Can we make a function which will 

{-# RULES
"mapMaybeConstNothing1" mapMaybe (\_ -> Nothing) = const []
"mapMaybeConstNothing2" forall xs . mapMaybe (\_ -> Nothing) xs = []

"mapMaybeConstJust1" forall r . mapMaybe (\_ -> Just r) = fmap (const r)
"mapMaybeConstJust2" forall r xs . mapMaybe (\_ -> Just r) xs = fmap (const r) xs

"mapMaybeConst" forall r xs . mapMaybe (\_ -> r) xs = maybe [] (flip fmap xs . const) r

"pickFailingConstNothing1" pickFailing (\_ -> Nothing) = const Nothing
"pickFailingConstNothing2" forall x . pickFailing (\_ -> Nothing) x = Nothing

"pickFailingConstJust1" forall r . pickFailing (\_ -> Just r) = Just . makeTriple r
"pickFailingConstJust2" forall r x . pickFailing (\_ -> Just r) x = Just (makeTriple r x)

"pickFirstFailingConstNothing1" pickFirstFailing (\_ -> Nothing) = const Nothing
"pickFirstFailingConstNothing2" forall x . pickFirstFailing (\_ -> Nothing) x = Nothing

"pickFirstFailingConstJust" forall r x xs . pickFirstFailing (\_ -> Just r) (x:xs) = Just (makeTriple r x)
#-}

-- No rule for pickFirstFailing (\_ -> Just r) because we ought to be able to
-- let that inline to headOfList . mapMaybe (pickFailing (\_ -> Just r)) and
-- then rewrite on that.

-- There are no rewrite rules for unfoldr in its defining module.
-- It does get INLINE though.

-- The idea would be that runStrategy is inlined and then fires the
-- mapMaybeConst rule.
-- But to get that, we need to have the "compliacte-then-simplify" part also
-- inline/simplify.
-- Could we do that by having the BFS complicate phase expressed as an unfoldr?
{-# INLINE runStrategy #-}
runStrategy :: forall state space failure .
                Strategy state space
             -> state
             -> space
             -> (space -> Maybe failure)
             -> Maybe (state, space, failure)
runStrategy strategy state space p = fmap (simplifyDepthFirst (simplify strategy) p) failingPoint
  where
    -- Always apply it to the given state and space directly, rather than by
    -- way of a list expression. Instead, complicateBreadthFirst gives the
    -- search strictly after the initial state. This helps with simplification,
    -- because
    --   pickFirstFailing (const r) (x:_)
    -- can always be rewritten.
    failingPoint = pickFirstFailing p ((state, space) : complicateBreadthFirst (complicate strategy) state space)

-- INLINE phase 2 or later, so that we give the RULES a chance to fire.
{-# INLINE [2] pickFailing #-}
-- |
-- Need to have this simplify/rewrite as follows
-- 
--     pickFailing (const Nothing) xs
--   = Nothing
--
pickFailing :: (space -> Maybe failure) -> (state, space) -> Maybe (state, space, failure)
pickFailing p x = do
  y <- p (snd x)
  pure (fst x, snd x, y)

-- INLINE phase 2 or later, so that we give the RULES a chance to fire.
{-# INLINE [2] pickFirstFailing #-}
-- The rule pickFailingConstNothing will be applicable to an inlining of this,
-- in case p = const Nothing. That would leave us with
--
--   headOfList . mapMaybe (const Nothing)
--
-- which can be re-written to
--
--   headOfList . const []
--
-- which hopefully can be inlined/rewritten to
--
--   const Nothing
--
pickFirstFailing :: (space -> Maybe failure) -> [(state, space)] -> Maybe (state, space, failure)
pickFirstFailing p = headOfList . mapMaybe (pickFailing p)

{-# INLINE complicateBreadthFirst #-}
-- | Breadth-first search of complications according to the strategy. The
-- given state and space values are the initial search point, and will not be
-- included in the resulting list.
complicateBreadthFirst :: (state -> space -> [(state, space)]) -> state -> space -> [(state, space)]
complicateBreadthFirst complicate state space = unfoldr 
  (\bfsQueue -> case bfsQueue of
    Seq.Empty -> Nothing
    (state, space) Seq.:<| bfsQueue -> Just ((state, space), bfsQueue <> Seq.fromList (complicate state space))
  )
  (Seq.fromList (complicate state space))

{-# INLINE simplifyDepthFirst #-}
-- Want GHC to be able to rewrite like so
--
--     simplifyDepthFirst strategy (const r) x
--   = fmap (\failure -> (fst x, snd x, failure)) r
--
-- Can we get that without writing a new rule?
--
-- More subtle that that, though: if it's const Nothing then it should indeed
-- simplify like that, but if it's const (Just t) then the strategy does has to
-- run the simplifier through. But if the simplifier is const (const []) then
-- it should fuse more.
--
-- So what can we do? Depth-first seems right, but not the usual meaning of
-- depth-first as in search, just depth-first single-path according to the
-- function p.
--
-- Can we express this as a mapMaybe over all simplifications? I don't think so,
-- because unlike complications, the simplification search is directed by the
-- predicate.
-- Really what we want is that whenever the simplify function is
--   const (const [])
-- then this function simplifies to id, no matter the predicate
--   simplifyDepthFirst (const (const [])) p x = x
-- because in this case, p will never be applied!
--
-- So, as an unfoldr? Take all of the simplifcations and pick the first that
-- fails.
--
--
-- In case we have a const Just or const Nothing test, simplifications and
-- complications will be skipped. This is all we need for unit tests to simplify
-- well.
-- Furthermore, if simplification is const (const []) then we would
-- expect
simplifyDepthFirst :: (state -> space -> [(state, space)])
                   -> (space -> Maybe failure)
                   -> (state, space, failure)
                   -> (state, space, failure)
simplifyDepthFirst simplify p = goSimplify
  where
    goSimplify (state, space, failure) = case pickFirstFailing p (simplify state space) of
      Nothing -> (state, space, failure)
      Just simplified -> goSimplify simplified

{-# INLINE searchSequential #-}
-- | Uses 'searchSpace' at each seed.
searchSequential :: forall state space failure .
                    Strategy state space
                 -> state
                 -> space
                 -> (Seed -> space -> Maybe failure)
                 -> NonEmpty Seed
                 -> [(Seed, state, space, failure)]
searchSequential strategy initialState startSpace p = mapMaybe (pickFailing . searchOne) . NE.toList
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
    searchOne seed = (seed, runStrategy strategy initialState startSpace (p seed))
    pickFailing :: (Seed, Maybe (state, space, failure)) -> Maybe (Seed, state, space, failure)
    pickFailing (seed, check) = fmap (\(state, space, failure) -> (seed, state, space, failure)) check

{-# INLINE searchSequential_ #-}
-- | Uses 'searchSpace' at each seed, but only giving the _first_ minimal
-- failing case (so not every Seed is necessarily used).
searchSequential_ :: forall state space failure .
                     Strategy state space
                  -> state
                  -> space
                  -> (Seed -> space -> Maybe failure)
                  -> NonEmpty Seed
                  -> Maybe (Seed, state, space, failure)
searchSequential_ strategy initialState startSpace p = headOfList . searchSequential strategy initialState startSpace p

{-# INLINE searchParallel #-}
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

{-# INLINE makeTriple #-}
makeTriple :: c -> (a, b) -> (a, b, c)
makeTriple c (a, b) = (a, b, c)

{-# INLINE headOfList #-}
headOfList :: [a] -> Maybe a
headOfList [] = Nothing
headOfList (x:_) = Just x

-- Used in the LHS of RULE definitions, so we don't want it inlined too soon.
-- mapMaybe itself has a NOINLINE.
--
-- FIXME this probably isn't necessary. I don't think any mapMaybe rules are
-- actually needed... yet.
{-# INLINE [2] mapMaybe #-}
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = Data.Maybe.mapMaybe
