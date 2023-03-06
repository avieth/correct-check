module Space.Search
  (
  -- * Searching a space at a particular random point.
    Strategy (..)
  , runStrategy

  -- * Searching over multiple random points.
  , searchSequential
  , searchSequentialAll
  , searchParallel
  , searchParallelAll

  -- * Various search strategies
  , unitSearchStrategy
  , hedgehogSearchStrategy
  , twoDimensionalSearchStrategy
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
import Space.Random as Random
import GHC.Base (build)

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
  { complicate = \st n -> if n >= upperBound then [] else [(st, n+1)]
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

-- | Used in 'searchSequentialAll' and 'searchParallelAll' as an argument to
-- 'mapMaybe' or 'mapMaybeParallel'. It's important that this function inlines
-- well, to allow for rewrite rules.
{-# INLINE searchFunction #-}
searchFunction :: Strategy state space
               -> state
               -> space
               -> (Seed -> space -> Maybe failure)
               -> Seed
               -> Maybe (Seed, (state, space, failure))
searchFunction strategy state space p = withPoint (runStrategy strategy state space . p)

{-# INLINE searchSequentialAll #-}
-- | Uses 'searchSpace' at each seed. For those which have a failing example,
-- the minimal space is given along with its search state.
searchSequentialAll :: forall state space failure .
                       Strategy state space
                    -> state
                    -> space
                    -> (Seed -> space -> Maybe failure)
                    -> RandomPoints
                    -> [(Seed, (state, space, failure))]
searchSequentialAll strategy initialState startSpace p =
    mapMaybe (searchFunction strategy initialState startSpace p)
    -- FIXME probably not benefit of having mapMaybeWithPoint. Should remove it.
    -- mapMaybeWithPoint (runStrategy strategy initialState startSpace . p)
  . NE.toList
  . Random.points
  where

{-# INLINE searchSequential #-}
-- | 'searchSequentialAll' but takes the first failing case.
searchSequential :: forall state space failure .
                     Strategy state space
                  -> state
                  -> space
                  -> (Seed -> space -> Maybe failure)
                  -> RandomPoints
                  -> Maybe (Seed, (state, space, failure))
searchSequential strategy initialState startSpace p =
  headOfList . searchSequentialAll strategy initialState startSpace p

{-# INLINE searchParallelAll #-}
-- | Will search at every seed, using parListChunk with a given chunk size.
--
-- There is no parallelism at each random point: complicating and simplifying
-- phases are all sequential.
searchParallelAll :: forall state space failure .
                     Natural -- ^ How much parallelism
                  -> Strategy state space
                  -> state
                  -> space
                  -> (Seed -> space -> Maybe failure)
                  -> RandomPoints
                  -> [(Seed, (state, space, failure))]
searchParallelAll parallelism strategy initialState startSpace p rpoints =
  mapMaybeParallel (searchFunction strategy initialState startSpace p) parallelism (Random.count rpoints) pstrat (NE.toList (Random.points rpoints))
  where
    -- Evaluation strategy for each element? We only need to figure out whether
    -- it's Just or Nothing, and that's done by mapMaybeParallel.
    -- No need to necessarily do the work of evalauting the entire space or
    -- search state (although they will probably be forced anyway).
    pstrat :: Parallel.Strategy (Seed, (state, space, failure))
    pstrat = Parallel.r0

{-# INLINE searchParallel #-}
-- | 'searchParallelAll' but takes the first failing case.
--
-- In the common case where the test is passing, this is likely to be faster
-- than 'searchSequential'. In case the test is failing, then it could be said
-- that some work is wasted by discarding all but the first case (in the order
-- of the seed list), but that's okay, since we can still expect that answer to
-- be found faster than it would have been by 'searchSequential'.
searchParallel :: forall state space failure .
                  Natural -- ^ How much parallelism
               -> Strategy state space
               -> state
               -> space
               -> (Seed -> space -> Maybe failure)
               -> RandomPoints
               -> Maybe (Seed, (state, space, failure))
searchParallel parallelism strategy initialState startSpace p =
  headOfList . searchParallelAll parallelism strategy initialState startSpace p

{-# INLINE makeTriple #-}
makeTriple :: c -> (a, b) -> (a, b, c)
makeTriple c (a, b) = (a, b, c)

{-# INLINE headOfList #-}
headOfList :: [a] -> Maybe a
headOfList [] = Nothing
headOfList (x:_) = Just x

-- | Used in the LHS of RULE definitions, so we don't want it inlined too soon.
-- mapMaybe itself has a NOINLINE.
{-# NOINLINE mapMaybe #-}
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = Data.Maybe.mapMaybe

{-# INLINE withPoint #-}
withPoint :: (a -> Maybe b) -> (a -> Maybe (a,b))
withPoint f a = (,) a <$> f a

-- | A rule will rewrite this to 'mapMaybeWithPointMagic', but gives us the
-- opportunity to apply more specialized rules as well.
{-# NOINLINE mapMaybeWithPoint #-}
mapMaybeWithPoint :: (a -> Maybe b) -> [a] -> [(a, b)]
mapMaybeWithPoint = mapMaybeWithPointMagic

{-# RULES
"mapMaybeWithPoint" [2] mapMaybeWithPoint = mapMaybeWithPointMagic
#-}

-- | Like 'mapMaybe' but keeps the input to the function around.
--
-- It's defined just like the original, even with rewrite rules and inline
-- directives copied. Hence the magical suffix.
--
-- It exists so that we can do a rewrite rule on it when the argument function
-- is \_ -> r, which can/will happen when running a property test which doesn't
-- use any randomness.
mapMaybeWithPointMagic :: (a -> Maybe b) -> [a] -> [(a, b)]
mapMaybeWithPointMagic _ [] = []
mapMaybeWithPointMagic f (x:xs) =
  let rs = mapMaybeWithPointMagic f xs in
  case f x of
    Nothing -> rs
    Just y -> (x,y):rs
{-# NOINLINE [1] mapMaybeWithPointMagic #-}

{-# RULES
"mapMaybeWithPointMagic"     [~1] forall f xs. mapMaybeWithPointMagic f xs
                                  = build (\c n -> foldr (mapMaybeWithPointFB c f) n xs)
"mapMaybeWithPointMagicList" [1]  forall f. foldr (mapMaybeWithPointFB (:) f) [] = mapMaybeWithPoint f
#-}

{-# INLINE [0] mapMaybeWithPointFB #-}
mapMaybeWithPointFB :: ((a,b) -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeWithPointFB cons f x next = case f x of
  Nothing -> next
  Just r -> cons (x,r) next

-- | First argument `n` is the amount of parallelism, second `l` is the length
-- of the list.
--
-- If n is 0 or 1 then there's no parallelism. Otherwise we'll make exactly `n`
-- calls to `rpar`. If the remainder `r` of `div l n` is not zero, then the
-- first `r` batches will get one extra work item.
--
-- A strategy can be given for the thing inside the Maybe. The only evaluation
-- that 'mapMaybeParallel' commits to is of course WHNF of the Maybe.
{-# INLINE [2] mapMaybeParallel #-}
mapMaybeParallel :: (a -> Maybe b) -> Natural -> Natural -> Parallel.Strategy b -> [a] -> [b]
mapMaybeParallel p n l strat =
  if n <= 1
  then Data.Maybe.mapMaybe p
  else mapMaybeParallelAt p (fromIntegral q) (fromIntegral r) strat
  where
    (q, r) = l `divMod` n

{-# NOINLINE mapMaybeParallelAt #-}
mapMaybeParallelAt :: (a -> Maybe b) -> Int -> Int -> Parallel.Strategy b -> [a] -> [b]
mapMaybeParallelAt p quotient remainder strat lst = concat (Parallel.runEval (go remainder lst))
  where
    go r lst = do
      let i = quotient + if r <= 0 then 0 else 1
          (prefix, suffix) = splitAt i lst
      -- evalList with the given strat will force the spine of the list and
      -- therefore will accomplish the work that we wanted to do in parallel:
      -- determining whether each element is Just or Nothing.
      prefix' <- Parallel.rparWith (Parallel.evalList strat) (Data.Maybe.mapMaybe p prefix)
      case suffix of
        [] -> pure [prefix']
        suffix@(_:_) -> do
          suffix' <- go (if r <= 0 then 0 else r - 1) suffix
          pure (prefix' : suffix')

-- TODO if mapMaybeWithPoint proves useful, define analogous mapMaybeParallelAt.

{-# RULES
"mapMaybeConstNothing1" mapMaybe (\_ -> Nothing) = const []
"mapMaybeConstNothing2" forall xs . mapMaybe (\_ -> Nothing) xs = []

"mapMaybeConstJust1" forall r . mapMaybe (\_ -> Just r) = fmap (const r)
"mapMaybeConstJust2" forall r xs . mapMaybe (\_ -> Just r) xs = fmap (const r) xs

"mapMaybeConst" forall r xs . mapMaybe (\_ -> r) xs = maybe [] (flip fmap xs . const) r

"mapMaybeWithPointConstNothing1" [2] mapMaybeWithPoint (\_ -> Nothing) = const []
"mapMaybeWithPointConstNothing2" [2] forall xs . mapMaybeWithPoint (\_ -> Nothing) xs = []

"mapMaybeWithPointConstJust1" [2] forall r . mapMaybeWithPoint (\_ -> Just r) = fmap (flip (,) r)
"mapMaybeWithPointConstJust2" [2] forall r xs . mapMaybeWithPoint (\_ -> Just r) xs = fmap (flip (,) r) xs

"mapMaybeWithPointConst" [2] forall r xs . mapMaybeWithPoint (\_ -> r) xs = maybe [] (flip fmap xs . (,)) r

-- This is the one that actually fires for unit tests. If a const (const (Just x))
-- test predicate is applied to a parallel or sequential search, what we end up
-- with is a function of the form \s -> Just (s, x) where s is the random seed,
-- and that is what we want to rewrite.
"mapMaybeJust1" forall c . mapMaybe (\s -> Just (s, c)) = fmap (flip (,) c)

"pickFailingConstNothing1" pickFailing (\_ -> Nothing) = const Nothing
"pickFailingConstNothing2" forall x . pickFailing (\_ -> Nothing) x = Nothing

"pickFailingConstJust1" forall r . pickFailing (\_ -> Just r) = Just . makeTriple r
"pickFailingConstJust2" forall r x . pickFailing (\_ -> Just r) x = Just (makeTriple r x)

"pickFirstFailingConstNothing1" pickFirstFailing (\_ -> Nothing) = const Nothing
"pickFirstFailingConstNothing2" forall x . pickFirstFailing (\_ -> Nothing) x = Nothing

"pickFirstFailingConstJust" forall r x xs . pickFirstFailing (\_ -> Just r) (x:xs) = Just (makeTriple r x)
#-}

{-# RULES
"mapMaybeParallelAtConstNothing1" forall n l s . mapMaybeParallelAt (\_ -> Nothing) n l s = const []
"mapMaybeParallelAtConstNothing2" forall n l s xs . mapMaybeParallelAt (\_ -> Nothing) n l s xs = []

"mapMaybeParallelAtConstJust1" forall n l s r . mapMaybeParallelAt (\_ -> Just r) n l s = fmap (const r)
"mapMaybeParallelAtConstJust2" forall n l s r xs . mapMaybeParallelAt (\_ -> Just r) n l s xs = fmap (const r) xs

"mapMaybeParallelAtConst" forall n l s r xs . mapMaybeParallelAt (\_ -> r) n l s xs = maybe [] (flip fmap xs . const) r

"mapMaybeParallelAtJust1" forall n l s c . mapMaybeParallelAt (\s -> Just (s, c)) n l s = fmap (flip (,) c)
#-}
