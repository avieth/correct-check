module Space.Search
  (
  -- * Searching a space at a particular random point.
    Strategy (..)
  , SearchDef (..)
  , searchWith

  -- * Searching over multiple random points.
  , searchSequential
  , searchSequentialAll
  , searchParallel
  , searchParallelAll

  -- * Various search definitions
  , trivialStrategy
  , trivialSearchDef
  , linearStrategy
  , linearSearchDef
  , powerStrategy
  , powerSearchDef
  , twoDimensionalStrategy
  , twoDimensionalSearchDef
  ) where

import Control.Applicative ((<|>))
import qualified Control.Parallel.Strategies as Parallel
import Data.List (unfoldr)
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Numeric.Natural (Natural)
import Space.Random as Random
import GHC.Base (build)

-- | Obvious definition is
--
-- > data Strategy space where
-- >   Strategy :: SearchDef state space -> state -> space -> Strategy space
--
-- but would that prohibit good inlining and simplification? I don't know, but
-- this one probably will inline better.
newtype Strategy space = Strategy
  { runStrategy :: forall r . (forall state. SearchDef state space -> state -> space -> r) -> r }

-- | Defines how to search an ordered space. These may use random generators
-- which do not depend upon any space (randomness only).
--
-- There may be arbitrarily-many minimally-more-complicated points, but the
-- strategy must pick a particular one to be searched next. It need not be
-- minimally-more-complex; for example, a complication could go from 0 to 42
-- in one jump.
--
-- A similar notion of simplification is here too, but it should randomly
-- choose a maximally-less-complicated point, because shrinking needs to go
-- in minimal steps in order to work properly.
data SearchDef state space = SearchDef
  { complicate :: Seed -> state -> space -> Maybe (state, space)
  -- ^ If Just, then the space must be strictly more complicated.
  , simplify :: Seed -> state -> space -> [(state, space)]
  -- ^ Every space must be strictly less complicated, but maximally so.
  }

-- | Never grows, never shrinks.
trivialSearchDef :: SearchDef state space
trivialSearchDef = SearchDef
  { complicate = \_ _ _ -> Nothing
  , simplify = \_ _ _ -> []
  }

-- | Always give (), no growing or shrinking.
{-# INLINE trivialStrategy #-}
trivialStrategy :: Strategy ()
trivialStrategy = Strategy $ \k -> k trivialSearchDef () ()

-- | Similar to hedgehog: increases a single "size" parameter to an upper
-- bound. This one is linear: it will always increase by a constant factor.
---
-- Note one key difference from hedgehog: a generator do not need to state an
-- upper bound, but the search strategy does.
--
-- NB: using a coefficient of 1 may not be desirable, because it means shrinking
-- is useless: the minimal failing case will always be found on the way up.
linearSearchDef :: Natural -> Natural -> Natural -> SearchDef state Natural
linearSearchDef c lowerBound upperBound = SearchDef
  { complicate = \_ st n -> if n >= upperBound then Nothing else Just (st, (n + 1) * c)
  , simplify = \_ st n -> if n <= lowerBound then [] else [(st, n-1)]
  }

-- | 'linearSearchDef' starting at 0.
{-# INLINE linearStrategy #-}
linearStrategy :: Natural -> Natural -> Natural -> Strategy Natural
linearStrategy c l u = Strategy $ \k -> k (linearSearchDef c l u) () 0

-- | Instead of multiplying by c, as in 'linearSearchStrategy', this one will
-- take the c'th power of the current point.
powerSearchDef :: Natural -> Natural -> Natural -> SearchDef state Natural
powerSearchDef c lowerBound upperBound = SearchDef
  { complicate = \_ st n -> if n >= upperBound then Nothing else Just (st, (n + 1) ^ c)
  , simplify = \_ st n -> if n <= lowerBound then [] else [(st, n-1)]
  }

-- | 'powerSearchDef' starting at 0.
{-# INLINE powerStrategy #-}
powerStrategy :: Natural -> Natural -> Natural -> Strategy Natural
powerStrategy c l u = Strategy $ \k -> k (powerSearchDef c l u) () 0

-- TODO
-- wouldn't it be better to use the state parameter, in linear and exponential,
-- to track how many times it has been increased? 
-- Still not actually clear whether having the state parameter is useful at all.

twoDimensionalSearchDef :: forall s1 s2 a b .
                           SearchDef s1 a
                        -> SearchDef s2 b
                        -> SearchDef (s1, s2) (a, b)
twoDimensionalSearchDef first second = SearchDef
  { complicate = complicateProduct
  , simplify = simplifyProduct
  }
  where

    -- These are the exact same form.

    complicateProduct g (s1, s2) (x, y) = split g $ \g1 g2 ->
      let l = complicate first  g1 s1 x
          r = complicate second g2 s2 y
          both = (\(s1, x) (s2, y) -> ((s1, s2), (x, y))) <$> l <*> r
          onlyL = (\(s1, x) -> ((s1, s2), (x, y))) <$> l
          onlyR = (\(s2, y) -> ((s1, s2), (x, y))) <$> r
       in both <|> onlyL <|> onlyR

    simplifyProduct g (s1, s2) (x, y) = split g $ \g1 g2 ->
      let l = simplify first  g1 s1 x
          r = simplify second g2 s2 y
          both = (\(s1, x) (s2, y) -> ((s1, s2), (x, y))) <$> l <*> r
          onlyL = (\(s1, x) -> ((s1, s2), (x, y))) <$> l
          onlyR = (\(s2, y) -> ((s1, s2), (x, y))) <$> r
       in both <|> onlyL <|> onlyR

{-# INLINE twoDimensionalStrategy #-}
twoDimensionalStrategy :: Strategy l -> Strategy r -> Strategy (l, r)
twoDimensionalStrategy l r = Strategy $ \k ->
  runStrategy l $ \lDef lState lSpace ->
    runStrategy r $ \rDef rState rSpace -> 
      k (twoDimensionalSearchDef lDef rDef) (lState, rState) (lSpace, rSpace)

-- | Use a strategy to find a minimal failing example. It's defined with this
-- type and in this way so that it inlines well, given where and how it is called
-- in 'searchSequentialAll' and 'searchParallelAll'.
--
-- The `space` given will not be simplified in case a failure is found here, and
-- so it should always be a minimal space.
{-# INLINE searchWith #-}
searchWith :: forall state space failure .
              SearchDef state space
           -> state
           -- ^ Start state for the search.
           -> space
           -- ^ This is assumed to be a _minimal_ search space point.
           -- If the test fails at the this point, it will not be shrunk.
           -> (Seed -> space -> Maybe failure)
           -> Seed
           -- ^ Will be split and used in the search strategy, and also the
           -- search predicate.
           -> Maybe (state, space, failure)
searchWith searchDef initialState minimalSpace p = splitK $ \g1 g2 ->
  -- splitK is used because it inlines well.
  --
  -- We're interested in the case where the function
  --
  --   p :: Seed -> space -> Maybe failure
  --
  -- is a constant, because in this case we can save some work. We never need to
  -- run it at the complications, since we know the answer is the same for all of
  -- them.
  --
  -- What's more, since we assume that this is called on the minimal search state,
  -- we don't need to do any shrinking in this case either.
  --
  -- So the goal is that
  --
  --   searchWith searchDef state space (\_ _ c)
  --
  -- would simplify to
  --
  --   \_ -> fmap (\failure -> (state, space, failure)) c
  --
  -- and therefore that its application to a mapMaybeWithPoint or mapMaybeParallel
  -- in searchSequentialAll and searchParallelAll would trigger one of the rewrite
  -- rules
  --
  --   "headOfListMapMaybeWithPointConst1"
  --   "headOfListMapMaybeParallelConst1"
  --
  -- and therefore that checking a unit test will skip the random sample
  -- generation and evaluate the test at most once.
  let f = p g1
      here = f minimalSpace
      others = searchStrategyNonMinimal (complicate searchDef) (simplify searchDef) f (g2, (initialState, minimalSpace))
      -- The idea is that if p is constant, then GHC will float it out and
      -- we'll be left with a constant function which does this case on the
      -- let-floated value `here`.
   in case here of
        Just failure -> Just (initialState, minimalSpace, failure)
        Nothing -> takeFirst others

-- | Uses the complicate function until a failing case is found, then uses
-- the simplify function to minimze it.
--
-- The state and space given are assumed to already have been checked.
{-# INLINE searchStrategyNonMinimal #-}
searchStrategyNonMinimal :: (Seed -> state -> space -> Maybe (state, space))
                         -> (Seed -> state -> space -> [(state, space)])
                         -> (space -> Maybe failure)
                         -> (Seed, (state, space))
                         -> [Maybe (state, space, failure)]
searchStrategyNonMinimal complicate simplify p = unfoldr
  (\(g, (state, space)) -> split g $ \g1 g2 -> do
    -- Only stops when complicate gives Nothing.
    (state', space') <- complicate g1 state space
    let next = (g2, (state', space'))
    Just $ case p space' of
      -- No failure? Continue the unfold.
      Nothing -> (Nothing, next)
      -- Failure? Minimize it.
      -- The same seed is given to both; minimize will split it.
      Just failure -> (Just (minimize simplify p next failure), next)
  )

{-# INLINE minimize #-}
minimize :: (Seed -> state -> space -> [(state, space)])
         -> (space -> Maybe failure)
         -> (Seed, (state, space))
         -> failure
         -> (state, space, failure)
minimize simplify p (g, (state, space)) failure = split g $ \g1 g2 ->
  case pickFirstFailing p (simplify g1 state space) of
    Nothing -> (state, space, failure)
    Just (state', smallerSpace, failure') -> minimize simplify p (g2, (state', smallerSpace)) failure'

-- Note about rewrite rules.
--
-- Suppose we have a test that doesn't depend upon the random seed. We would
-- hope that a search over a list of seeds could short-circuit automatically,
-- and give the result of one run, without even forcing the list of all random
-- seeds.
--
-- If the test is known to fail (gives Just) then laziness does this for us.
-- 'searchSequential', for instance, will only force the head of the list,
-- because it's Just.
--
-- But if the test is known to never fail (gives Nothing) then things are more
-- complicated. We know that such a search can be rewritten:
--
--     headOfList (mapMaybeWithPoint (const r) seeds)
--   = case seeds of
--       [] -> Nothing
--       (s:_) -> case r of
--         Nothing -> Nothing
--         Just y -> (s, y)
{-# RULES
"headOfListMapMaybeWithPointConst1" forall r xs .
    headOfList (mapMaybeWithPoint (\_ -> r) xs)
  = case xs of
      [] -> Nothing
      (x:_) -> case r of
        Nothing -> Nothing
        Just y -> Just (x, y)

"headOfListMapMaybeParallelConst1" forall n m searchDef r xs .
    headOfList (mapMaybeParallel (\_ -> r) n m searchDef xs)
  = case xs of
      [] -> Nothing
      (x:_) -> case r of
        Nothing -> Nothing
        Just y -> Just (x, y)
#-}

-- TODO
-- There may be other rewrite rules that we'd like regarding mapMaybeWithPoint
-- and mapMaybeParallel when the function given is known to be a constant.
-- For instance:
--
--     mapMaybeWithPoint (const r) xs
--   = case r of
--       Nothing -> []
--       Just y -> fmap (const y) xs
--
-- This can save work in a full search, but will it interfere with the
-- headOfList rules? We would want those ones to go first, so we would need
-- some phase control.

{-# INLINE searchSequentialAll #-}
-- | Uses 'searchSpace' at each seed. For those which have a failing example,
-- the minimal space is given along with its search state.
searchSequentialAll :: forall state space failure .
                       SearchDef state space
                    -> state
                    -> space
                    -> (Seed -> space -> Maybe failure)
                    -> RandomPoints
                    -> [(Seed, (state, space, failure))]
searchSequentialAll searchDef initialState startSpace p =
    mapMaybeWithPoint (searchWith searchDef initialState startSpace p)
  . NE.toList
  . Random.points
  where

{-# INLINE searchSequential #-}
-- | 'searchSequentialAll' but takes the first failing case.
searchSequential :: forall state space failure .
                     SearchDef state space
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
                     Word32 -- ^ How much parallelism
                  -> SearchDef state space
                  -> state
                  -> space
                  -> (Seed -> space -> Maybe failure)
                  -> RandomPoints
                  -> [(Seed, (state, space, failure))]
searchParallelAll parallelism searchDef initialState startSpace p rpoints =
  mapMaybeParallel (searchWith searchDef initialState startSpace p) (fromIntegral parallelism) (Random.count rpoints) pstrat (NE.toList (Random.points rpoints))
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
                  Word32 -- ^ How much parallelism
               -> SearchDef state space
               -> state
               -> space
               -> (Seed -> space -> Maybe failure)
               -> RandomPoints
               -> Maybe (Seed, (state, space, failure))
searchParallel parallelism strategy initialState startSpace p =
  headOfList . searchParallelAll parallelism strategy initialState startSpace p

{-# INLINE takeFirst #-}
takeFirst :: [Maybe t] -> Maybe t
takeFirst = foldr combine Nothing
  where
    combine Nothing  x = x
    combine (Just x) _ = Just x

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

{-# INLINE makeTriple #-}
makeTriple :: (a, b) -> c -> (a, b, c)
makeTriple (a, b) c = (a, b, c)

{-# INLINE [2] headOfList #-}
headOfList :: [a] -> Maybe a
headOfList [] = Nothing
headOfList (x:_) = Just x

-- | Used in the LHS of RULE definitions, so we don't want it inlined too soon.
-- mapMaybe itself has a NOINLINE.
{-# NOINLINE mapMaybe #-}
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = Data.Maybe.mapMaybe

-- | A rule will rewrite this to 'mapMaybeWithPointMagic', but gives us the
-- opportunity to apply more specialized rules as well.
{-# NOINLINE mapMaybeWithPoint #-}
mapMaybeWithPoint :: (a -> Maybe b) -> [a] -> [(a, b)]
mapMaybeWithPoint = mapMaybeWithPointMagic

{-# RULES
"mapMaybeWithPoint" [1] mapMaybeWithPoint = mapMaybeWithPointMagic
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
mapMaybeParallel :: (a -> Maybe b) -> Int -> Int -> Parallel.Strategy (a, b) -> [a] -> [(a, b)]
mapMaybeParallel p n l strat =
  if n <= 1
  then mapMaybeWithPoint p
  else mapMaybeParallelAt p q r strat
  where
    (q, r) = l `divMod` n

{-# NOINLINE mapMaybeParallelAt #-}
mapMaybeParallelAt :: (a -> Maybe b) -> Int -> Int -> Parallel.Strategy (a, b) -> [a] -> [(a, b)]
mapMaybeParallelAt p quotient remainder strat lst = concat (Parallel.runEval (go remainder lst))
  where
    go r lst = do
      let i = quotient + if r <= 0 then 0 else 1
          (prefix, suffix) = splitAt i lst
      -- evalList with the given strat will force the spine of the list and
      -- therefore will accomplish the work that we wanted to do in parallel:
      -- determining whether each element is Just or Nothing.
      prefix' <- Parallel.rparWith (Parallel.evalList strat) (mapMaybeWithPoint p prefix)
      case suffix of
        [] -> pure [prefix']
        suffix@(_:_) -> do
          suffix' <- go (if r <= 0 then 0 else r - 1) suffix
          pure (prefix' : suffix')

-- Not all of these are necessary / ever used.
{-# RULES
"mapMaybeConstNothing1" mapMaybe (\_ -> Nothing) = const []
"mapMaybeConstNothing2" forall xs . mapMaybe (\_ -> Nothing) xs = []

"mapMaybeConstJust1" forall r . mapMaybe (\_ -> Just r) = fmap (const r)
"mapMaybeConstJust2" forall r xs . mapMaybe (\_ -> Just r) xs = fmap (const r) xs

"mapMaybeConst" forall r xs . mapMaybe (\_ -> r) xs = maybe [] (flip fmap xs . const) r

"mapMaybeWithPointConstNothing1" mapMaybeWithPoint (\_ -> Nothing) = const []
"mapMaybeWithPointConstNothing2" forall xs . mapMaybeWithPoint (\_ -> Nothing) xs = []

"mapMaybeWithPointConstJust1" forall r . mapMaybeWithPoint (\_ -> Just r) = fmap (flip (,) r)
"mapMaybeWithPointConstJust2" forall r xs . mapMaybeWithPoint (\_ -> Just r) xs = fmap (flip (,) r) xs

"mapMaybeWithPointConst" forall r xs . mapMaybeWithPoint (\_ -> r) xs = maybe [] (flip fmap xs . (,)) r

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
