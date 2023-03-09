module Check
  ( Counterexample (..)
  , checkSequential
  , checkSequentialAll
  , checkParallel
  , checkParallelAll

  -- * Low-level
  , searchPredicate
  ) where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Word (Word32)
import Space.Random
import Space.Search
import Types

-- | Check the property at each of these seeds, giving all counterexamples.
--
-- Think of this as application of a test to a domain.
{-# INLINE checkSequentialAll #-}
checkSequentialAll :: RandomPoints
                   -> Domain space specimen
                   -> Test assertion specimen result
                   -> [Counterexample space specimen result assertion]
checkSequentialAll seeds domain test = runStrategy (search domain) $ \searchDef state space ->
  fmap toCounterexample (searchSequentialAll searchDef state space (searchPredicate (generate domain) test) seeds)

-- | Check the property at each of these seeds, stopping at the first
-- counterexample.
{-# INLINE checkSequential #-}
checkSequential :: RandomPoints
                -> Domain space specimen
                -> Test assertion specimen result
                -> Maybe (Counterexample space specimen result assertion)
checkSequential seeds domain test = runStrategy (search domain) $ \searchDef state space ->
  fmap toCounterexample (searchSequential searchDef state space (searchPredicate (generate domain) test) seeds)

-- | Same as 'checkSequentialAll' but will check a given number in parallel (the
-- Word32 parameter) and every seed will be checked.
{-# INLINE checkParallel #-}
checkParallelAll :: Word32
                 -> RandomPoints
                 -> Domain space specimen
                 -> Test assertion specimen result
                 -> [Counterexample space specimen result assertion]
checkParallelAll n seeds domain test = runStrategy (search domain) $ \searchDef state space ->
  fmap toCounterexample (searchParallelAll n searchDef state space (searchPredicate (generate domain) test) seeds)

-- | 'checkParallel' but stop at the first counterexample.
{-# INLINE checkParallelAll #-}
checkParallel :: Word32
              -> RandomPoints
              -> Domain space specimen
              -> Test assertion specimen result
              -> Maybe (Counterexample space specimen result assertion)
checkParallel n seeds domain test = runStrategy (search domain) $ \searchDef state space ->
  fmap toCounterexample (searchParallel n searchDef state space (searchPredicate (generate domain) test) seeds)

-- | Convert a search result into a counterexample.
{-# INLINE toCounterexample #-}
toCounterexample :: (Seed, (state, space, (dynamic, result, NonEmpty (Assertion refutation))))
                 -> Counterexample space dynamic result refutation
toCounterexample (seed, (_, space, (generated, result, refutations))) = Counterexample
  { randomSeed = seed
  , searchPoint = space
  , generatedValue = generated
  , testResult = result
  , refutations = refutations
  }

-- | Used to define a search.
{-# INLINE searchPredicate #-}
searchPredicate :: forall space specimen result assertion .
                   Gen space specimen
                -> Test assertion specimen result
                -> (Seed -> space -> Maybe (specimen, result, NonEmpty (Assertion assertion)))
-- Want to make this function simplify in a certain way.
--
-- Writing it as an explicit lambda on seed and space seems to help the
-- simplifier to do the right thing and float out as much as possible, in case
-- of tests which do not depend upon the seed or the space or both.
searchPredicate gen test = \seed -> searchPredicateAt
  (sampleAt_ gen seed)
  (runSubject (subject test))
  (checkExpectations (expectations test))

{-# INLINE searchPredicateAt #-}
searchPredicateAt :: (space -> specimen)
                  -> (specimen -> result)
                  -> (specimen -> result -> [Assertion assertion])
                  -> (space -> Maybe (specimen, result, NonEmpty (Assertion assertion)))
searchPredicateAt appliedGen appliedSubject appliedVerification = \space ->
  let dynamic = appliedGen space
      result = appliedSubject dynamic
      refutations = appliedVerification dynamic result
   in fmap (\t -> (dynamic, result, t)) (nonEmpty refutations)

-- | Defined for use in runConjunction; choice of list is for its semigroup
-- instance.
{-# INLINE CONLIKE checkExpectation #-}
checkExpectation :: specimen -> result -> Expectation assertion specimen result -> [Assertion assertion]
checkExpectation s r (Expectation assertion ver) = case runVerification ver s r of
  True -> []
  False -> [assertion]

{-# INLINE CONLIKE checkExpectations #-}
checkExpectations :: Expectations assertion specimen result -> specimen -> result -> [Assertion assertion]
checkExpectations expectations specimen result =
  runConjunction (checkExpectation specimen result) expectations

-- | A counterexample produced from checking a property test. Must give
-- enough information to reproduce the failure, but also gives some extra
-- stuff that the programmer may be interested to know.
data Counterexample space generated result assertion = Counterexample
  -- May as well be strict in all fields since they've already been computed
  -- or else we wouldn't have a counterexample.
  { randomSeed :: !Seed
  , searchPoint :: !space
  , generatedValue :: !generated
  -- ^ The random seed and search point generated this value (according to
  -- the domain used).
  , testResult :: !result
  -- ^ The test subject gave this value, with generatedValue as input.
  , refutations :: !(NonEmpty (Assertion assertion))
  -- ^ These assertions were refuted.
  }
