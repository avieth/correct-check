module Quick.Check
  ( quickCheck
  , quickCheckParallel
  , quickCheckAt
  , quickCheckParallelAt
  , QuickCheck (..)
  ) where

import Control.Exception (SomeException (..), evaluate, try)
import Data.Foldable (toList)
import Check
import Numeric.Natural (Natural)
import Space.Random
import Types

-- | Useful for interactive development. Checks the property at a given number
-- of points, giving at most one counterexample.
--
-- It's in IO because it will grab a new random seed from system entropy, and
-- it will also catch any exception from the test (in case of a partial match,
-- error call, throw, etc.)
--
-- To re-run a complete 'quickCheck', use 'quickCheckAt'.
{-# INLINE quickCheck #-}
quickCheck :: Natural
           -> Test assertion specimen result
           -> Domain space specimen
           -> IO (QuickCheck space specimen result assertion)
quickCheck n test domain = newSeedIO >>= quickCheckAt n domain test

-- | First number is the amount of parallelism, second is the number of random
-- samples to check.
{-# INLINE quickCheckParallel #-}
quickCheckParallel :: Natural
                   -> Natural
                   -> Test assertion specimen result
                   -> Domain space specimen
                   -> IO (QuickCheck space specimen result assertion)
quickCheckParallel m n test domain = newSeedIO >>= quickCheckParallelAt m n domain test

{-# INLINE quickCheckAt #-}
quickCheckAt :: Natural
             -> Domain space specimen
             -> Test assertion specimen result
             -> Seed
             -> IO (QuickCheck space specimen result assertion)
quickCheckAt n domain test seed = fmap (mkQuickCheckResult n seed)
  -- checkSequential always uses at least one point, so we have to check n here.
  -- randomPoints takes the number of points desired less 1.
  (if n > 0
  then try (evaluate (checkSequential (randomPoints (n' - 1) seed) domain test))
  else pure (Right Nothing))
  where
    !n' = clampToWord32 tag n
    tag = "quickCheckSequential"

{-# INLINE quickCheckParallelAt #-}
quickCheckParallelAt :: Natural
                     -> Natural
                     -> Domain space specimen
                     -> Test assertion specimen result
                     -> Seed
                     -> IO (QuickCheck space specimen result assertion)
quickCheckParallelAt m n domain test seed = fmap (mkQuickCheckResult n seed)
  (if n > 0
  then try (evaluate (checkParallel m' (randomPoints (n' - 1) seed) domain test))
  else pure (Right Nothing))
  where
    -- Want to crash early so we force these
    !m' = clampToWord32 tag m
    !n' = clampToWord32 tag n
    tag = "quickCheckParallel"

mkQuickCheckResult :: Natural
                   -> Seed
                   -> Either SomeException (Maybe (Counterexample space specimen result assertion))
                   -> QuickCheck space specimen result assertion
mkQuickCheckResult n s (Right Nothing) = Passed n s
mkQuickCheckResult n s (Left ex) = Failed n s (Left ex)
mkQuickCheckResult n s (Right (Just cexample)) = Failed n s (Right cexample)

-- | A type for the result of a 'quickCheck', mainly defined for its Show
-- instance.
data QuickCheck space specimen result assertion where
  Passed :: Natural -> Seed -> QuickCheck space specimen result assertion
  -- | Includes the initial seed that was generated by 'quickCheck'; the
  -- counterexample will have the seed of the particular test case.
  Failed :: Natural -> Seed -> Either SomeException (Counterexample space specimen result assertion) -> QuickCheck space specimen result assertion

-- | Show instance is defined for the benefit of using 'quickCheck' in the repl.
--
-- Doesn't show that much detail about a failure.
instance (Show space, Show specimen, Show result, Show assertion) => Show (QuickCheck space specimen result assertion) where
  show (Passed n seed) = mconcat
    [ "Passed "
    , show n
    , " random samples with initial seed "
    , showSeedHex seed
    ]
  show (Failed _ _ (Left (SomeException ex))) = mconcat
    [ "Failed with exception "
    , show ex
    ]
  show (Failed _ seed (Right cexample)) = mconcat
    [ "Found counterexample from initial seed ", showSeedHex seed, "\n"
    , "Random seed:     ", showSeedHex (randomSeed cexample), "\n"
    , "Search point:    ", show (searchPoint cexample), "\n"
    , "Generated value: ", show (generatedValue cexample), "\n"
    , "Test result:     ", show (testResult cexample), "\n"
    , "Refuted:         ", show (toList (refutations cexample))
    ]
