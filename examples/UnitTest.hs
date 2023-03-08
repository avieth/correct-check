module UnitTest where

import Control.Concurrent.MVar
import Data.Maybe (mapMaybe)
import Composite
import Driver
import Property
import Space
import Types
import Data.Word (Word8)
import System.Environment (getArgs)

import qualified Debug.Trace as Debug

-- A unit test is a special case of a property test in which
-- - the dynamic part is () and therefore no randomness is used
-- - the search space is () and therefore no searching is ever done
-- and so the only sensible domain is:

unitTestDomain :: Domain () () ()
unitTestDomain = Domain
  { search = Search
      { strategy = unitSearchStrategy
      , initialState = ()
      , minimalSpace = ()
      }
  , generate = pure ()
  }

-- Running a unit test with a property test driver will sample a bunch of random
-- seeds, which is wasteful, but it's reasonable to expect GHC to be able to
-- automatically simplify this to run the test subject and exepctations only once.
--
-- What's more, if the test passes, GHC ought to be able to figure out that the
-- entire list of seeds can be thrown out. There are rewrite RULES and INLINE
-- directives in Space.Search which make this possible. And so this property
-- testing framework can be used to express unit tests just as well, without any
-- extra cost.

-- Here are some blatantly obvious cases, in which GHC judged it can skip the
-- entire search because the test case result is completely static.

  {-

-- This will simplify to Nothing, because `runStrategy` will be inlined and
-- `pickFirstFailing (const Nothing)` will be rewritten to `Nothing`.
shouldSimplify_0 :: Maybe ((), (), Int)
shouldSimplify_0 = runStrategy unitSearchStrategy () () (const Nothing)

-- This will simplify to `Just ((), (), 42)`, but is slightly more involved than
-- 'shouldSimplify_0': `runStrategy` will inline to a term involving
--
--   pickFirstFailing (const (Just 42)) ((state, space):_)
--
-- which GHC is able to rewrite to `Just (state, space, 42)`.
shouldSimplify_1 :: Maybe ((), (), Int)
shouldSimplify_1 = runStrategy unitSearchStrategy () () (const (Just 42))

-- 'searchSequential' will use 'runStrategy', which we know will simplify for
-- constant functions. Thanks to inlining, this term also simplifies to Nothing.
shouldSimplify_2 :: Maybe (Seed, ((), (), Int))
shouldSimplify_2 = searchSequential unitSearchStrategy () () (const (const Nothing)) (randomPoints 99 (mkSMGen 42))

-- This one is trickier: it relies upon the fact that splitN gives a non-empty
-- list, and so GHC will be able to inline a cons, which allows for a rewrite
-- of `pickFirstFailing (const (Just r)) (x:_)` to `Just r`. If the list were
-- not known to be cons, then it would not be simplified.
shouldSimplify_3 :: Maybe (Seed, ((), (), Int))
shouldSimplify_3 = searchSequential unitSearchStrategy () () (const (const (Just 42))) (randomPoints 99 (mkSMGen 42))

-- The transformation also works for parallel search.
shouldSimplify_4 :: Maybe (Seed, ((), (), Int))
shouldSimplify_4 = searchParallel 2 unitSearchStrategy () () (const (const Nothing)) (randomPoints 99 (mkSMGen 42))

shouldSimplify_5 :: Maybe (Seed, ((), (), Int))
shouldSimplify_5 = searchParallel 2 unitSearchStrategy () () (const (const (Just 42))) (randomPoints 99 (mkSMGen 42))
-}

-- Now for the more interesting cases of checkSequential and checkParallel on a
-- somewhat less contrived example.
--
-- For non-trivial unit tests, we can't expect GHC to be able to completely
-- eliminate the list from the generated code, but we should expect it to
-- generate code which evaluates the test at most once, even if more than one
-- random sample is requested.
--
-- To demonstrate, we'll need a unit test property. Here's one: it checks that
-- the sum of 1 to n is n*(n+1)/2, but only at one given number. It doesn't
-- use any randomness, and the search space is the unit space, so it's a unit
-- test.
--
-- To make things interesting, we'll put a trace in the subject function. If
-- things are working properly, then a 'quickCheck' of this property should
-- print the trace at most once. It will still claim to have run the test at
-- as many samples as instructed, which may seem like a lie, but is it really?
exampleUnitTest :: Property () () () Int String Int
exampleUnitTest = Property
  { domain = unitTestDomain
  , test = unitTestDefinition
  }

unitTestDefinition :: Test () Int String Int
unitTestDefinition = Test
  { subject = Subject $ \n _ -> Debug.trace ("Evaluating unit test at " ++ show n) (sum [1..n])
  , expectations = that "Gauss was right" $ \n _ s -> s == n * (n + 1) `div` 2
  }

  {-
shouldMemoize_1 :: Maybe (Counterexample () () String)
shouldMemoize_1 = checkSequential 100 (randomPoints 99 (mkSMGen 42)) exampleUnitTest

shouldMemoize_2 :: Maybe (Counterexample () () String)
shouldMemoize_2 = checkParallel 8 100 (randomPoints 99 (mkSMGen 43)) exampleUnitTest
-}

-- For contrast with the unit test, here's the same thing but the parameter
-- comes is derived from the search space. There is no randomness, so a
-- 'quickCheck' on this shouldn't evaluate at multiple random points. However,
-- the driver _will_ have to search through the space for a minimal failing,
-- so the subject will be evaluated potentially more than once (GHC can't
-- float it out).
exampleNonUnitTest :: (forall t. String -> t -> t) -> Property () Natural Int Int String ()
exampleNonUnitTest trace = Property
  { domain = Domain
      { search = Search
          { strategy = linearSearchStrategy 3 0 200
          , initialState = ()
          , minimalSpace = 0
          }
      , generate = fromIntegral <$> parameter
      }
  , test = Test
      { subject = Subject $ \_ n -> trace ("Evaluating non unit test at " ++ show n) (sum [1..n])
      -- NB: a property test search will try to find a minimal example in which
      -- _any_ of the expectations fail (it's a conjunction). It may find a
      -- large example in which many fail, and then shrink it to one in which
      -- only one of them fails.
      -- Here they will both fail at 42, so they'll always both appear in the
      -- report.
      , expectations =
             (that "Gauss was right" $ \_ n s -> if n >= 142 then False else (s == n * (n + 1) `div` 2))
          .& (that "Silly property" $ \_ n s -> if n >= 142 then False else True)
      }
  }

-- The same non-unit test as above, but this one _does_ have randomness.
-- A 'quickCheck' of this one will have to search at every point.
exampleNonUnitTestWithRandomness :: (forall t. String -> t -> t) -> Property () Natural Int Int String ()
exampleNonUnitTestWithRandomness trace = base 
  { domain = (domain base)
    { generate = do
        w8 <- genWord8
        n <- parameter
        pure (fromIntegral w8 + fromIntegral n)
    }
  }
  where
    base = exampleNonUnitTest trace

-- Here's a property test which will search 2-dimensional space, checking that
-- the static part is greater than the sum of squares of the search space, plus
-- a small random number.
--
-- Since the search space is limited to 16 is one dimension and 32 in the other,
-- for a parameter greaer than 1280, not every random sample point will yield a
-- failing example. For a parameter greater than 1280 + 255 = 1535, it will
-- never fail.
--
-- It's a silly property test, but it's enought to demonstrate that the driver
-- is working.
examplePropertyTest :: (forall t. String -> t -> t)
                    -> Property ((), ()) (Natural, Natural) (Word8, Natural, Natural) Natural String Int
examplePropertyTest trace = Property
  { domain = Domain
      { search = Search
          { strategy = twoDimensionalSearchStrategy
              (linearSearchStrategy 3 0 16)
              (linearSearchStrategy 4 0 32)
          , initialState = ((), ())
          , minimalSpace = (0, 0)
          }
      , generate = do
          (n, m) <- parameter
          w8 <- genWord8
          pure (w8, n, m)
      }
  , test = Test
      { subject = Subject $ \_ (w8, n, m) -> n ^ 2 + m ^2 + fromIntegral w8
      , expectations = (that "The threshold is greater" $ \t n s -> s < fromIntegral t)
      }
  }

main :: IO ()
main = do
  [str1] <- getArgs
  -- Can we expect this to not force the entire list of seeds? Yes! Thanks
  -- to the rewrite rules of headOfList applied to a mapMaybeWithPoint or
  -- mapMaybeParallel on a constant function.
  --print (searchSequential unitSearchStrategy () () (searchPredicate (pure ()) unitTestDefinition 100) (randomPoints 99 (mkSMGen 42)))
  --print shouldMemoize_1
  -- This won't evaluate the subject again. In this case that's good, but in
  -- general could be bad, in case the unit test result is so big it could be
  -- considered a space leak.
  --print shouldMemoize_2
  -- Each of these will evaluate the unit test once, and immediately pass,
  -- without paying the cost of 2^64 tests.
  --
  -- FIXME in fact we find that GHC will float out the unit test evaluation, but
  -- will still pay for the cost of the random seed list traversal
  --  quickCheck 131070 100 exampleUnitTest >>= print
  --  print (quickCheckAt 262140 100 exampleUnitTest (mkSMGen 42))
  --  print (checkSequential 100 (randomPoints 262140 (mkSMGen 42)) exampleUnitTest)
  --quickCheckParallel 8 (2 ^ 32 - 1) 102 exampleUnitTest >>= print
  -- This one will do a search, but only at one random point, since the
  -- generator doesn't use the random seed.
  --quickCheck (2 ^ 32 - 1) () (exampleNonUnitTest Debug.trace) >>= print
  -- Can't give a big list here, because we're going to have to search everything.
  -- quickCheckParallel 8 (2 ^ 16) () (exampleNonUnitTestWithRandomness (flip const)) >>= print

  -- A unit test will still simplify within a composite test.
  Debug.traceM "Begin composite test"
  mvar <- newMVar ()
  result <-
    composite defaultGlobalConfig $
    declare "UNIT TEST" exampleUnitTest viaShowRenderer defaultLocalConfig $ \unitTest ->
    declare "NON UNIT TEST" (exampleNonUnitTest (const id)) viaShowRenderer defaultLocalConfig $ \nonUnitTest ->
    declare "NON UNIT TEST WITH RANDOMNESS" (exampleNonUnitTestWithRandomness (const id)) viaShowRenderer defaultLocalConfig $ \nonUnitTestWithRandomness ->
    declare "PROPERTY TEST" (examplePropertyTest (const id)) viaShowRenderer defaultLocalConfig $ \propertyTest ->
    compose $ do
      b <- check unitTest 142
      effect (putStrLn $ "Passed? " ++ show b)
      assert unitTest 143
      check unitTest 144
      check propertyTest (read str1)
      -- For some reason, having a `() <-` can cause rewrite rules to not fire,
      -- and the non-random test not to simplify enough.
      --
      -- It only happens if we write () literally. _ is fine, c is fine.
      -- It's the thing on the RHS that doesn't get simplified.
      --
      -- This surely has something to do with some builtin GHC rule firing and
      -- maybe causing GHC to not have any budget left to try our rules?
      --
      -- If we use 3 simplifier phases, it works.
      assert unitTest 1042
      check unitTest 142
      bracket (takeMVar mvar) (putMVar mvar) $ \_ -> do
        assert unitTest 142
      effect (putStrLn $ "Passed? " ++ show b)
      check propertyTest 1280
      assert nonUnitTest ()
      -- Why do we bother with composite/declare?
      -- After all, we can still run properties directly, and even via quickCheck
      -- because we can do IO for a new random seed.
      effect (quickCheck (2 ^ 32 - 1) 200 exampleUnitTest >>= print)
      -- The point of composite/declare is to put a barrier between the properties
      -- that may be tested, and everything else. It ensures that the arbitrary IO
      -- in the composite cannot interact with the TVar CheckState.
      pure ()
  -- The bracket within the composite ensures this will not block, even if
  -- unitTest failed at 142 and the assertion caused the composite to end
  -- early.
  takeMVar mvar
  printTestResult result
