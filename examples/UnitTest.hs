module UnitTest where

import Data.Maybe (mapMaybe)
import Driver
import Property
import Space
import Types

import Debug.Trace

-- A unit test is a special case of a property test in which
-- - the dynamic part is () and therefore no randomness is used
-- - the search space is () and therefore no searching needs to be done
-- and so the only sensible domain is

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

  {-
-- shouldSimplify_6 :: Seed -> Maybe (Seed, ((), (), Int))
-- shouldSimplify_6 seed = searchSequential unitSearchStrategy () () (const (const Nothing)) (randomPoints 99 seed)

-- TODO now try it on checkSequential and checkParallel. They should also
-- simplify just like the searches.
--
-- To do this, we'll need a unit test property.
--
-- Here's one: it checks that the sum of 1 to n is n*(n+1)/2, but only at a
-- single given number. It doesn't use any randomness, and the search space is
-- the unit space, so it's a unit test.
--
-- To make things interesting, we'll put a trace in the subject function. If
-- things are working properly, then a quickCheck of this property should
-- print the trace at most once. It will still claim to have run the test at
-- as many samples as instructed, which may seem like a lie, but is it really?
exampleUnitTest :: Property () () () Int String Int
exampleUnitTest = Property
  { domain = unitTestDomain
  , test = Test
      { subject = Subject $ \n _ -> trace "EVAL" (sum [1..n])
      , expectations = assert $ that "Gauss was right" $ \n _ s -> s == n * (n + 1) `div` 2
      }
  }

-- What we can hope for here is that GHC will float out the evaluation of the
-- test, which only needs to happen once.
--
-- But it still won't know statically whether it's a Just or a Nothing, so
-- can we expect a rewrite? We'll get a form that looks like
--
--   let result = floatedUnitTestResult
--       ...
--    in mapMaybe (\s -> fmap ((,) s) result) xs
--
-- which should rewrite to
--
--   case result of
--     Nothing -> const Nothing
--     Just r -> \xs -> case xs of
--       [] -> Nothing
--       (x:xs) -> Just (s, r)
--
-- and that would fuse the list away.
--
-- Should define our own mapMaybe variant
--
--   mapMaybeWithPoint :: (a -> Maybe b) -> [a] -> [(a, b)]
--
-- with rewrite rule
--
--   mapMaybeWithPoint (\_ -> r) xs = case r of
--     Nothing -> []
--     Just t -> fmap (flip (,) t) xs
--
-- and then rely upon headOfList inlining on this to simplify further to
--
--   headOfList (mapMaybeWithPoint (\_ -> r) xs) = case r of
--     Nothing -> Nothing
--     Just t -> case xs of
--       [] -> Nothing 
--       (x:_) -> flip (,) t x
--
--
-- Should work!
-- So that's the cool thing: the rewrite rules previously discussed would only
-- work if you actually wrote out a statically constant unit test. But if you
-- write a unit test that actually does need to compute something, GHC can still
-- fuse away the random sampling part and skip it.
shouldSimplify_7 :: Maybe (Counterexample () () String)
shouldSimplify_7 = checkSequential 100 (randomPoints 99 (mkSMGen 42)) exampleUnitTest

{-



exampleNonUnitTest :: Property () Natural Natural Natural String ()
exampleNonUnitTest = Property
  { domain = Domain
      { search = Search
          { strategy = hedgehogSearchStrategy
          , initialState = ()
          , minimalSpace = 0
          }
      , generate = do
          w8 <- genWord8
          n <- parameter
          pure (fromIntegral w8 + n)
      }
  , test = Test
      { subject = Subject $ \_ n -> sum [1..n]
      , expectations = assert $ that "Gauss was right" $ \_ n s -> s == n * (n + 1) `div` 2
      }
  }
-}

--main :: IO ()
-- main = checkSequential 100 (randomPoints 99 seed) exampleUnitTest
--main = quickCheck 100 (2*4096) exampleUnitTest >>= print
--main = quickCheckParallel () 8 (2*4096) exampleNonUnitTest >>= print
-- main = quickCheck () (2*4096) exampleNonUnitTest >>= print
-}
