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

-- Running a unit test with the typical property test drivers will sample a
-- bunch of random seeds, which is wasteful, but it's reasonable to expect GHC
-- to be able to automatically simplify this to run the test subject and
-- exepctations only once.
--
-- What's more, if the test passes, GHC ought to be able to figure out that the
-- entire list of seeds can be skipped, because this rewrite rule is justified:
--
--     mapMaybe (const r) lst
--   = case r of
--       Nothing -> []
--       Just t -> fmap (const t) lst
--
-- or another way
--
--     mapMaybe (const r) xs = maybe [] (flip fmap xs . const) r
--
-- Making this actually happen will probably require more work.

  {-
-- This will simplify, via the rewrite rule pickFirstFailingConstNothing2, to
-- Nothing.
shouldSimplify_1 :: Maybe ((), (), String)
shouldSimplify_1 = pickFirstFailing (const Nothing) (complicateBreadthFirst (complicate unitSearchStrategy) () ())

-- Want this to simplify to only run the function once.
--
shouldSimplify_2 :: Maybe ((), (), String)
shouldSimplify_2 = pickFirstFailing (const (Just "blargh")) (complicateBreadthFirst (complicate unitSearchStrategy) () ())
-}

-- This simplifies thanks to pickFirstFailingConstNothing2. runStrategy is
-- inlined, its failingPoint becomes Nothing, and then fmap of simplifications
-- over Nothing is simplified to Nothing.
shouldSimplify_3 :: Maybe ((), (), Int)
shouldSimplify_3 = runStrategy unitSearchStrategy () () (const Nothing)

-- In case we have a const Just form, meaning the test always fails in the same
-- way, not depending on the space, then GHC _should_ be able to eliminate the
-- complicateBreadthFirst list: we aleady have the space at which to run the
-- predicate, which we know is Just, and we have the search state as well... so
-- we should be able to rewrite
--
--     pickFirstFailing (const (Just 42)) whatever
--   = Just ((), (), 42)
--
-- and then we would have
--
--   fmap (simplifyDepthFirst (const (const [])) (const (Just 42))) (Just ((), (), 42))
--
-- which, given the (const (const [])) simplification, should rewrite to
--
--   fmap id (Just ((), (), 42)) = Just ((), (), 42)
--
shouldSimplify_4 :: Maybe ((), (), Int)
shouldSimplify_4 = runStrategy unitSearchStrategy () () (const (Just 42))

-- Goal for this one: simplifies to Nothing, just like shouldSimplify_3.
shouldSimplify_5 :: Maybe (Seed, (), (), Int)
shouldSimplify_5 = searchSequential_ unitSearchStrategy () () (const (const Nothing)) (splitN 100 (mkSMGen 42))

-- Goal for this one: simplifies to the first seed.
-- This won't simplify unless we show GHC that the list is definitely a cons.
-- It won't figure that out on its own.
-- - If GHC knows the list is empty then it will simplify to Nothing
-- - If GHC knows the list is cons then it will simplify if the test is const Just
-- - If GHC knows the test is const Nothing then it will simplify.
-- What's the missing link? If it's const Just, but GHC can't determine the head
-- of the list statically, then it of course can't rewrite it, and will have to
-- generate code to compute the head.
--
-- So what to do about this? It's nice to be able to give any [Seed] to the
-- search. One solution would be to export a splitN which GHC _can_ inline to
-- a cons.
shouldSimplify_6 :: Maybe (Seed, (), (), Int)
shouldSimplify_6 = searchSequential_ unitSearchStrategy () () (const (const (Just 42))) (splitN 99 (mkSMGen 42))
-- Does not simplify
--   shouldSimplify_6 = searchSequential_ unitSearchStrategy () () (const (const (Just 42))) (take 100 (splitUnfold (mkSMGen 42)))
-- Does simplify
--   shouldSimplify_6 = searchSequential_ unitSearchStrategy () () (const (const (Just 42))) (mkSMGen 42 : take 99 (splitUnfold (mkSMGen 43)))
-- Does simplify
--   shouldSimplify_6 = searchSequential_ unitSearchStrategy () () (const (const (Just 42))) []

-- Let's put it all together into a quickCheck of the following unit test.
--
-- It checks that the sum of 1 to n is n*(n+1)/2, but only at a single given
-- number. It doesn't use any randomness, and the search space is the unit
-- space, so it's a unit test.
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

-- main :: IO ()
-- main = quickCheck 100 (2*4096) exampleUnitTest >>= print
