-- |
--
-- Unit testing definitions can be expressed in property test style, and this
-- shows that they are a kind of "degenerate" case, because reproducing a unit
-- test failure is always trivial: you just give False or check equality of two
-- values that are known to not be equal.

{-# LANGUAGE StaticPointers #-}

module Unit
  ( -- * Unit Test and Domain
    unitTest
  , unitDomain
  , withUnitTest
  , Assertion (..)
  , Result (..)

    -- * hunit style assertions
  , assertFailure
  , assertTrue
  , assertEqual
  , (===)

    -- * hspec-expectations style assertions
  , shouldReturn
  , shouldNotReturn
  , shouldThrow
  ) where

import Control.Exception (Exception, try)
import Composite
import Location
import Pretty
import Space
import Types hiding (Assertion (..))

-- | To serve as a value for the assertion type of the unit test.
data UnitTest = UnitTest
  deriving (Show)

instance Pretty UnitTest where
  pretty _ = fromString "Unit test"

-- | Input to a unit test.
data Assertion where
  AssertFailure :: String -> Assertion
  AssertTrue :: String -> Bool -> Assertion
  AssertEqual :: (Eq a, Pretty a) => String -> a -> a -> Assertion

instance Show Assertion where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

instance Pretty Assertion where
  pretty (AssertFailure str) = fromString "Failure" <+> fromString str
  pretty (AssertTrue str b) = fromString "AssertTrue" <+> fromString str <+> pretty b
  pretty (AssertEqual str a a') = fromString "AssertEqual" <+> fromString str <+> pretty a <+> pretty a'

-- | The result of a unit test.
data Result where
  AssertionFailed :: Result
  AssertionPassed :: Result

instance Show Result where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

instance Pretty Result where
  pretty AssertionFailed = fromString "Assertion failed"
  pretty AssertionPassed = fromString "Assertion passed"

assertFailure :: String -> Assertion
assertFailure = AssertFailure

assertTrue :: String -> Bool -> Assertion
assertTrue = AssertTrue

assertEqual :: (Eq a, Pretty a) => String -> a -> a -> Assertion
assertEqual = AssertEqual

(===) :: (Eq a, Pretty a) => a -> a -> Assertion
(===) = assertEqual ""

-- | A unit test is trivial: the test is already done before it is given to the
-- test subject, its result compressed into a `Bool` and possibly a `String`
-- explanation. Reproducing such a test is not very enlightening.
unitTest :: Test UnitTest Assertion Result
unitTest = Test
  { subject = Subject $ \assertion -> case assertion of
      AssertFailure _ -> AssertionFailed
      AssertTrue _ b -> if b then AssertionPassed else AssertionFailed
      AssertEqual _ a a' -> if a == a' then AssertionPassed else AssertionFailed
  , expectations = that UnitTest $ \_ result -> case result of
      AssertionFailed -> False
      AssertionPassed -> True
  }

-- | There's only one sensible domain for a unit test: no randomness and no
-- searching.
unitDomain :: Assertion -> Domain () Assertion
unitDomain assertion = Domain
  { search = trivialStrategy
  , generate = pure assertion
  }

-- | Declares unit test and gives a canonical way to run it: use the
-- 'unitDomain' and only one random seed.
--
-- Although this may be useful in practice, it probably will not lead to a
-- reproducible test.
{-# INLINE withUnitTest #-}
withUnitTest :: HasCallStack
             => ((HasCallStack => Assertion -> Composite check Bool) -> Declaration check)
             -> Declaration check
withUnitTest k = withFrozenCallStack (declare renderTestViaPretty "Unit" (static unitTest) $ \runHUnit ->
  -- Freeze the call stack in the continuation as well, so that check will use
  -- the one from k, which will stand in as the `check` function inside the
  -- composite.
  k (\assertion -> withFrozenCallStack (check (serially 1) renderDomainViaPretty runHUnit (unitDomain assertion))))

-- One thing to note about these HUnit tests is that the content of the test
-- itself is always trivial (except maybe in the 'assertEqual' case, which does
-- an _almost_ trivial amount of the work).
--
-- And so when an HUnit assertion fails as part of a composite test, the
-- information about the failure is quite limited: it shows what kind of
-- assertion failed, possibly some string chosen by the assertion call site, and
-- in the case of an equality failure, the values that are not equal.
-- The test is reproducible, but in a degenerate sort of way: you just have to
-- give the same Bool. The _real content_ of the test was already computed,
-- prior to and outside of the HUnit test itself.
--
-- This is just a fact about HUnit style testing in general. It's arguably more
-- convenient, because the programmer does not have to think too hard about what
-- they are testing. But on the other hand it does not encourage good software
-- quality, for that same exact reason.


-- hspec-expectations is much the same, since it uses HUnit, but it brings some
-- notions that we can't directly express as property tests:
--
-- - shouldReturn :: Ea a => IO a -> a -> Assertion
--   shouldReturn action expected = action >>= \actual -> assertEqual actual expected
--
-- - shouldNotReturn :: Eq a => IO a -> a -> Assertion
--   shouldNotReturn action notExpected = action >>= \actual -> assertBool (actual /= notExpected)
--
-- - shouldThrow :: Exception e => IO a -> Selector e -> Assertion
--   shouldThrow io = try io >>= \ex -> <too long to write out>
--
-- NB: there is no shouldNotThrow, since that's sort of implicit in every
-- hspec-expectation / HUnit test, since an exception is a failure.
--
-- They do IO, so of course they can't be property tests. Instead, they would
-- always appear in composites, and they would take an HUnit runner as an
-- argument.

shouldReturn :: (Eq a, Pretty a) => (Assertion -> Composite check r) -> IO a -> a -> Composite check r
shouldReturn hunit io expected = effect_ io >>= \actual ->
  hunit (assertEqual "" actual expected)

shouldNotReturn :: (Eq a, Pretty a) => (Assertion -> Composite check r) -> IO a -> a -> Composite check r
shouldNotReturn hunit io expected = effect_ io >>= \actual ->
  hunit (assertTrue "" (actual /= expected))

shouldThrow :: (Exception e) => (Assertion -> Composite check r) -> IO a -> (e -> Bool) -> Composite check r
shouldThrow hunit io p = effect_ (try io) >>= \r -> case r of
  Right _ -> hunit (assertFailure "did not get expected excetion")
  Left e -> hunit (assertTrue "predicate failed on expected exception" (p e))
