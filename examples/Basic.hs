{-# LANGUAGE StaticPointers #-}

module Basic where

import Data.Word
import Composite
import Pretty
import Quick.Check
import Space.Random as Random
import Space.Search as Search
import Types
import Unit

-- | Generic round-trip test. The input is some @a@, but the result keeps the
-- intermediate value, rather than computing the result in the expectations.
-- That makes for a more useful explanation of a failure.
roundtripTest :: (a -> a -> Bool) -> (a -> b) -> (b -> Maybe a) -> Test String a (b, Maybe a)
roundtripTest equal there back = Test
  { subject = Subject $ \a -> let mid = there a in (mid, back mid)
  , expectations = that "back . there = Just" $ \a (_b, ma) -> case ma of
      Nothing -> False
      Just a' -> equal a a'
  }

-- | Instantiate the 'roundtripTest' at particular values. This can be made
-- into a static pointer.
roundtripSeedHex :: Test String Seed (String, Maybe Seed)
roundtripSeedHex = roundtripTest (==) showSeedHex readSeedHex

-- | Since there is only one domain we ever want to use for 'roundtripSeedHex',
-- we can go ahead and define it top-level. The domain is simple: just give a
-- random seed. There is no complexity-order and therefore no notion of
-- shrinking (every seed is equally complex as every other).
domainSeed :: Domain () Seed
domainSeed = Domain
  { search = trivialStrategy
  , generate = genSeed
  }

-- | The famous list-reverse test.
--
-- No accompanying domain is given, since this one is quite generic and there is
-- no obvious "best domain" to apply.
listReverseTest :: Eq a => Test String [a] [a]
listReverseTest = Test
  { subject = Subject $ \lst -> if length lst >= 2 then lst ++ reverse lst else reverse lst
    -- ^ Deliberately wrong, for demonstration.
  , expectations = that "reverse . reverse = id" $ \lst lst' -> lst == reverse lst'
  }

-- Need to pick a type for the list reverse test, in order to be able to
-- make it static.
listReverse :: TestDeclaration [Int]
listReverse = declare "List reverse" renderTestViaPretty (static (listReverseTest @Int))

main :: IO ()
main = do
  -- quickCheck the seed roundtrip. Could be useful in GHCi.
  quickCheck 100 roundtripSeedHex domainSeed >>= print
  -- Can also quickCheck a unit test, which is a bit silly but also interesting
  -- because GHC will simplify this and only run the test once, since it doesn't
  -- depend upon the random seed.
  quickCheck 100 unitTest (unitDomain ((42 :: Int) === 42)) >>= print
  -- One particular domain to quickCheck the list reverse over.
  let w32ListDomain :: Domain Natural [Word32]
      w32ListDomain = Domain (powerStrategy 2 0 100) (listOf parameter genWord32)
  quickCheckParallel 8 100 listReverseTest w32ListDomain >>= print
  -- Now a composite test. Each test we use must be made into a static pointer,
  -- which helps to ensure it is actually reproducible, i.e. it doesn't have
  -- any hidden state in its closure.
  result <- composite (allowAsynchronousChecks 2 defaultGlobalConfig) $ do
    -- A domain can be made dynamically, with IO; the important thing is that
    -- the test itself is static.
    let intDomain :: Domain Natural [Int]
        intDomain = Domain (linearStrategy 10 0 256) (listOf parameter (fromIntegral <$> genInteger 0 4096))
    _ <- check (serially 64) renderDomainViaPretty listReverse intDomain
    -- It's possible to opt-in to running the check in the background while
    -- the IO continues. This can be useful when searching large domains
    -- over expensive tests: the rest of the composite test does not need to
    -- stop and wait for it.
    -- Important to render via show, because the pretty instance is defined by
    -- showSeedHex, which is under test.
    let declaredRoundtripSeedHex :: TestDeclaration Seed
        declaredRoundtripSeedHex = declare "Rountrip Seed Hex" renderTestViaShow (static roundtripSeedHex)
    async <- checkAsync (inParallel 1024) renderDomainViaPretty declaredRoundtripSeedHex domainSeed
    checkUnitTest (assertTrue "2plus2" (2 + 2 == 4))
    b <- awaitCheck async
    -- Don't _need_ to await a check in order for it to finish, but it can
    -- still be useful for coordination, and also if you want to know whether
    -- it passed.
    pure ()
  printTestResult result
