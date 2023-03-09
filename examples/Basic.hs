module Basic where

import Data.Word
import Composite
import Pretty
import Quick.Check
import Space.Random as Random
import Space.Search as Search
import Types
import Unit

-- | Generic round-trip test. The input is some `a`, but the result keeps the
-- intermediate value, rather than computing the result in the expectations.
-- That makes for a more useful explanation of a failure.
roundtripTest :: (a -> a -> Bool) -> (a -> b) -> (b -> Maybe a) -> Test String a (b, Maybe a)
roundtripTest equal there back = Test
  { subject = Subject $ \a -> let mid = there a in (mid, back mid)
  , expectations = that "back . there = Just" $ \a (_b, ma) -> case ma of
      Nothing -> False
      Just a' -> equal a a'
  }

listReverseTest :: Eq a => Test String [a] [a]
listReverseTest = Test
  { subject = Subject $ \lst -> reverse lst
  , expectations = that "reverse . reverse = id" $ \lst lst' -> lst == reverse lst'
  }

domainList :: Domain Natural [Word32]
domainList = Domain
  { search = powerStrategy 2 0 100
  , generate = listOf parameter genWord32
  }

domainSeed :: Domain () Seed
domainSeed = Domain
  { search = trivialStrategy
  , generate = genSeed
  }

roundtripSeedHex :: Test String Seed (String, Maybe Seed)
roundtripSeedHex = roundtripTest (==) showSeedHex readSeedHex

main :: IO ()
main = do
  quickCheckParallel 8 100 listReverseTest domainList >>= print
  -- GHC will simplify this and only run the test once, since it doesn't
  -- depend upon the random seed.
  quickCheck 100 unitTest (unitDomain ((42 :: Int) === 42)) >>= print
  quickCheck 100 roundtripSeedHex domainSeed >>= print
  result <- composite defaultGlobalConfig $
    withUnitTest $ \unitTest ->
    -- Important to render via show, because the pretty instance is defined by
    -- showSeedHex, which is under test.
    declare renderTestViaShow "Rountrip Seed Hex" roundtripSeedHex $ \roundtripSeedHex ->
    compose $ do
      unitTest (assertTrue "2plus2" (2 + 2 == 4))
      check (inParallel 1024) renderDomainViaPretty roundtripSeedHex domainSeed
      pure ()
  printTestResult result
