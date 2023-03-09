module Strategy where

-- This example shows how we can property test search strategies, to check that
-- they respect an ordering.

import Data.Maybe (fromMaybe)
import Composite
import Check
import Pretty
import Space.Ordered
import Space.Random
import Space.Search
import Types

data Point state space = Point
  { state :: !state
  , space :: !space
  , seed :: !Seed
  }
  deriving (Show)

instance (Pretty state, Pretty space) => Pretty (Point state space) where
  pretty (Point state space seed) = vsep
    [ fromString "State" <+> pretty state
    , fromString "Point" <+> pretty space
    , fromString "Seed " <+> prettySeedHex seed
    ]

data Increases = Increases
  deriving (Show)

instance Pretty Increases where
  pretty _ = fromString "Increases"

data Decreases = Decreases
  deriving (Show)

instance Pretty Decreases where
  pretty _ = fromString "Decreases"

-- | Maybe but with a different Pretty instance that actually shows a Nothing
-- value.
newtype Optional t = Optional { getOptional :: Maybe t }
instance Pretty t => Pretty (Optional t) where
  pretty (Optional (Just t)) = pretty t
  pretty (Optional Nothing) = fromString "< nothing >"

-- | A family of tests for any partial order and strategy of agreeing types (the
-- static part).
--
-- FIXME would be better to give a result type that includes the partial order
-- computation. It should be computed as part of the subject, and the verification
-- should be a simple pattern match.
testComplication :: PartialOrder space
                 -> SearchDef state space
                 -> Test Increases (Point state space) (Optional space)
testComplication pord searchDef = Test
  { subject = Subject $ \(Point state space seed) -> Optional $
      fmap snd (complicate searchDef seed state space)
  , expectations =
      that Increases $ \(Point state space seed) (Optional result) ->
        -- The test passes vacuously if the result is Nothing (there was no
        -- more complex thing chosen).
        --
        -- flip it because, like 'compare', 'isGreaterThan pord' says whether
        -- the first argument is greater than the second. Also, since this is
        -- a partial order, we can't change it to isLessThan.
        fromMaybe True (fmap (flip (isGreaterThan pord) space) result)
  }

testSimplification :: PartialOrder space
                   -> SearchDef state space
                   -> Test Decreases (Point state space) [space]
testSimplification pord searchDef = Test
  { subject = Subject $ \(Point state space seed) ->
      fmap snd (simplify searchDef seed state space)
  , expectations = that Decreases $ \(Point sstate space seed) result -> 
      all (flip (isLessThan pord) space) result
  }

-- A domain used to test complication/simplification? Only the trivial strategy
-- could be appropriate, since it is too obvious to test.
domain :: Domain () (Point () Natural)
domain = Domain
  { search = trivialStrategy
  , generate = do
      (state, space) <- (,) () <$> genNatural 0 upperBound
      seed <- genSeed
      pure $ Point
        { state = state
        , space = space
        , seed = seed
        }
  }
    where
    gen :: Arbitrary ((), Natural)
    gen = (,) () <$> genNatural 0 (2^32)

localConfig :: LocalConfig
localConfig = defaultLocalConfig
  { localParallelism = nCapabilities
  , localRandomSamples = 1024 * 4096
  }

linear = linearSearchDef 1 0 upperBound

upperBound :: Natural
upperBound = fromIntegral (maxBound :: Int)

main :: IO ()
main = do
  result <- composite defaultGlobalConfig $
    declare renderTestViaPretty "Linear search complication"   (testComplication   ordPartialOrder linear) $ \complication ->
    declare renderTestViaPretty "Linear search simplification" (testSimplification ordPartialOrder linear) $ \simplification ->
    compose $ do
      check (inParallel 4096) renderDomainViaPretty complication domain
      check (inParallel 4096) renderDomainViaPretty simplification domain
      pure ()
  printTestResult result
