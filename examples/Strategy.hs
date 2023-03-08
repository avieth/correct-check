module Strategy where

-- This example shows how we can property test search strategies, to check that
-- they respect an ordering.

import Data.Maybe (fromMaybe)
import Composite
import Property
import Space.Ordered
import Space.Random
import Space.Search
import Types

-- | The randomly-generated part of the test.
data DynamicPart state space = DynamicPart
  { state :: !state
  , space :: !space
  , seed :: !Seed
  }
  deriving (Show)

instance (Pretty state, Pretty space) => Pretty (DynamicPart state space) where
  pretty dpart = vsep
    [ fromString "State" <+> pretty (state dpart)
    , fromString "Point" <+> pretty (space dpart)
    , fromString "Seed " <+> prettySeedHex (seed dpart)
    ]

data StaticPart state space = StaticPart
  { showStaticPart :: String -- ^ Just for a show instance
  , searchStrategy :: Strategy state space
  , partialOrder :: PartialOrder space
  }

instance Show (StaticPart state space) where
  show = showStaticPart

instance (Pretty state, Pretty space) => Pretty (StaticPart state space) where
  pretty = fromString . showStaticPart

data Increases = Increases
  deriving (Show)

instance Pretty Increases where
  pretty _ = fromString "Increases"

data Decreases = Decreases
  deriving (Show)

instance Pretty Decreases where
  pretty _ = fromString "Decreases"

-- | A family of tests for any partial order and strategy of agreeing types (the
-- static part).
test_complication :: Test (DynamicPart state space) (Maybe space) Increases (StaticPart state space)
test_complication = Test
  { subject = Subject $ \spart dpart ->
      fmap snd (complicate (searchStrategy spart) (seed dpart) (state dpart) (space dpart))
  , expectations =
      that Increases $ \spart dpart result -> False &&
        -- The test passes vacuously if the result is Nothing (there was no
        -- more complex thing chosen).
        --
        -- flip it because, like 'compare', 'isGreaterThan pord' says whether
        -- the first argument is greater than the second. Also, since this is
        -- a partial order, we can't change it to isLessThan.
        fromMaybe True (fmap (flip (isGreaterThan (partialOrder spart)) (space dpart)) result)
  }

test_simplification :: Test (DynamicPart state space) [space] Decreases (StaticPart state space)
test_simplification = Test
  { subject = Subject $ \spart dpart ->
      fmap snd (simplify (searchStrategy spart) (seed dpart) (state dpart) (space dpart))
  , expectations = that Decreases $ \spart dpart result -> False &&
      all (flip (isLessThan (partialOrder spart)) (space dpart)) result
  }

-- To make properties, we'll need a domain.
-- Since we're testing search strategies, it only makes sense to use the strategy
-- that is so obvious it need not be tested.

domainFor :: Arbitrary (state, space) -> Domain () () (DynamicPart state space)
domainFor gen = Domain
  { search = trivialSearch
  , generate = do
      (state, space) <- gen
      seed <- genSeed
      pure $ DynamicPart
        { state = state
        , space = space
        , seed = seed
        }
  }

property_complication :: Property () () (DynamicPart () Natural) (Maybe Natural) Increases (StaticPart () Natural)
property_complication = Property
  { domain = domainFor g
  , test = test_complication
  }

property_simplification :: Property () () (DynamicPart () Natural) [Natural] Decreases (StaticPart () Natural)
property_simplification = Property
  { domain = domainFor g
  , test = test_simplification
  }

localConfig :: LocalConfig
localConfig = defaultLocalConfig
  { localParallelism = noParallelism -- nCapabilities
  , localRandomSamples = 1024 * 4096
  }

g :: Arbitrary ((), Natural)
g = (,) () <$> genNatural 0 99

main :: IO ()
main = do
  -- Q: does it make sense to give the domain in the declaration??
  -- Maybe not! Should be able to apply any domain within the check.
  -- True enough. You should have to give
  -- - The LocalConfig
  -- - A name
  -- - The Test dynamic result refutation static
  -- - The Renderer state space spcimen result refutation static
  result <- composite defaultGlobalConfig $
    declare "Linear search complication"   property_complication   viaPrettyRenderer localConfig $ \linearComplication ->
    declare "Linear search simplification" property_simplification viaPrettyRenderer localConfig $ \linearSimplification ->
    compose $ do
      -- FIXME seems more appropriate to take the strategy as the static part,
      -- under some fixed order.
      check linearComplication   (StaticPart "Linear(1,0,99)" (linearSearchStrategy 1 0 99) ordPartialOrder)
      check linearSimplification (StaticPart "Linear(1,0,99)" (linearSearchStrategy 1 0 99) ordPartialOrder)
      pure ()
  printTestResult result
