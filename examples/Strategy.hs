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

exampleDomain :: Domain () () (DynamicPart () Natural)
exampleDomain = domain trivialSearch generate
  where
    generate = do
      (state, space) <- gen
      seed <- genSeed
      pure $ DynamicPart
        { state = state
        , space = space
        , seed = seed
        }
    gen :: Arbitrary ((), Natural)
    gen = (,) () <$> genNatural 0 99

localConfig :: LocalConfig
localConfig = defaultLocalConfig
  { localParallelism = noParallelism -- nCapabilities
  , localRandomSamples = 1024 * 4096
  }

main :: IO ()
main = do
  -- Q: does it make sense to give the domain in the declaration??
  -- Maybe not! Should be able to apply any domain within the check.
  -- True enough. You should have to give
  -- - The LocalConfig
  -- - The `Renderer state space spcimen result refutation static`
  -- - A name
  -- - The `Test dynamic result refutation static`
  --
  result <- composite defaultGlobalConfig $
    declare viaPrettyRenderer "Linear search complication"   test_complication   $ \complication ->
    declare viaPrettyRenderer "Linear search simplification" test_simplification $ \simplification ->
    compose $ do
      -- FIXME seems more appropriate to take the strategy as the static part,
      -- under some fixed order.
      check (serially 99)
        complication
        exampleDomain
        (StaticPart "Linear(1,0,99)" (linearSearchStrategy 1 0 99) ordPartialOrder)
      check (serially 99)
        simplification
        exampleDomain
        (StaticPart "Linear(1,0,99)" (linearSearchStrategy 1 0 99) ordPartialOrder)
      pure ()
  printTestResult result
