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

-- | Maybe but with a different Pretty instance that actually shows a Nothing
-- value.
newtype Optional t = Optional { getOptional :: Maybe t }
instance Pretty t => Pretty (Optional t) where
  pretty (Optional (Just t)) = pretty t
  pretty (Optional Nothing) = fromString "< nothing >"

-- | A family of tests for any partial order and strategy of agreeing types (the
-- static part).
test_complication :: Test (DynamicPart state space) (Optional space) Increases (StaticPart state space)
test_complication = Test
  { subject = Subject $ \spart dpart -> Optional $
      fmap snd (complicate (searchStrategy spart) (seed dpart) (state dpart) (space dpart))
  , expectations =
      that Increases $ \spart dpart (Optional result) -> False &&
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

bigDomain :: Domain () () (DynamicPart () Natural)
bigDomain = domain trivialSearch generate
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
    gen = (,) () <$> genNatural 0 (2^32)

localConfig :: LocalConfig
localConfig = defaultLocalConfig
  { localParallelism = noParallelism -- nCapabilities
  , localRandomSamples = 1024 * 4096
  }

main :: IO ()
main = do
  result <- composite defaultGlobalConfig $
    declare viaPrettyRenderer "Linear search complication"   test_complication   $ \complication ->
    declare viaPrettyRenderer "Linear search simplification" test_simplification $ \simplification ->
    compose $ do
      check (serially 99)
        complication
        bigDomain
        (StaticPart "Linear(1,0,99)" (linearSearchStrategy 1 0 99) ordPartialOrder)
      check (serially 99)
        simplification
        bigDomain
        (StaticPart "Linear(1,0,99)" (linearSearchStrategy 1 0 99) ordPartialOrder)
      pure ()
  printTestResult result
