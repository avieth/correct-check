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

data StaticPart t = StaticPart (PartialOrder t)

instance Show (StaticPart t) where
  show _ = "< partial order function >"

data Increases = Increases
  deriving (Show)

data Decreases = Decreases
  deriving (Show)

-- | A family of tests for a given strategy, which can be instantiated at any
-- partial order (the static part).
test_complication :: Strategy state space
                  -> Test (DynamicPart state space) (Maybe space) Increases (StaticPart space)
test_complication strategy = Test
  { subject = Subject $ \spart dpart -> fmap snd (complicate strategy (seed dpart) (state dpart) (space dpart))
  , expectations =
      that Increases $ \(StaticPart pord) dpart result -> 
        -- The test passes vacuously if the result is Nothing (there was no
        -- more complex thing chosen).
        --
        -- flip it because, like 'compare', 'isGreaterThan pord' says whether
        -- the first argument is greater than the second. Also, since this is
        -- a partial order, we can't change it to isLessThan.
        fromMaybe True (fmap (flip (isGreaterThan pord) (space dpart)) result)
  }

test_simplification :: Strategy state space
                    -> Test (DynamicPart state space) [space] Decreases (StaticPart space)
test_simplification strategy = Test
  { subject = Subject $ \spart dpart -> snd <$> simplify strategy (seed dpart) (state dpart) (space dpart)
  , expectations = that Decreases $ \(StaticPart pord) dpart result -> all (flip (isLessThan pord) (space dpart)) result
  }

-- To make properties, we'll need a domain.
-- Since we're testing search strategies, it only makes sense to use the strategy
-- that is so obvious it need not be tested.

domainFor :: (forall x . Gen x (state, space)) -> Domain () () (DynamicPart state space)
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

property_complication :: Strategy state space
                      -> (forall x . Gen x (state, space))
                      -> Property () () (DynamicPart state space) (Maybe space) Increases (StaticPart space)
property_complication strategy gen = Property
  { domain = domainFor gen
  , test = test_complication strategy
  }

property_simplification :: Strategy state space
                        -> (forall x . Gen x (state, space))
                        -> Property () () (DynamicPart state space) [space] Decreases (StaticPart space)
property_simplification strategy gen = Property
  { domain = domainFor gen
  , test = test_simplification strategy
  }

localConfig :: LocalConfig
localConfig = defaultLocalConfig
  { localParallelism = nCapabilities
  , localRandomSamples = 1024 * 4096
  }

main :: IO ()
main = do
  result <- composite defaultGlobalConfig $
    declare "Linear search complication" (property_complication (linearSearchStrategy 1 0 99) ((,) () <$> genNatural 0 99)) viaShowRenderer localConfig $ \linearComplication ->
    declare "Linear search simplification" (property_simplification (linearSearchStrategy 1 0 99) ((,) () <$> genNatural 0 99)) viaShowRenderer localConfig $ \linearSimplification ->
    compose $ do
      -- FIXME seems more appropriate to take the strategy as the static part,
      -- under some fixed order.
      check linearComplication (StaticPart ordPartialOrder)
      check linearSimplification (StaticPart ordPartialOrder)
      pure ()
  printTestResult result
