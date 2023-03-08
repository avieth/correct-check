-- TODO this module isn't actually useful, and its name suggests importance.
-- Remove?
module Property
  ( Property (..)
  ) where

import Data.Functor.Contravariant
import Data.Word (Word32)
import qualified Space.Random as Random
import qualified Space.Search as Search
import Types

-- | Called Property for consistency with the hedgehog and quickcheck. It's the
-- same idea, just factored differently: the domain determines how to generate
-- inputs for the separate test part which determines expectations.
data Property state space dynamic result refutation t = Property
  { propertyDomain :: Domain state space dynamic
  , propertyTest :: Test dynamic result refutation t
  }

instance Contravariant (Property state space specimen result refutation) where
  contramap f p = p
    { propertyTest = contramap f (propertyTest p)
    }

-- TODO move to an examples module.

-- | The list-reverse property.
--
-- Forced to choose a list element here: Word32.
example_property :: Property () Natural [Word32] [Word32] String ()
example_property = Property
  { propertyDomain = Domain
      { search = Search
          { strategy = Search.linearSearchStrategy 10 0 99
          , initialState = ()
          , minimalSpace = 0
          }
      , generate = Random.listOf Random.parameter Random.genWord32
      }
  , propertyTest = Test
      { subject = Subject $ \() lst -> reverse lst
      , expectations = example_expectations
      }
  }


-- This example shows how definig expectations doesn't necessarily make sense
-- in a vacuum. Each assertion in the conjunction must agree about what the
-- verification parameters mean, and that constraints what the subject can be.
--
-- This one, for instance, basically implies what the subject is: it reverses
-- the generated list.
--
-- In practice, expectations would only be defined once a subject is known.
example_expectations :: Eq a => Expectations String [a] [a] ()
example_expectations =
     (that "reverse . reverse = id" $ \() lst lst' -> lst == reverse lst')
  .& (that "last = head . reverse"  $ \() lst lst' -> head lst == last lst')
  where
    head [] = Nothing
    head (x:_) = Just x
    last [] = Nothing
    last [x] = Just x
    last (_:xs) = last xs
