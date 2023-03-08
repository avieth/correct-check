module Basic where

import Data.Word
import Property
import Space.Random as Random
import Space.Search as Search
import Types

-- | The list-reverse property.
--
-- Forced to choose a list element here: Word32.
example_property :: Property () Natural [Word32] [Word32] String ()
example_property = Property
  { domain = Domain
      { search = Search
          { strategy = Search.linearSearchStrategy 2 0 99
          , initialState = ()
          , minimalSpace = 0
          }
      , generate = Random.listOf Random.parameter Random.genWord32
      }
  , test = Test
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
