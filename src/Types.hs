module Types
  ( Test (..)

  -- * Test subject and verification
  , Subject (..)
  , Verification (..)
  , passes
  , fails

  -- * Expectations
  , Expectation (..)
  , Expectations
  , Assertion (..)
  , Conjunction (..)
  , (.&)
  , that
  , runConjunction

  -- * Domains
  , Domain (..)

  -- * Re-export
  , Strategy (..)
  , Gen (..)
  , Natural
  ) where

import Numeric.Natural (Natural)
import Location
import Space.Random as Random
import Space.Search as Search

-- | A test subject. Any pure function.
newtype Subject specimen result = Subject
  { runSubject :: specimen -> result }

-- | Verification of a Subject. Defining properties are 'passes' and 'fails'.
newtype Verification specimen result = Verification
  { runVerification :: specimen -> result -> Bool }

passes :: Subject specimen result -> Verification specimen result -> specimen -> Bool
passes sub ver s = runVerification ver s (runSubject sub s)

fails :: Subject specimen result -> Verification specimen result -> specimen -> Bool
fails sub ver = not . passes sub ver

data Expectation assertion specimen result where
  Expectation :: Assertion assertion -> Verification specimen result -> Expectation assertion specimen result

type Expectations assertion specimen result = Conjunction (Expectation assertion specimen result)

-- | A non-empty list but as an arbitrary tree so that it's easier to write.
data Conjunction s where
  And :: Conjunction s -> Conjunction s -> Conjunction s
  Assert :: s -> Conjunction s

instance Functor Conjunction where
  fmap f (And l r) = And (fmap f l) (fmap f r)
  fmap f (Assert s) = Assert (f s)

instance Semigroup (Conjunction s) where
  (<>) = (.&)

(.&) :: Conjunction s -> Conjunction s -> Conjunction s
(.&) = And

infixr 1 .&

-- | Use this to define expectations so that you get a source location.
that :: HasCallStack
     => assertion
     -> (specimen -> result -> Bool)
     -> Conjunction (Expectation assertion specimen result)
that r f = Assert (Expectation (Assertion (srcLocOf callStack) r) (Verification f))

-- TODO could probably speed things up by rewriting Conjunction in CPS. In
-- practice, all of the conjunctions will be known statically and could be
-- inlined.
runConjunction :: Semigroup s => (t -> s) -> Conjunction t -> s
runConjunction f (Assert s) = f s
runConjunction f (And l r) = runConjunction f l <> runConjunction f r

data Assertion assertion = Assertion
  { assertionLocation :: MaybeSrcLoc
  , assertionLabel :: assertion
  }

instance Show assertion => Show (Assertion assertion) where
  show assertion = mconcat
    [ show (assertionLabel assertion)
    , " at "
    , showMaybeSrcLoc (assertionLocation assertion)
    ]

-- | A subject and expectations.
data Test assertion specimen result = Test
  { subject :: Subject specimen result
  , expectations :: Expectations assertion specimen result
  }

-- What's the other part? We would want to say that a Property is a Domain
-- applied to a Test, as in search through this Domain for a counterexample to
-- this Test.
--
-- With that guiding principle, what should be a domain?

data Domain space t = Domain
  { search :: Strategy space
  , generate :: Gen space t
  }
