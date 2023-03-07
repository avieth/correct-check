module Types
  (
  -- * Property definitions
    Subject (..)
  , Verification (..)
  , Expectation (..)
  , Refutation (..)
  , Expectations

  -- * Generic conjunctions
  , Conjunction (..)
  , (.&)
  , that
  , runConjunction

  -- * Counterexample
  , Counterexample (..)

  -- * Re-export
  , Natural
  ) where

import Prelude hiding (id, (.))
import Data.Functor.Contravariant
import Numeric.Natural (Natural)
import Location (HasCallStack, MaybeSrcLoc, srcLocOf, callStack)
import Space.Random (Seed)
import Data.List.NonEmpty (NonEmpty)

-- | A test subject: given the static and dynamic (from the search space) parts,
-- come up with a result. Meaningful in relation to 'Verification' and
-- 'Expectation'.
newtype Subject dynamic result static = Subject
  { runSubject :: static -> dynamic -> result }

instance Contravariant (Subject dynamic result) where
  contramap f (Subject k) = Subject $ \param -> k (f param)

-- | Whereas a 'Subject d r s' gives a functional relation `(s, d, r)`,
-- 'Verification d r s' gives a subset of that relation, to be interpreted as
-- every tuple which is _expected_ to be part of the functional relation. A
-- property test will check that these relations coincide.
newtype Verification dynamic result static = Verification
  { runVerification :: static -> dynamic -> result -> Bool }

instance Contravariant (Verification dynamic result) where
  contramap f (Verification k) = Verification $ \param -> k (f param)

-- | Pairs a refutation with a function which checks whether that refutation has
-- been shown. To keep consistent with other testing library metaphors, a value
-- of True means it has not been refuted (the test passes).
data Expectation refutation dynamic result static where
  Expectation :: Refutation refutation -> Verification dynamic result static -> Expectation refutation dynamic result static

instance Contravariant (Expectation refutation dynamic result) where
  -- The source location of the original definition doesn't change after a
  -- contramap.
  contramap f (Expectation r v) = Expectation r (contramap f v)

-- | Indicates that some type acts as a refutation of an assertion. This will
-- often be Text with a human-readable explanation.
data Refutation refutation = Refutation
  { refutationLocation :: MaybeSrcLoc
  , refutationLabel :: refutation
  }

deriving instance Show refutation => Show (Refutation refutation)

type Expectations refutation dynamic result = Conjunction (Expectation refutation dynamic result)

-- | A non-empty list but as an arbitrary tree so that it's easier to write.
--
-- FIXME generalize this so it can be used to contain any contravariant functor.
-- Will be used for composite IO tests as well, holding properties at each spot.
data Conjunction f t where
  And :: Conjunction f t -> Conjunction f t -> Conjunction f t
  Assert :: f t -> Conjunction f t

instance Contravariant f => Contravariant (Conjunction f) where
  contramap f (And l r) = And (contramap f l) (contramap f r)
  contramap f (Assert e) = Assert (contramap f e)

instance Semigroup (Conjunction f t) where
  (<>) = (.&)

(.&) :: Conjunction f t -> Conjunction f t -> Conjunction f t
(.&) = And

infixr 1 .&

that :: HasCallStack => refutation -> (t -> dynamic -> result -> Bool) -> Conjunction (Expectation refutation dynamic result) t
that r f = Assert (Expectation (Refutation (srcLocOf callStack) r) (Verification f))

-- TODO could probably speed things up by rewriting Conjunction in CPS. In
-- practice, all of the conjunctions will be known statically and should be
-- inlined.
runConjunction :: Semigroup s => (f t -> s) -> Conjunction f t -> s
runConjunction k (Assert f) = k f
runConjunction k (And l r) = runConjunction k l <> runConjunction k r

-- | A counterexample produced by a property test. It gives the random seed and
-- search space, and also the value that was produced by the generator at this
-- point (even though it can be reproduced). It also has the non-empty set of
-- refutations, corresponding to the expectations which failed at this point.
data Counterexample space dynamic result refutation = Counterexample
  -- May as well be strict in all fields since they've already been computed
  -- or else we wouldn't have a counterexample.
  { randomSeed :: !Seed
  , searchPoint :: !space
  , dynamicPart :: !dynamic
  , resultPart :: !result
  , refutations :: !(NonEmpty (Refutation refutation))
  }

-- TODO remove this show instace; give a library with pretty-printers.
instance Show (Counterexample space dynamic result refutation) where
  show = const "Counterexample"
