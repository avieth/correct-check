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
  , assert
  , that
  , runConjunction

  -- * Generic semigroupoid compositions
  , Composite (..)
  , toKleisli
  , then_

  -- * Re-export
  , Natural
  ) where

import Prelude hiding (id, (.))
import Control.Arrow (Kleisli (..))
import Control.Category
import Numeric.Natural (Natural)
import Data.Functor.Contravariant

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
  contramap f (Expectation r v) = Expectation r (contramap f v)

-- | Indicates that some type acts as a refutation of an assertion. This will
-- often be Text with a human-readable explanation.
newtype Refutation refutation = Refutation { getRefutation :: refutation }

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

infixr 0 .&

(.&) :: Conjunction f t -> Conjunction f t -> Conjunction f t
(.&) = And

assert :: f t -> Conjunction f t
assert = Assert

that :: refutation -> (t -> dynamic -> result -> Bool) -> Expectation refutation dynamic result t
that r f = Expectation (Refutation r) (Verification f)

-- TODO could probably speed things up by rewriting Conjunction in CPS. In
-- practice, all of the conjunctions will be known statically and should be
-- inlined.
runConjunction :: Semigroup s => (f t -> s) -> Conjunction f t -> s
runConjunction k (Assert f) = k f
runConjunction k (And l r) = runConjunction k l <> runConjunction k r

data Composite f s t where
  Then :: Composite f s t -> Composite f t u -> Composite f s u
  Check :: f s t -> Composite f s t

toKleisli :: Monad m => (forall x y . f x y -> Kleisli m x y) -> Composite f s t -> Kleisli m s t
toKleisli k (Then l r) = toKleisli k l >>> toKleisli k r
toKleisli k (Check it) = k it

-- Composite is not a category: we don't want to have a unit.
-- But it is a semigroupoid, and we do want to be able to composite it.
then_ :: Composite f s t -> Composite f t u -> Composite f s u
then_ = Then
