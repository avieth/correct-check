module Space.Ordered
  (
  -- * Non-decreasing functions
    NonDecreasing (..)
  , constant

  , PartialOrder
  , isLessThan
  , isGreaterThan
  , unitPartialOrder
  , ordPartialOrder

  , Product (..)
  , productPartialOrder
  ) where

import Prelude hiding (id, (.))
import Control.Category

-- | Intended as a signpost to help make it easier to ensure only non-decreasing
-- functions are used to contramap the space parameter of a Gen.
--
-- Meaningful only in reference to particular PartialOrder definitions on
-- space and subspace.
newtype NonDecreasing space subspace = NonDecreasing
  { nonDecreasing :: space -> subspace }

instance Category NonDecreasing where
  id = NonDecreasing id
  NonDecreasing left . NonDecreasing right = NonDecreasing (left . right)

{-# INLINE constant #-}
constant :: t -> NonDecreasing s t
constant = NonDecreasing . const

type PartialOrder t = t -> t -> Maybe Ordering

-- | True if the first argument is less than the first, for consistency
-- with 'compare'
isLessThan :: PartialOrder t -> t -> t -> Bool
isLessThan pord l r = case pord l r of
  Just LT -> True
  _ -> False

-- | True if the first argument is greater than the first, for consistency
-- with 'compare'
isGreaterThan :: PartialOrder t -> t -> t -> Bool
isGreaterThan pord l r = case pord l r of
  Just GT -> True
  _ -> False

-- | The only sensible partial order of ().
unitPartialOrder :: PartialOrder ()
unitPartialOrder = \_ _ -> Just EQ

-- | Use an Ord instance to give a partial order (it's actually total).
ordPartialOrder :: Ord t => PartialOrder t
ordPartialOrder = \l r -> Just (compare l r)

data Product s t where
  (:*:) :: s -> t -> Product s t

-- | Partial order of products: both components must have the same comparison.
productPartialOrder :: PartialOrder s -> PartialOrder t -> PartialOrder (Product s t)
productPartialOrder fstPord sndPord = \(leftA :*: leftB) (rightA :*: rightB) -> do
  f <- fstPord leftA rightA
  s <- sndPord leftB rightB
  case (f, s) of
    (EQ, EQ) -> Just EQ
    (LT, LT) -> Just LT
    (GT, GT) -> Just GT
    (_ , _ ) -> Nothing
