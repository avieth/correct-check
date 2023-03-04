module Space.Ordered
  (
  -- * Non-decreasing functions
    NonDecreasing (..)
  , constant
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad (ap, replicateM)
import Numeric.Natural
import Data.Word
import System.Random.SplitMix (SMGen)
import qualified System.Random.SplitMix as SM

-- | Intended as a signpost to help make it easier to ensure only non-decreasing
-- functions are used to contramap the space parameter of a Gen.
newtype NonDecreasing space subspace = NonDecreasing { nonDecreasing :: space -> subspace }

instance Category NonDecreasing where
  id = NonDecreasing id
  NonDecreasing left . NonDecreasing right = NonDecreasing (left . right)

{-# INLINE constant #-}
constant :: t -> NonDecreasing s t
constant = NonDecreasing . const

-- TODO define various ordered space and non-decreasing functions.
