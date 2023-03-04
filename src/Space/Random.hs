module Space.Random
  (
  -- * The Gen monad
    Gen (..)
  , sampleAt
  , ndmap
  , parameter

  -- * Various generators
  , genWord8
  , genWord16
  , genWord32
  , genWord64
  , genWord64Bounded
  , genInteger
  , listOf
  , listOfLength
  , listOfKnownLength

  -- * Things to do with splitmix generators
  , Seed
  , newSeedIO
  , split
  , splitN
  , SM.mkSMGen
  , SM.newSMGen

  -- * Useful for showing seeds on failing tests, and reloading them to rerun
  , showSeedHex
  , readSeedHex

  -- * Extra stuff
  , sampleIO
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad (ap, replicateM)
import Numeric (readHex, showHex)
import Numeric.Natural (Natural)
import Data.List (unfoldr)
import Data.Word
import System.Random.SplitMix (SMGen)
import qualified System.Random.SplitMix as SM
import Space.Ordered

-- | The splitmix generator is our random seed.
type Seed = SMGen

-- | `Seed -> space -> (Seed, t)` but in CPS.
--
-- The monad instance splits the seed, so that, for instance, in
--
-- @
--   replicateM n someGenerator
--   otherGenerator
-- @
--
-- otherGenerator is always run with the same seed, regardless of the value of n.
newtype Gen space t = Gen { unGen :: forall r. Seed -> space -> (Seed -> t -> r) -> r }

instance Functor (Gen space) where
  {-# INLINE fmap #-}
  fmap f (Gen k) = Gen $ \smgen space l -> k smgen space $ \smgen' t -> l smgen' (f t)

instance Applicative (Gen space) where
  {-# INLINE pure #-}
  pure t = Gen $ \smgen _space k -> k smgen t
  (<*>) = ap

instance Monad (Gen space) where
  return = pure
  {-# INLINE (>>=) #-}
  left >>= k = Gen $ \smgen space l -> split smgen $ \smgen1 smgen2 ->
    -- Essential to split here, so that the structure of the left term does
    -- not affect the seed that is given to the term after the bind.
    unGen left smgen1 space $ \_smgen3 t ->
      unGen (k t) smgen2 space l

{-# INLINE split #-}
split :: Seed -> (Seed -> Seed -> t) -> t
split smgen k = let (g1, g2) = SM.splitSMGen smgen in k g1 g2

{-# INLINE splitN #-}
splitN :: Natural -> Seed -> [Seed]
splitN n = take (fromIntegral n) . splitUnfold

{-# INLINE splitUnfold #-}
splitUnfold :: Seed -> [Seed]
splitUnfold = unfoldr (Just . SM.splitSMGen)

{-# INLINE parameter #-}
-- | 
-- @ 
--   fromParameter . constant = pure
-- @
fromParameter :: NonDecreasing space t -> Gen space t
fromParameter f = Gen $ \smgen space k -> k smgen (nonDecreasing f space)

parameter :: Gen space space
parameter = Gen $ \smgen space k -> k smgen space

-- | Use system entropy sources (uses System.Random.SplitMix.initSMGen)
newSeedIO :: IO Seed
newSeedIO = SM.initSMGen

{-# INLINE sampleAt #-}
sampleAt :: Seed -> space -> Gen space t -> t
sampleAt seed space gen = unGen gen seed space $ \_ t -> t

-- | Picks a random seed. Intended for playing around / testing this library.
sampleIO :: space -> Gen space t -> IO t
sampleIO space gen = newSeedIO >>= \smgen -> pure (sampleAt smgen space gen)

genWord8 :: Gen space Word8
genWord8 = Gen $ \smgen _ k -> let (w32, smgen') = SM.nextWord32 smgen in k smgen' (fromIntegral w32)

genWord16 :: Gen space Word32
genWord16 = Gen $ \smgen _ k -> let (w32, smgen') = SM.nextWord32 smgen in k smgen' (fromIntegral w32)

genWord32 :: Gen space Word32
genWord32 = Gen $ \smgen _ k -> let (w32, smgen') = SM.nextWord32 smgen in k smgen' w32

genWord64 :: Gen space Word64
genWord64 = Gen $ \smgen _ k -> let (w64, smgen') = SM.nextWord64 smgen in k smgen' w64

-- | Within these bounds, inclusive. See 'System.Random.SplitMix.nextInteger'.
genInteger :: Integer -> Integer -> Gen space Integer
genInteger a b = Gen $ \smgen _ k -> let (n, smgen') = SM.nextInteger a b smgen in k smgen' n

-- | The lower number will be the lower bound, higher will be the upper bound,
-- inclusive.
genWord64Bounded :: Word64 -> Word64 -> Gen space Word64
genWord64Bounded a b = do
  w64 <- genWord64
  pure (lo + (w64 `mod` modulus))
  where
    lo = min a b
    hi = max a b
    modulus = (hi - lo) + 1

{-# INLINE ndmap #-}
-- | This is what contramap would be if we have a Contravariant instance.
-- 
-- In order for a generator to shrink in a predictable way, the function must be
-- non-decreasing under the complexity order of the spaces. It's unreasonable
-- to expect GHC to prove that, so instead we use a newtype and hopefully
-- it's enough to check every constructor.
ndmap :: NonDecreasing space subspace -> Gen subspace t -> Gen space t
ndmap f gen = Gen $ \smgen space k -> unGen gen smgen (nonDecreasing f space) k

-- There are 3 sensible ways to generate a list
-- 1. Use a fixed known length as a function parameter
-- 2. Use a length from the space parameter
-- 3. Use a random length
-- But number 3 can be used as a basis for them all, without sacrificing the
-- possibility of good shrinking.

-- | The obvious way to define a list generator.
--
-- How it shrinks is determined by how its arguments shrinks.
--
-- For example it could be used to define a list where the length will shrink
-- independently of the elements
--
-- @
--   type Space
--   type SubSpace
--   _1 :: NonDecreasing Space Natural
--   _2 :: NonDecreasing Space SubSpace
--
--   list :: Gen SubSpace t -> Gen Space [t]
--   list subGen = listOf (fromParameter _1) (ndmap _2 subGen)
-- @
--
-- It could also define a list which shrinks in length and in its components
--
-- @
--   list :: Gen Natural t -> Gen Natural [t]
--   list subGen = listOf parameter subGen
-- @
listOf :: Gen space Natural -> Gen space t -> Gen space [t]
listOf genN genT = do
  n <- genN
  replicateM (fromIntegral n) genT

listOfLength :: NonDecreasing space Natural -> Gen space t -> Gen space [t]
listOfLength len = listOf (fromParameter len)

listOfKnownLength :: Natural -> Gen space t -> Gen space [t]
listOfKnownLength len = listOf (fromParameter (constant len))

showSeedHex :: SMGen -> String
showSeedHex smgen = let (w1, w2) = SM.unseedSMGen smgen in (showHex w1 <> showHex w2) ""

readSeedHex :: String -> Maybe SMGen
readSeedHex str = do
  let (str1, str2) = splitAt 16 str
  w1 <- fromHex str1
  w2 <- fromHex str2
  Just (SM.seedSMGen' (w1, w2))
  where
    fromHex :: String -> Maybe Word64
    fromHex str = case readHex str of
      [(w64, "")] -> Just w64
      _ -> Nothing
