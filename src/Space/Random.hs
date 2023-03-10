module Space.Random
  (
  -- * The Gen monad
    Gen (..)
  , Arbitrary
  , sampleAt
  , sampleAt_
  , ndmap
  , parameter

  -- * Various generators
  , genSeed
  , genWord8
  , genWord16
  , genWord32
  , genWord64
  , genWord64Bounded
  , genInteger
  , genNatural
  , listOf
  , listOfLength
  , listOfKnownLength

  -- * For searching through the random part of a space
  , RandomPoints (..)
  , randomPoints

  -- * Things to do with splitmix generators
  , Seed (..)
  , newSeedIO
  , split
  , splitK
  , splitTuple
  , splitN
  , splitUnfoldN
  , SM.mkSMGen
  , SM.newSMGen
  , SM.seedSMGen
  , SM.seedSMGen'
  , SM.unseedSMGen

  -- * Useful for showing seeds on failing tests, and reloading them to rerun
  , prettySeedHex
  , showSeedHex
  , readSeedHex

  -- * Extra stuff
  , sampleIO
  , clampToWord32
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad (ap, replicateM)
import Numeric (readHex, showHex)
import Numeric.Natural (Natural)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Word
import Pretty
import Prettyprinter (Doc)
import System.Random.SplitMix (SMGen)
import qualified System.Random.SplitMix as SM
import Space.Ordered

-- | The splitmix generator is our random seed.
newtype Seed = Seed SMGen
  deriving (Show)

instance Eq Seed where
  Seed l == Seed r = SM.unseedSMGen l == SM.unseedSMGen r

instance Pretty Seed where
  pretty = prettySeedHex

-- | For monadic construction of pseudorandom values which also depend upon some
-- parameter space. CPS form of
--
-- > Seed -> space -> (Seed, t)
--
-- for (hopefully) better inlining.
--
-- The monad instance splits the seed, so that, for instance, in
--
-- > replicateM n someGenerator
-- > otherGenerator
--
-- otherGenerator is always run with the same seed, regardless of the value of n.
newtype Gen space t = Gen { unGen :: forall r. SMGen -> space -> (SMGen -> t -> r) -> r }

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
  left >>= k = Gen $ \smgen space l -> split (Seed smgen) $ \(Seed smgen1) (Seed smgen2) ->
    -- Essential to split here, so that the structure of the left term does
    -- not affect the seed that is given to the term after the bind.
    unGen left smgen1 space $ \_smgen3 t ->
      unGen (k t) smgen2 space l

-- | A generator which doesn't have a parameter.
type Arbitrary = Gen ()

-- | Use system entropy sources (uses System.Random.SplitMix.initSMGen)
newSeedIO :: IO Seed
newSeedIO = Seed <$> SM.initSMGen

{-# INLINE CONLIKE sampleAt #-}
sampleAt :: Seed -> space -> Gen space t -> t
sampleAt (Seed g) space gen = unGen gen g space $ \_ t -> t

{-# INLINE CONLIKE sampleAt_ #-}
sampleAt_ :: Gen space t -> Seed -> space -> t
sampleAt_ gen (Seed g) = \space -> unGen gen g space $ \_ t -> t

-- | Picks a random seed. Intended for playing around / testing this library.
sampleIO :: space -> Gen space t -> IO t
sampleIO space gen = newSeedIO >>= \smgen -> pure (sampleAt smgen space gen)

{-# INLINE split #-}
split :: Seed -> (Seed -> Seed -> t) -> t
split (Seed g) k = let (g1, g2) = SM.splitSMGen g in k (Seed g1) (Seed g2)

{-# INLINE splitK #-}
splitK :: (Seed -> Seed -> t) -> Seed -> t
splitK = flip split

{-# INLINE splitTuple #-}
splitTuple :: Seed -> (Seed, Seed)
splitTuple smgen = split smgen (,)

-- | This is @splitUnfoldN n g@ but `g` is the head of the non-empty list.
--
-- The resulting list is of length @n+1@.
--
-- Useful in conjunction with rewrite rules in 'Space.Search'. Applying this to
-- 'Space.Search.searchSequential', for instance, is recommended, because GHC
-- can inline the cons and drastically simplify tests which do not actually
-- depend upon the random seed.
{-# INLINE splitN #-}
splitN :: Word32 -> Seed -> NonEmpty Seed
splitN n g = g NE.:| splitUnfoldN n g
-- Could also use
--   splitN n g = g NE.:| take (fromIntegral n) (splitUnfold g)

-- | Like 'splitUnfold' but explicitly limits the size of the list.
{-# INLINE splitUnfoldN #-}
splitUnfoldN :: Word32 -> Seed -> [Seed]
splitUnfoldN n (Seed g) = unfoldr
  (\(m, g) -> if m == 0
              then Nothing
              else Just (let (g1, g2) = SM.splitSMGen g in (Seed g1, (m-1, g2)) )
  )
  (n, g)

-- | A list of seeds and its length, which can be useful to know.
--
-- This should always be
--
-- > count = n
-- > points = splitN n g
--
-- for some seed `g`.
data RandomPoints = RandomPoints
  { count :: Int -- Number of points.
  , points :: NonEmpty Seed
  }

-- | Givs `n + 1` seeds drived from splitting this seed (see 'splitN').
{-# INLINE CONLIKE randomPoints #-}
randomPoints :: Word32 -> Seed -> RandomPoints
randomPoints n g = RandomPoints
  { count = fromIntegral (n + 1)
  , points = splitN n g
  }

-- | 
-- @ 
--   fromParameter . constant = pure
-- @
fromParameter :: NonDecreasing space t -> Gen space t
fromParameter f = Gen $ \smgen space k -> k smgen (nonDecreasing f space)

-- | Gives the space parameter itself. See also 'fromParameter'.
--
-- It's important that the space parameter is only used in a non-decreasing way,
-- but we can't get much of a guarantee about this from the type checker.
parameter :: Gen space space
parameter = Gen $ \smgen space k -> k smgen space

-- | Splits the current seed and gives the first half.
genSeed :: Gen space Seed
genSeed = Gen $ \g _ k -> split (Seed g) $ \(Seed g') seed -> k g' seed

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

genNatural :: Natural -> Natural -> Gen space Natural
genNatural a b = fromIntegral <$> genInteger (fromIntegral a) (fromIntegral b)

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
-- How it shrinks is determined by how its arguments shrink.
--
-- For example, it could be used to define a list where the length will shrink
-- independently of the elements
--
-- @
-- type Space
-- type SubSpace
-- _1 :: NonDecreasing Space Natural
-- _2 :: NonDecreasing Space SubSpace
--
-- list :: Gen SubSpace t -> Gen Space [t]
-- list subGen = listOf (fromParameter _1) (ndmap _2 subGen)
-- @
--
-- It could also define a list which shrinks in length and in its components
--
-- @
-- list :: Gen Natural t -> Gen Natural [t]
-- list subGen = listOf parameter subGen
-- @
listOf :: Gen space Natural -> Gen space t -> Gen space [t]
listOf genN genT = do
  n <- genN
  replicateM (fromIntegral n) genT

listOfLength :: NonDecreasing space Natural -> Gen space t -> Gen space [t]
listOfLength len = listOf (fromParameter len)

listOfKnownLength :: Natural -> Gen space t -> Gen space [t]
listOfKnownLength len = listOf (fromParameter (constant len))

prettySeedHex :: Seed -> Doc ann
prettySeedHex = fromString . showSeedHex

-- TODO property test that read/show roundtrip.

showSeedHex :: Seed -> String
showSeedHex (Seed smgen) = let (w1, w2) = SM.unseedSMGen smgen in ((padHex . showHex w1) <> (padHex . showHex w2)) ""

-- | Pads the hex string on the left with 0s until 16 hex digits.
padHex :: String -> String
padHex str = if l < 16 then replicate (16 - l) '0' ++ str else str
  where l = length str

-- | Assumes it's 2 16-character hex-encoded numbers concatenated
readSeedHex :: String -> Maybe Seed
readSeedHex str = do
  let (str1, str2) = splitAt 16 str
  w1 <- fromHex str1
  w2 <- fromHex str2
  Just (Seed (SM.seedSMGen' (w1, w2)))
  where
    fromHex :: String -> Maybe Word64
    fromHex str = case readHex str of
      [(w64, "")] -> Just w64
      _ -> Nothing

-- | Useful for programs which wish to take a 'Natural' from the programmer, but
-- clamp it to a 'Word32' and fail early if it's too big. Avoids silly mistakes
-- like, e.g. writing @2 ^ 64@ as a 'Word32' and getting @0@.
clampToWord32 :: String -> Natural -> Word32
clampToWord32 tag n = m
  where
    !m = if n >= 2^(32 :: Int) then error msg' else fromIntegral n
    msg' = mconcat [ tag, ": ", show n, " is too lager (maximum 32 bit unsigned)" ]
