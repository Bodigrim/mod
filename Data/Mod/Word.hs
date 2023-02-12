-- |
-- Module:      Data.Mod.Word
-- Copyright:   (c) 2017-2022 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Modular_arithmetic Modular arithmetic>,
-- promoting moduli to the type level, with an emphasis on performance.
-- Originally part of the <https://hackage.haskell.org/package/arithmoi arithmoi> package.
--
-- This module supports only moduli, which fit into 'Word'.
-- Use the (slower) "Data.Mod" module for handling arbitrary-sized moduli.

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}

module Data.Mod.Word
  ( Mod
  , unMod
  , invertMod
  , (^%)
  ) where

import Prelude as P hiding (even)
import Control.Exception
import Control.DeepSeq
import Data.Bits
import Data.Mod.Compat (timesWord2#, remWord2#)
import Data.Ratio
#ifdef MIN_VERSION_semirings
import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Semiring (Semiring(..), Ring(..))
#endif
#ifdef MIN_VERSION_vector
import Data.Primitive (Prim)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U
#endif
import Foreign.Storable (Storable)
import GHC.Exts hiding (timesWord2#, quotRemWord2#)
import GHC.Generics
import GHC.Natural (Natural(..))
import GHC.Num.BigNat
import GHC.Num.Integer
import GHC.TypeNats (Nat, KnownNat, natVal)
import Text.Read (Read(readPrec))

-- | This data type represents
-- <https://en.wikipedia.org/wiki/Modular_arithmetic#Integers_modulo_n integers modulo m>,
-- equipped with useful instances.
--
-- For example, 3 :: 'Mod' 10 stands for the class of integers
-- congruent to \( 3 \bmod 10 \colon \ldots {−17}, −7, 3, 13, 23 \ldots \)
--
-- >>> :set -XDataKinds
-- >>> 3 + 8 :: Mod 10 -- 3 + 8 = 11 ≡ 1 (mod 10)
-- 1
--
-- __Note:__ 'Mod' 0 has no inhabitants, eventhough \( \mathbb{Z}/0\mathbb{Z} \) is technically isomorphic to \( \mathbb{Z} \).
newtype Mod (m :: Nat) = Mod
  { unMod :: Word
  -- ^ The canonical representative of the residue class,
  -- always between 0 and \( m - 1 \) (inclusively).
  --
  -- >>> :set -XDataKinds
  -- >>> -1 :: Mod 10
  -- 9
  }
  deriving (Eq, Ord, Generic)
  deriving Storable
  -- ^ No validation checks are performed;
  -- reading untrusted data may corrupt internal invariants.
#ifdef MIN_VERSION_vector
  deriving Prim
  -- ^ No validation checks are performed;
  -- reading untrusted data may corrupt internal invariants.
#endif

instance NFData (Mod m)

instance Show (Mod m) where
  show (Mod x) = show x

-- | Wrapping behaviour, similar to
-- the existing @instance@ 'Read' 'Int'.
instance KnownNat m => Read (Mod m) where
  readPrec = fromInteger <$> readPrec

instance KnownNat m => Real (Mod m) where
  toRational (Mod x) = toRational x

instance KnownNat m => Enum (Mod m) where
  succ x = if x == maxBound then throw Overflow  else coerce (succ @Word) x
  pred x = if x == minBound then throw Underflow else coerce (pred @Word) x

  toEnum   = fromIntegral
  fromEnum = fromIntegral . unMod

  enumFrom x       = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)

  enumFromTo     = coerce (enumFromTo     @Word)
  enumFromThenTo = coerce (enumFromThenTo @Word)

instance KnownNat m => Bounded (Mod m) where
  minBound = mx
    where
      mx = if natVal mx > 0 then Mod 0 else throw DivideByZero
  maxBound = mx
    where
      mx = if m > 0 then Mod (fromIntegral (m - 1)) else throw DivideByZero
      m = natVal mx

addMod :: Natural -> Word -> Word -> Word
addMod (NatS# m#) (W# x#) (W# y#) =
  if isTrue# c# || isTrue# (z# `geWord#` m#) then W# (z# `minusWord#` m#) else W# z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod NatJ#{} _ _ = tooLargeModulus

subMod :: Natural -> Word -> Word -> Word
subMod (NatS# m#) (W# x#) (W# y#) =
  if isTrue# (x# `geWord#` y#) then W# z# else W# (z# `plusWord#` m#)
  where
    z# = x# `minusWord#` y#
subMod NatJ#{} _ _ = tooLargeModulus

negateMod :: Natural -> Word -> Word
negateMod _ (W# 0##) = W# 0##
negateMod (NatS# m#) (W# x#) = W# (m# `minusWord#` x#)
negateMod NatJ#{} _ = tooLargeModulus

halfWord :: Word
halfWord = 1 `shiftL` (finiteBitSize (0 :: Word) `shiftR` 1)

mulMod :: Natural -> Word -> Word -> Word
mulMod (NatS# m#) (W# x#) (W# y#)
  | W# m# <= halfWord = W# (timesWord# x# y# `remWord#` m#)
  | otherwise = W# r#
  where
    !(# hi#, lo# #) = timesWord2# x# y#
    !r# = remWord2# lo# hi# m#
mulMod NatJ#{} _ _ = tooLargeModulus

fromIntegerMod :: Natural -> Integer -> Word
fromIntegerMod (NatS# 0##) !_ = throw DivideByZero
fromIntegerMod (NatS# m#) (IS x#) =
  if isTrue# (x# >=# 0#)
    then W# (int2Word# x# `remWord#` m#)
    else negateMod (NatS# m#) (W# (int2Word# (negateInt# x#) `remWord#` m#))
fromIntegerMod (NatS# m#) (IP x#) =
  W# (x# `bigNatRemWord#` m#)
fromIntegerMod (NatS# m#) (IN x#) =
  negateMod (NatS# m#) (W# (x# `bigNatRemWord#` m#))
fromIntegerMod NatJ#{} _ = tooLargeModulus

#ifdef MIN_VERSION_semirings

fromNaturalMod :: Natural -> Natural -> Word
fromNaturalMod (NatS# 0##) !_ = throw DivideByZero
fromNaturalMod (NatS# m#) (NatS# x#) = W# (x# `remWord#` m#)
fromNaturalMod (NatS# m#) (NatJ# (BN# x#)) = W# (x# `bigNatRemWord#` m#)
fromNaturalMod NatJ#{} _ = tooLargeModulus

getModulus :: Natural -> Word
getModulus (NatS# m#) = W# m#
getModulus NatJ#{} = tooLargeModulus

#endif

tooLargeModulus :: a
tooLargeModulus = error "modulus does not fit into a machine word"

instance KnownNat m => Num (Mod m) where
  mx@(Mod !x) + (Mod !y) = Mod $ addMod (natVal mx) x y
  {-# INLINE (+) #-}
  mx@(Mod !x) - (Mod !y) = Mod $ subMod (natVal mx) x y
  {-# INLINE (-) #-}
  negate mx@(Mod !x) = Mod $ negateMod (natVal mx) x
  {-# INLINE negate #-}
  mx@(Mod !x) * (Mod !y) = Mod $ mulMod (natVal mx) x y
  {-# INLINE (*) #-}
  abs = id
  {-# INLINE abs #-}
  signum = const x
    where
      x = if natVal x > 1 then Mod 1 else Mod 0
  {-# INLINE signum #-}
  fromInteger x = mx
    where
      mx = Mod $ fromIntegerMod (natVal mx) x
  {-# INLINE fromInteger #-}

#ifdef MIN_VERSION_semirings

instance KnownNat m => Semiring (Mod m) where
  plus  = (+)
  {-# INLINE plus #-}
  times = (*)
  {-# INLINE times #-}
  zero  = mx
    where
      mx = if natVal mx > 0 then Mod 0 else throw DivideByZero
  {-# INLINE zero #-}
  one   = mx
    where
      mx = case m `compare` 1 of
        LT -> throw DivideByZero
        EQ -> Mod 0
        GT -> Mod 1
      m = natVal mx
  {-# INLINE one #-}
  fromNatural x = mx
    where
      mx = Mod $ fromNaturalMod (natVal mx) x
  {-# INLINE fromNatural #-}

instance KnownNat m => Ring (Mod m) where
  negate = P.negate
  {-# INLINE negate #-}

-- | 'Mod' @m@ is not even an
-- <https://en.wikipedia.org/wiki/Integral_domain integral domain> for
-- <https://en.wikipedia.org/wiki/Composite_number composite> @m@,
-- much less a <https://en.wikipedia.org/wiki/GCD_domain GCD domain>.
-- However, 'Data.Euclidean.gcd' and 'Data.Euclidean.lcm' are still meaningful
-- even for composite @m@, corresponding to a sum and an intersection of
-- <https://en.wikipedia.org/wiki/Ideal_(ring_theory) ideals>.
--
-- The instance is lawful only for
-- <https://en.wikipedia.org/wiki/Prime_number prime> @m@, otherwise
-- @'Data.Euclidean.divide' x y@ tries to return any @Just z@ such that @x == y * z@.
--
instance KnownNat m => GcdDomain (Mod m) where
  divide (Mod 0) !_ = Just (Mod 0)
  divide _ (Mod 0) = Nothing
  divide mx@(Mod x) (Mod y) = case mry of
    Just ry -> if xr == 0 then Just (Mod xq * Mod ry) else Nothing
    Nothing -> Nothing
    where
      m = getModulus (natVal mx)
      gmy = P.gcd m y
      (xq, xr) = P.quotRem x gmy
      mry = invertModWord (y `P.quot` gmy)  (m `P.quot` gmy)

  gcd (Mod !x) (Mod !y) = g
    where
      m = getModulus (natVal g)
      g = Mod $ if m > 1 then P.gcd (P.gcd m x) y else 0
  lcm (Mod !x) (Mod !y) = l
    where
      m = getModulus (natVal l)
      l = Mod $ if m > 1 then P.lcm (P.gcd m x) (P.gcd m y) else 0
  coprime x y = Data.Euclidean.gcd x y == one

-- | 'Mod' @m@ is not even an
-- <https://en.wikipedia.org/wiki/Integral_domain integral domain> for
-- <https://en.wikipedia.org/wiki/Composite_number composite> @m@,
-- much less a <https://en.wikipedia.org/wiki/Euclidean_domain Euclidean domain>.
--
-- The instance is lawful only for
-- <https://en.wikipedia.org/wiki/Prime_number prime> @m@, otherwise
-- we try to do our best:
-- @'Data.Euclidean.quot' x y@ returns any @z@ such that @x == y * z@,
-- 'Data.Euclidean.rem' is not always 0, and both can throw 'DivideByZero'.
--
instance KnownNat m => Euclidean (Mod m) where
  degree = fromIntegral . unMod

  quotRem (Mod 0) !_ = (Mod 0, Mod 0)
  quotRem _ (Mod 0) = throw DivideByZero
  quotRem mx@(Mod x) (Mod y) = case mry of
    Just ry -> (Mod xq * Mod ry, Mod xr)
    Nothing -> throw DivideByZero
    where
      m = getModulus (natVal mx)
      gmy = P.gcd m y
      (xq, xr) = P.quotRem x gmy
      mry = invertModWord (y `P.quot` gmy)  (m `P.quot` gmy)

-- | 'Mod' @m@ is not even an
-- <https://en.wikipedia.org/wiki/Integral_domain integral domain> for
-- <https://en.wikipedia.org/wiki/Composite_number composite> @m@,
-- much less a <https://en.wikipedia.org/wiki/Field_(mathematics) field>.
--
-- The instance is lawful only for
-- <https://en.wikipedia.org/wiki/Prime_number prime> @m@, otherwise
-- division by a residue, which is not
-- <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- with the modulus, throws 'DivideByZero'.
-- Consider using 'invertMod' for non-prime moduli.
--
instance KnownNat m => Field (Mod m)

#endif

-- | Division by a residue, which is not
-- <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- with the modulus, throws 'DivideByZero'.
-- Consider using 'invertMod' for non-prime moduli.
instance KnownNat m => Fractional (Mod m) where
  fromRational r = case denominator r of
    1   -> num
    den -> num / fromInteger den
    where
      num = fromInteger (numerator r)
  {-# INLINE fromRational #-}
  recip mx = case invertMod mx of
    Nothing -> throw DivideByZero
    Just y  -> y
  {-# INLINE recip #-}

-- | If an argument is
-- <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- with the modulus, return its modular inverse.
-- Otherwise return 'Nothing'.
--
-- >>> :set -XDataKinds
-- >>> invertMod 3 :: Mod 10 -- 3 * 7 = 21 ≡ 1 (mod 10)
-- Just 7
-- >>> invertMod 4 :: Mod 10 -- 4 and 10 are not coprime
-- Nothing
invertMod :: KnownNat m => Mod m -> Maybe (Mod m)
invertMod mx@(Mod !x) = case natVal mx of
  NatJ#{}   -> tooLargeModulus
  NatS# 0## -> Nothing
  NatS# m#  -> Mod <$> invertModWord x (W# m#)

invertModWord :: Word -> Word -> Maybe Word
invertModWord x m@(W# m#)
  -- If both x and m are even, no inverse exists
  | even x, isTrue# (k# `gtWord#` 0##) = Nothing
  | otherwise = case invertModWordOdd x m' of
    Nothing -> Nothing
    -- goDouble cares only about mod 2^k,
    -- so overflows and underflows in (1 - x * y) are fine
    Just y -> Just $ goDouble y (1 - x * y)
  where
    k# = ctz# m#
    m' = m `unsafeShiftR` I# (word2Int# k#)

    xm' = x * m'

    goDouble :: Word -> Word -> Word
    goDouble acc r@(W# r#)
      | isTrue# (tz# `geWord#` k#)
      = acc
      | otherwise
      = goDouble (acc + m' `unsafeShiftL` tz) (r - xm' `unsafeShiftL` tz)
      where
        tz# = ctz# r#
        tz = I# (word2Int# tz#)

-- | Extended binary gcd.
-- The second argument must be odd.
invertModWordOdd :: Word -> Word -> Maybe Word
invertModWordOdd 0 !_ = Nothing
invertModWordOdd !x !m = go00 0 m 1 x
  where
    halfMp1 :: Word
    halfMp1 = half m + 1

    -- Both s and s' may be even
    go00 :: Word -> Word -> Word -> Word -> Maybe Word
    go00 !r !s !r' !s'
      | even s = let (# hr, hs #) = doHalf r s in go00 hr hs r' s'
      | otherwise = go10 r s r' s'

    -- Here s is odd, s' may be even
    go10 :: Word -> Word -> Word -> Word -> Maybe Word
    go10 !r !s !r' !s'
      | even s' = let (# hr', hs' #) = doHalf r' s' in go10 r s hr' hs'
      | otherwise = go11 r s r' s'

    -- Here s may be even, s' is odd
    go01 :: Word -> Word -> Word -> Word -> Maybe Word
    go01 !r !s !r' !s'
      | even s = let (# hr, hs #) = doHalf r s in go01 hr hs r' s'
      | otherwise = go11 r s r' s'

    -- Both s and s' are odd
    go11 :: Word -> Word -> Word -> Word -> Maybe Word
    go11 !r !s !r' !s' = case s `compare` s' of
      EQ -> if s == 1 then Just r else Nothing
      LT -> let newR' = r' - r + (r `ge` r') * m in
            let newS' = s' - s in
            let (# hr', hs' #) = doHalf newR' newS' in
            go10 r s hr' hs'
      GT -> let newR = r - r' + (r' `ge` r) * m in
            let newS = s - s' in
            let (# hr, hs #) = doHalf newR newS in
            go01 hr hs r' s'

    doHalf :: Word -> Word -> (# Word, Word #)
    doHalf r s = (# half r + (r .&. 1) * halfMp1, half s #)
    {-# INLINE doHalf #-}

-- | ge x y returns 1 is x >= y and 0 otherwise.
ge :: Word -> Word -> Word
ge (W# x) (W# y) = W# (int2Word# (x `geWord#` y))

even :: Word -> Bool
even x = (x .&. 1) == 0
{-# INLINE even #-}

half :: Word -> Word
half x = x `shiftR` 1
{-# INLINE half #-}

-- | Drop-in replacement for 'Prelude.^' with a bit better performance.
-- Negative powers are allowed, but may throw 'DivideByZero', if an argument
-- is not <https://en.wikipedia.org/wiki/Coprime_integers coprime> with the modulus.
--
-- >>> :set -XDataKinds
-- >>> 3 ^% 4 :: Mod 10    -- 3 ^ 4 = 81 ≡ 1 (mod 10)
-- 1
-- >>> 3 ^% (-1) :: Mod 10 -- 3 * 7 = 21 ≡ 1 (mod 10)
-- 7
-- >>> 4 ^% (-1) :: Mod 10 -- 4 and 10 are not coprime
-- (*** Exception: divide by zero
(^%) :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
mx@(Mod !x) ^% a = case natVal mx of
  NatJ#{} -> tooLargeModulus
  m@(NatS# _)
    | a < 0 -> case invertMod mx of
      Nothing      -> throw DivideByZero
      Just (Mod y) -> Mod $ f y (-a) 1
    | otherwise    -> Mod $ f x a 1
    where
      f !_ 0 acc = acc
      f b  e acc = f (mulMod m b b) (e `P.quot` 2) (if odd e then mulMod m b acc else acc)
{-# INLINABLE [1] (^%) #-}

{-# SPECIALISE [1] (^%) ::
  KnownNat m => Mod m -> Integer -> Mod m,
  KnownNat m => Mod m -> Natural -> Mod m,
  KnownNat m => Mod m -> Int     -> Mod m,
  KnownNat m => Mod m -> Word    -> Mod m #-}

{-# RULES
"powMod/2/Integer"     forall x. x ^% (2 :: Integer) = let u = x in u*u
"powMod/3/Integer"     forall x. x ^% (3 :: Integer) = let u = x in u*u*u
"powMod/2/Int"         forall x. x ^% (2 :: Int)     = let u = x in u*u
"powMod/3/Int"         forall x. x ^% (3 :: Int)     = let u = x in u*u*u
"powMod/2/Word"        forall x. x ^% (2 :: Word)    = let u = x in u*u
"powMod/3/Word"        forall x. x ^% (3 :: Word)    = let u = x in u*u*u #-}

infixr 8 ^%

#ifdef MIN_VERSION_vector

newtype instance U.MVector s (Mod m) = MV_Mod (P.MVector s Word)
newtype instance U.Vector    (Mod m) = V_Mod  (P.Vector    Word)

-- | No validation checks are performed;
-- reading untrusted data may corrupt internal invariants.
instance U.Unbox (Mod m)

-- | No validation checks are performed;
-- reading untrusted data may corrupt internal invariants.
instance M.MVector U.MVector (Mod m) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Mod v) = M.basicLength v
  basicUnsafeSlice i n (MV_Mod v) = MV_Mod $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Mod v1) (MV_Mod v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Mod <$> M.basicUnsafeNew n
  basicInitialize (MV_Mod v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Mod <$> M.basicUnsafeReplicate n (unMod x)
  basicUnsafeRead (MV_Mod v) i = Mod <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Mod v) i x = M.basicUnsafeWrite v i (unMod x)
  basicClear (MV_Mod v) = M.basicClear v
  basicSet (MV_Mod v) x = M.basicSet v (unMod x)
  basicUnsafeCopy (MV_Mod v1) (MV_Mod v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Mod v1) (MV_Mod v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Mod v) n = MV_Mod <$> M.basicUnsafeGrow v n

-- | No validation checks are performed;
-- reading untrusted data may corrupt internal invariants.
instance G.Vector U.Vector (Mod m) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Mod v) = V_Mod <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Mod v) = MV_Mod <$> G.basicUnsafeThaw v
  basicLength (V_Mod v) = G.basicLength v
  basicUnsafeSlice i n (V_Mod v) = V_Mod $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Mod v) i = Mod <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Mod mv) (V_Mod v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

#endif
