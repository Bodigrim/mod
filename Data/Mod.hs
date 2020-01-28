-- |
-- Module:      Data.Mod
-- Copyright:   (c) 2017-2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Modular_arithmetic Modular arithmetic>,
-- promoting moduli to the type level, with an emphasis on performance.
-- Originally part of <https://hackage.haskell.org/package/arithmoi arithmoi> package.
--
-- This module supports moduli of arbitrary size.
-- Use "Data.Mod.Word" to achieve better performance,
-- when your moduli fit into 'Word'.

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples    #-}

module Data.Mod
  ( Mod
  , unMod
  , invertMod
  , (^%)
  ) where

import Control.Exception
import Control.DeepSeq
#ifdef MIN_VERSION_semirings
import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Ratio
import Data.Semiring (Semiring(..), Ring(..))
#endif
import GHC.Exts
import GHC.Generics
import GHC.Integer.GMP.Internals
import GHC.Natural (Natural(..), powModNatural)
import GHC.TypeNats (Nat, KnownNat, natVal)

-- | This data type represents
-- <https://en.wikipedia.org/wiki/Modular_arithmetic#Integers_modulo_n integers modulo m>,
-- equipped with useful instances.
--
-- For example, 3 :: 'Mod' 10 stands for the class of integers
-- congruent to 3 modulo 10: …−17, −7, 3, 13, 23…
--
-- >>> :set -XDataKinds
-- >>> 3 + 8 :: Mod 10
-- (1 `modulo` 10) -- because 3 + 8 = 11 ≡ 1 (mod 10)
--
-- __Warning:__ division by residue, which is not
-- <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- with the modulo, throws 'DivideByZero'.
-- Consider using 'invertMod' for non-prime moduli.
newtype Mod (m :: Nat) = Mod
  { unMod :: Natural
  -- ^ The canonical representative of the residue class,
  -- always between 0 and m - 1 inclusively.
  }
  deriving (Eq, Ord, Generic)

instance NFData (Mod m)

instance KnownNat m => Show (Mod m) where
  show m = "(" ++ show (unMod m) ++ " `modulo` " ++ show (natVal m) ++ ")"

instance KnownNat m => Enum (Mod m) where
  succ x = if x == maxBound then throw Overflow  else coerce (succ @Natural) x
  pred x = if x == minBound then throw Underflow else coerce (pred @Natural) x

  toEnum   = fromIntegral
  fromEnum = fromIntegral . unMod

  enumFrom x       = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)

  enumFromTo     = coerce (enumFromTo     @Natural)
  enumFromThenTo = coerce (enumFromThenTo @Natural)

instance KnownNat m => Bounded (Mod m) where
  minBound = Mod 0
  maxBound = let mx = Mod (natVal mx - 1) in mx

bigNatToNat :: BigNat -> Natural
bigNatToNat r# =
  if isTrue# (sizeofBigNat# r# ==# 1#) then NatS# (bigNatToWord r#) else NatJ# r#

subIfGe :: BigNat -> BigNat -> Natural
subIfGe z# m# = case z# `compareBigNat` m# of
  LT -> NatJ# z#
  EQ -> NatS# 0##
  GT -> bigNatToNat $ z# `minusBigNat` m#

#if !MIN_VERSION_base(4,12,0)
addWordC# :: Word# -> Word# -> (# Word#, Int# #)
addWordC# x# y# = (# z#, word2Int# c# #)
  where
    !(# c#, z# #) = x# `plusWord2#` y#
#endif

addMod :: Natural -> Natural -> Natural -> Natural
addMod (NatS# m#) (NatS# x#) (NatS# y#) =
  if isTrue# c# || isTrue# (z# `geWord#` m#) then NatS# (z# `minusWord#` m#) else NatS# z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod NatS#{} _ _ = brokenInvariant
addMod (NatJ# m#) (NatS# x#) (NatS# y#) =
  if isTrue# c# then subIfGe (wordToBigNat2 1## z#) m# else NatS# z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod (NatJ# m#) (NatS# x#) (NatJ# y#) = subIfGe (y# `plusBigNatWord` x#) m#
addMod (NatJ# m#) (NatJ# x#) (NatS# y#) = subIfGe (x# `plusBigNatWord` y#) m#
addMod (NatJ# m#) (NatJ# x#) (NatJ# y#) = subIfGe (x# `plusBigNat`     y#) m#

subMod :: Natural -> Natural -> Natural -> Natural
subMod (NatS# m#) (NatS# x#) (NatS# y#) =
  if isTrue# (x# `geWord#` y#) then NatS# z# else NatS# (z# `plusWord#` m#)
  where
    z# = x# `minusWord#` y#
subMod NatS#{} _ _ = brokenInvariant
subMod (NatJ# m#) (NatS# x#) (NatS# y#) =
  if isTrue# (x# `geWord#` y#)
    then NatS# (x# `minusWord#` y#)
    else bigNatToNat $ m# `minusBigNatWord` (y# `minusWord#` x#)
subMod (NatJ# m#) (NatS# x#) (NatJ# y#) =
  bigNatToNat $ (m# `minusBigNat` y#) `plusBigNatWord` x#
subMod NatJ#{} (NatJ# x#) (NatS# y#) =
  bigNatToNat $ x# `minusBigNatWord` y#
subMod (NatJ# m#) (NatJ# x#) (NatJ# y#) = case x# `compareBigNat` y# of
  LT -> bigNatToNat $ (m# `minusBigNat` y#) `plusBigNat` x#
  EQ -> NatS# 0##
  GT -> bigNatToNat $ x# `minusBigNat` y#

negateMod :: Natural -> Natural -> Natural
negateMod _ (NatS# 0##) = NatS# 0##
negateMod (NatS# m#) (NatS# x#) = NatS# (m# `minusWord#` x#)
negateMod NatS#{} _ = brokenInvariant
negateMod (NatJ# m#) (NatS# x#) = bigNatToNat $ m# `minusBigNatWord` x#
negateMod (NatJ# m#) (NatJ# x#) = bigNatToNat $ m# `minusBigNat`     x#

mulMod :: Natural -> Natural -> Natural -> Natural
mulMod (NatS# m#) (NatS# x#) (NatS# y#) = NatS# r#
  where
    !(# z1#, z2# #) = timesWord2# x# y#
    !(# _, r# #) = quotRemWord2# z1# z2# m#
mulMod NatS#{} _ _ = brokenInvariant
mulMod (NatJ# m#) (NatS# x#) (NatS# y#) =
  bigNatToNat $ wordToBigNat2 z1# z2# `remBigNat` m#
  where
    !(# z1#, z2# #) = timesWord2# x# y#
mulMod (NatJ# m#) (NatS# x#) (NatJ# y#) =
  bigNatToNat $ (y# `timesBigNatWord` x#) `remBigNat` m#
mulMod (NatJ# m#) (NatJ# x#) (NatS# y#) =
  bigNatToNat $ (x# `timesBigNatWord` y#) `remBigNat` m#
mulMod (NatJ# m#) (NatJ# x#) (NatJ# y#) =
  bigNatToNat $ (x# `timesBigNat` y#) `remBigNat` m#

brokenInvariant :: a
brokenInvariant = error "argument is larger than modulo"

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
      mx = Mod $ fromInteger $ x `mod` toInteger (natVal mx)
  {-# INLINE fromInteger #-}

#ifdef MIN_VERSION_semirings

instance KnownNat m => Semiring (Mod m) where
  plus  = (+)
  {-# INLINE plus #-}
  times = (*)
  {-# INLINE times #-}
  zero  = Mod 0
  {-# INLINE zero #-}
  one   = mx
    where
      mx = if natVal mx > 1 then Mod 1 else Mod 0
  {-# INLINE one #-}
  fromNatural x = mx
    where
      mx = Mod $ x `mod` natVal mx
  {-# INLINE fromNatural #-}

instance KnownNat m => Ring (Mod m) where
  negate = Prelude.negate
  {-# INLINE negate #-}

-- | See the warning about division above.
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

-- | See the warning about division above.
instance KnownNat m => GcdDomain (Mod m) where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

-- | See the warning about division above.
instance KnownNat m => Euclidean (Mod m) where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

-- | See the warning about division above.
instance KnownNat m => Field (Mod m)

#endif

-- | If an argument is
-- <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- with the modulo, return its modular inverse.
-- Otherwise return 'Nothing'.
--
-- >>> :set -XDataKinds
-- >>> invertMod 3 :: Mod 10
-- Just (7 `modulo` 10) -- because 3 * 7 = 21 ≡ 1 (mod 10)
-- >>> invertMod 4 :: Mod 10
-- Nothing -- because 4 and 10 are not coprime
invertMod :: KnownNat m => Mod m -> Maybe (Mod m)
invertMod mx
  = if y <= 0
    then Nothing
    else Just $ Mod $ fromInteger y
  where
    y = recipModInteger (toInteger (unMod mx)) (toInteger (natVal mx))
{-# INLINABLE invertMod #-}

-- | Drop-in replacement for 'Prelude.^' with much better performance.
-- Negative powers are allowed, but may throw 'DivideByZero', if an argument
-- is not <https://en.wikipedia.org/wiki/Coprime_integers coprime> with the modulo.
--
-- Building with @-O@ triggers a rewrite rule 'Prelude.^' = '^%'.
--
-- >>> :set -XDataKinds
-- >>> 3 ^% 4 :: Mod 10
-- (1 `modulo` 10) -- because 3 ^ 4 = 81 ≡ 1 (mod 10)
-- >>> 3 ^% (-1) :: Mod 10
-- (7 `modulo` 10) -- because 3 * 7 = 21 ≡ 1 (mod 10)
-- >>> 4 ^% (-1) :: Mod 10
-- (*** Exception: divide by zero -- because 4 and 10 are not coprime
(^%) :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
mx ^% a
  | a < 0     = case invertMod mx of
    Nothing ->  throw DivideByZero
    Just my ->  Mod $ powModNatural (unMod my) (fromIntegral (-a)) (natVal mx)
  | otherwise = Mod $ powModNatural (unMod mx) (fromIntegral a)    (natVal mx)
{-# INLINABLE [1] (^%) #-}

{-# SPECIALISE [1] (^%) ::
  KnownNat m => Mod m -> Integer -> Mod m,
  KnownNat m => Mod m -> Natural -> Mod m,
  KnownNat m => Mod m -> Int     -> Mod m,
  KnownNat m => Mod m -> Word    -> Mod m #-}

{-# RULES
"powMod"               forall (x :: KnownNat m => Mod m) p. x ^ p = x ^% p

"powMod/2/Integer"     forall x. x ^% (2 :: Integer) = let u = x in u*u
"powMod/3/Integer"     forall x. x ^% (3 :: Integer) = let u = x in u*u*u
"powMod/2/Int"         forall x. x ^% (2 :: Int)     = let u = x in u*u
"powMod/3/Int"         forall x. x ^% (3 :: Int)     = let u = x in u*u*u
"powMod/2/Word"        forall x. x ^% (2 :: Word)    = let u = x in u*u
"powMod/3/Word"        forall x. x ^% (3 :: Word)    = let u = x in u*u*u
#-}

infixr 8 ^%
