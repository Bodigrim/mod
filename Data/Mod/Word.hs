-- |
-- Module:      Data.Mod.Word
-- Copyright:   (c) 2017-2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Modular_arithmetic Modular arithmetic>,
-- promoting moduli to the type level, with an emphasis on performance.
-- Originally part of <https://hackage.haskell.org/package/arithmoi arithmoi> package.

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples    #-}

module Data.Mod.Word
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
import GHC.Natural (Natural(..))

#if MIN_VERSION_base(4,11,0)
import GHC.TypeNats hiding (Mod)
#elif MIN_VERSION_base(4,10,0)
import GHC.TypeNats
#else

import GHC.TypeLits hiding (natVal, someNatVal)
import qualified GHC.TypeLits as TL

natVal :: KnownNat n => proxy n -> Natural
natVal = fromInteger . TL.natVal

someNatVal :: Natural -> SomeNat
someNatVal n = case TL.someNatVal (toInteger n) of
  Nothing -> error "someNatVal: impossible negative argument"
  Just sn -> sn

#endif

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
  { unMod :: Word
  -- ^ The canonical representative of the residue class,
  -- always between 0 and m - 1 inclusively.
  }
  deriving (Eq, Ord, Generic)

instance NFData (Mod m)

instance KnownNat m => Show (Mod m) where
  show m = "(" ++ show (unMod m) ++ " `modulo` " ++ show (natVal m) ++ ")"

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
  minBound = Mod 0
  maxBound = let mx = Mod (fromIntegral (natVal mx) - 1) in mx

#if !MIN_VERSION_base(4,12,0)
addWordC# :: Word# -> Word# -> (# Word#, Int# #)
addWordC# x# y# = (# z#, word2Int# c# #)
  where
    !(# c#, z# #) = x# `plusWord2#` y#
#endif

addMod :: Natural -> Word -> Word -> Word
addMod (NatS# m#) (W# x#) (W# y#) =
  if isTrue# c# || isTrue# (z# `geWord#` m#) then W# (z# `minusWord#` m#) else W# z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod NatJ#{} _ _ = tooLargeModulo

subMod :: Natural -> Word -> Word -> Word
subMod (NatS# m#) (W# x#) (W# y#) =
  if isTrue# (x# `geWord#` y#) then W# z# else W# (z# `plusWord#` m#)
  where
    z# = x# `minusWord#` y#
subMod NatJ#{} _ _ = tooLargeModulo

negateMod :: Natural -> Word -> Word
negateMod _ (W# 0##) = W# 0##
negateMod (NatS# m#) (W# x#) = W# (m# `minusWord#` x#)
negateMod NatJ#{} _ = tooLargeModulo

mulMod :: Natural -> Word -> Word -> Word
mulMod (NatS# m#) (W# x#) (W# y#) = W# r#
  where
    !(# z1#, z2# #) = timesWord2# x# y#
    !(# _, r# #) = quotRemWord2# z1# z2# m#
mulMod NatJ#{} _ _ = tooLargeModulo

tooLargeModulo :: a
tooLargeModulo = error "modulo does not fit into a machine word"

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
      m = natVal mx
      mx = case m of
        NatS#{} -> Mod $ fromInteger $ x `mod` toInteger m
        NatJ#{} -> tooLargeModulo
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
      m = natVal mx
      mx = case m of
        NatS#{} -> Mod $ fromIntegral $ x `mod` m
        NatJ#{} -> tooLargeModulo
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
-- TODO reimplement natively?
invertMod mx
  = if y <= 0
    then Nothing
    else Just $ Mod $ fromInteger y
  where
    y = recipModInteger (toInteger (unMod mx)) (toInteger (natVal mx))
{-# INLINABLE invertMod #-}

-- | Drop-in replacement for 'Prelude.^' with a bit better performance.
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
mx@(Mod (W# x#)) ^% a = case natVal mx of
  NatJ#{} -> tooLargeModulo
  NatS# m#
    | a < 0 -> case invertMod mx of
      Nothing            -> throw DivideByZero
      Just (Mod (W# y#)) -> Mod $ W# (f y# (- a) 1##)
    | otherwise          -> Mod $ W# (f x# a 1##)
    where
      f :: Integral a => Word# -> a -> Word# -> Word#
      f _  0 acc# = acc#
      f b# e acc# = f bb# (e `Prelude.quot` 2) (if odd e then ba# else acc#)
        where
          !(# bb1#, bb2# #) = timesWord2# b# b#
          !(#    _, bb#  #) = quotRemWord2# bb1# bb2# m#
          !(# ba1#, ba2# #) = timesWord2# b# acc#
          !(#    _, ba#  #) = quotRemWord2# ba1# ba2# m#
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
