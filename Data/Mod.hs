-- |
-- Module:      Data.Mod
-- Copyright:   (c) 2017-2022 Andrew Lelechenko
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

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

module Data.Mod
  ( Mod
  , unMod
  , invertMod
  , (^%)
  ) where

import Control.Exception
import Control.DeepSeq
import Data.Ratio
#ifdef MIN_VERSION_semirings
import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Semiring (Semiring(..), Ring(..))
#endif
import GHC.Exts
import GHC.Generics
import GHC.Natural (Natural(..), powModNatural)
import GHC.TypeNats (Nat, KnownNat, natVal)

-- | This data type represents
-- <https://en.wikipedia.org/wiki/Modular_arithmetic#Integers_modulo_n integers modulo m>,
-- equipped with useful instances.
--
-- For example, 3 :: 'Mod' 10 stands for the class of integers
-- congruent to \( 3 \bmod 10 \colon \ldots {}−17, −7, 3, 13, 23 \ldots \)
--
-- >>> :set -XDataKinds
-- >>> 3 + 8 :: Mod 10 -- 3 + 8 = 11 ≡ 1 (mod 10)
-- (1 `modulo` 10)
--
-- __Warning:__ division by residue, which is not
-- <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- with the modulo, throws 'DivideByZero'.
-- Consider using 'invertMod' for non-prime moduli.
newtype Mod (m :: Nat) = Mod
  { unMod :: Natural
  -- ^ The canonical representative of the residue class,
  -- always between 0 and \( m - 1 \) inclusively.
  --
  -- >>> :set -XDataKinds
  -- >>> -1 :: Mod 10
  -- (9 `modulo` 10)
  }
  deriving (Eq, Ord, Generic)

instance NFData (Mod m)

instance KnownNat m => Show (Mod m) where
  show m = "(" ++ show (unMod m) ++ " `modulo` " ++ show (natVal m) ++ ")"

instance KnownNat m => Enum (Mod m) where
  succ x = if x == maxBound then throw Overflow  else coerce (succ @Natural) x
  pred x = if x == minBound then throw Underflow else coerce (pred @Natural) x

  toEnum   = (fromIntegral :: Int -> Mod m)
  fromEnum = (fromIntegral :: Natural -> Int) . unMod

  enumFrom x       = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)

  enumFromTo     = coerce (enumFromTo     @Natural)
  enumFromThenTo = coerce (enumFromThenTo @Natural)

instance KnownNat m => Bounded (Mod m) where
  minBound = Mod 0
  maxBound = let mx = Mod (natVal mx - 1) in mx

addMod :: Natural -> Natural -> Natural -> Natural
addMod m x y = let z = x + y in if z >= m then z - m else z

subMod :: Natural -> Natural -> Natural -> Natural
subMod m x y = if x >= y then x - y else m + x - y

negateMod :: Natural -> Natural -> Natural
negateMod !_ 0 = 0
negateMod m x = m - x

mulMod :: Natural -> Natural -> Natural -> Natural
mulMod m x y = (x * y) `Prelude.rem` m

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

-- | If an argument is
-- <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- with the modulo, return its modular inverse.
-- Otherwise return 'Nothing'.
--
-- >>> :set -XDataKinds
-- >>> invertMod 3 :: Mod 10 -- 3 * 7 = 21 ≡ 1 (mod 10)
-- Just (7 `modulo` 10)
-- >>> invertMod 4 :: Mod 10 -- 4 and 10 are not coprime
-- Nothing
invertMod :: KnownNat m => Mod m -> Maybe (Mod m)
invertMod mx
  = if y <= 0
    then Nothing
    else Just $ Mod $ fromInteger y
  where
    y = recipModInteger (toInteger (unMod mx)) (toInteger (natVal mx))
{-# INLINABLE invertMod #-}

recipModInteger :: Integer -> Integer -> Integer
recipModInteger x m = case gcdExt x m of
  (1, s) -> s `mod` m
  _ -> -1

gcdExt :: Integer -> Integer -> (Integer, Integer)
gcdExt = go 1 0
  where
    go s !_ r 0 = (r, s)
    go s s' r r' = case Prelude.quotRem r r' of
      (q, r'') -> go s' (s - q * s') r' r''

-- | Drop-in replacement for 'Prelude.^' with much better performance.
-- Negative powers are allowed, but may throw 'DivideByZero', if an argument
-- is not <https://en.wikipedia.org/wiki/Coprime_integers coprime> with the modulo.
--
-- Building with @-O@ triggers a rewrite rule 'Prelude.^' = '^%'.
--
-- >>> :set -XDataKinds
-- >>> 3 ^% 4 :: Mod 10    -- 3 ^ 4 = 81 ≡ 1 (mod 10)
-- (1 `modulo` 10)
-- >>> 3 ^% (-1) :: Mod 10 -- 3 * 7 = 21 ≡ 1 (mod 10)
-- (7 `modulo` 10)
-- >>> 4 ^% (-1) :: Mod 10 -- 4 and 10 are not coprime
-- (*** Exception: divide by zero
(^%) :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
mx ^% a
  | a < 0     = case invertMod mx of
    Nothing ->  throw DivideByZero
    Just my ->  Mod $ powModNatural (unMod my) (fromIntegral' (-a)) (natVal mx)
  | otherwise = Mod $ powModNatural (unMod mx) (fromIntegral' a)    (natVal mx)
  where
#if __GLASGOW_HASKELL__ == 900 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1
    -- Cannot use fromIntegral because of https://gitlab.haskell.org/ghc/ghc/-/issues/19411
    fromIntegral' = fromInteger . toInteger
#else
    fromIntegral' = fromIntegral
#endif
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
"powMod/3/Word"        forall x. x ^% (3 :: Word)    = let u = x in u*u*u #-}

infixr 8 ^%
