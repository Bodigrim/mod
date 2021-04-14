-- |
-- Module:      Data.Mod
-- Copyright:   (c) 2017-2020 Andrew Lelechenko
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
{-# LANGUAGE MagicHash             #-}
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
import Control.Monad
import Data.Bits
import Data.Ratio
import Data.Word (Word8)
#ifdef MIN_VERSION_semirings
import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Semiring (Semiring(..), Ring(..))
#endif
#ifdef MIN_VERSION_vector
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Primitive.Types        as P
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Primitive       as P
import Foreign (copyBytes)
import GHC.IO.Unsafe (unsafeDupablePerformIO)
#endif
import Foreign.Storable (Storable(..))
import GHC.Exts
import GHC.Generics
import GHC.Integer.GMP.Internals
import GHC.Natural (Natural(..), powModNatural)
import GHC.TypeNats (Nat, KnownNat, natVal, natVal')

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

bigNatToNat :: BigNat -> Natural
bigNatToNat r# =
  if isTrue# (sizeofBigNat# r# <=# 1#) then NatS# (bigNatToWord r#) else NatJ# r#

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

wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

lgWordSize :: Int
lgWordSize = case wordSize of
  32 -> 2 -- 2^2 bytes in word
  64 -> 3 -- 2^3 bytes in word
  _  -> error "lgWordSize: unknown architecture"

instance KnownNat m => Storable (Mod m) where
  sizeOf _ = case natVal' (proxy# :: Proxy# m) of
    NatS#{}  -> sizeOf (0 :: Word)
    NatJ# m# -> I# (sizeofBigNat# m#) `shiftL` lgWordSize
  {-# INLINE sizeOf #-}

  alignment _ = alignment (0 :: Word)
  {-# INLINE alignment #-}

  peek (Ptr addr#) = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> do
      W# w# <- peek (Ptr addr#)
      pure . Mod $! NatS# w#
    NatJ# m# -> do
      let !(I# lgWordSize#) = lgWordSize
          sz# = sizeofBigNat# m# `iShiftL#` lgWordSize#
      bn <- importBigNatFromAddr addr# (int2Word# sz#) 0#
      pure . Mod $! bigNatToNat bn
  {-# INLINE peek #-}

  poke (Ptr addr#) (Mod x) = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> case x of
      NatS# x# -> poke (Ptr addr#) (W# x#)
      _        -> brokenInvariant
    NatJ# m# -> case x of
      NatS# x# -> do
        poke (Ptr addr#) (W# x#)
        forM_ [1 .. sz - 1] $ \off ->
          pokeElemOff (Ptr addr#) off (0 :: Word)
      NatJ# bn -> do
        l <- exportBigNatToAddr bn addr# 0#
        forM_ [(fromIntegral :: Word -> Int) l .. (sz `shiftL` lgWordSize) - 1] $ \off ->
          pokeElemOff (Ptr addr#) off (0 :: Word8)
      where
        sz = I# (sizeofBigNat# m#)
  {-# INLINE poke #-}

#ifdef MIN_VERSION_vector

instance KnownNat m => P.Prim (Mod m) where
  sizeOf# x    = let !(I# sz#) = sizeOf x    in sz#
  {-# INLINE sizeOf# #-}

  alignment# x = let !(I# a#)  = alignment x in a#
  {-# INLINE alignment# #-}

  indexByteArray# arr# i' = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> Mod (NatS# w#)
      where
        !(W# w#) = P.indexByteArray# arr# i'
    NatJ# m# -> Mod $ bigNatToNat $ importBigNatFromByteArray arr# (int2Word# i#) (int2Word# sz#) 0#
      where
        !(I# lgWordSize#) = lgWordSize
        sz# = sizeofBigNat# m# `iShiftL#` lgWordSize#
        i# = i' *# sz#
  {-# INLINE indexByteArray# #-}

  indexOffAddr# arr# i' = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> Mod (NatS# w#)
      where
        !(W# w#) = P.indexOffAddr# arr# i'
    NatJ# m# -> Mod $ bigNatToNat $ unsafeDupablePerformIO $ importBigNatFromAddr (arr# `plusAddr#` i#) (int2Word# sz#) 0#
      where
        !(I# lgWordSize#) = lgWordSize
        sz# = sizeofBigNat# m# `iShiftL#` lgWordSize#
        i# = i' *# sz#
  {-# INLINE indexOffAddr# #-}

  readByteArray# marr !i' token = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> case P.readByteArray# marr i' token of
      (# newToken, W# w# #) -> (# newToken, Mod (NatS# w#) #)
    NatJ# m# -> case unsafeFreezeByteArray# marr token of
      (# newToken, arr #) -> (# newToken, Mod (bigNatToNat (importBigNatFromByteArray arr (int2Word# i#) (int2Word# sz#) 0#)) #)
      where
        !(I# lgWordSize#) = lgWordSize
        sz# = sizeofBigNat# m# `iShiftL#` lgWordSize#
        i# = i' *# sz#
  {-# INLINE readByteArray# #-}

  readOffAddr# marr !i' token = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> case P.readOffAddr# marr i' token of
      (# newToken, W# w# #) -> (# newToken, Mod (NatS# w#) #)
    NatJ# m# -> case internal (unsafeIOToPrim (importBigNatFromAddr (marr `plusAddr#` i#) (int2Word# sz#) 0#) :: ST s BigNat) token of
      (# newToken, bn #) -> (# newToken, Mod (bigNatToNat bn) #)
      where
        !(I# lgWordSize#) = lgWordSize
        sz# = sizeofBigNat# m# `iShiftL#` lgWordSize#
        i# = i' *# sz#
  {-# INLINE readOffAddr# #-}

  writeByteArray# marr !i' !(Mod x) token = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> case x of
      NatS# x# -> P.writeByteArray# marr i' (W# x#) token
      _        -> error "argument is larger than modulo"
    NatJ# m# -> case x of
      NatS# x# -> case P.writeByteArray# marr i# (W# x#) token of
        newToken -> P.setByteArray# marr (i# +# 1#) (sz# -# 1#) (0 :: Word) newToken
      NatJ# bn -> case internal (unsafeIOToPrim (exportBigNatToMutableByteArray bn (unsafeCoerce# marr) (int2Word# (i# `iShiftL#` lgWordSize#)) 0#) :: ST s Word) token of
        (# newToken, W# l# #) -> P.setByteArray# marr (i# `iShiftL#` lgWordSize# +# word2Int# l#) (sz# `iShiftL#` lgWordSize# -# word2Int# l#) (0 :: Word8) newToken
      where
        !(I# lgWordSize#) = lgWordSize
        !sz@(I# sz#) = I# (sizeofBigNat# m#)
        !(I# i#)     = I# i' * sz
  {-# INLINE writeByteArray# #-}

  writeOffAddr# marr !i' !(Mod x) token = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> case x of
      NatS# x# -> P.writeOffAddr# marr i' (W# x#) token
      _        -> error "argument is larger than modulo"
    NatJ# m# -> case x of
      NatS# x# -> case P.writeOffAddr# marr i# (W# x#) token of
        newToken -> P.setOffAddr# marr (i# +# 1#) (sz# -# 1#) (0 :: Word) newToken
      NatJ# bn -> case internal (unsafeIOToPrim (exportBigNatToAddr bn (marr `plusAddr#` (i# `iShiftL#` lgWordSize#)) 0#) :: ST s Word) token of
        (# newToken, W# l# #) -> P.setOffAddr# marr (i# `iShiftL#` lgWordSize# +# word2Int# l#) (sz# `iShiftL#` lgWordSize# -# word2Int# l#) (0 :: Word8) newToken
      where
        !(I# lgWordSize#) = lgWordSize
        !sz@(I# sz#) = I# (sizeofBigNat# m#)
        !(I# i#)   = I# i' * sz
  {-# INLINE writeOffAddr# #-}

  setByteArray# !_ !_ 0# !_ token = token
  setByteArray# marr off len mx@(Mod x) token = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> case x of
      NatS# x# -> P.setByteArray# marr off len (W# x#) token
      _        -> error "argument is larger than modulo"
    NatJ# m# -> case P.writeByteArray# marr off mx token of
      newToken -> doSet (sz `iShiftL#` lgWordSize#) newToken
      where
        !(I# lgWordSize#) = lgWordSize
        sz = sizeofBigNat# m#
        off' = (off *# sz) `iShiftL#` lgWordSize#
        len' = (len *# sz) `iShiftL#` lgWordSize#
        doSet i tkn
          | isTrue# (2# *# i <# len') = case copyMutableByteArray# marr off' marr (off' +# i) i tkn of
            tkn' -> doSet (2# *# i) tkn'
          | otherwise    = copyMutableByteArray# marr off' marr (off' +# i) (len' -# i) tkn
  {-# INLINE setByteArray# #-}

  setOffAddr# !_ !_ 0# !_ token = token
  setOffAddr# marr off len mx@(Mod x) token = case natVal' (proxy# :: Proxy# m) of
    NatS#{} -> case x of
      NatS# x# -> P.setOffAddr# marr off len (W# x#) token
      _        -> error "argument is larger than modulo"
    NatJ# m# -> case P.writeOffAddr# marr off mx token of
      newToken -> doSet (sz `iShiftL#` lgWordSize#) newToken
      where
        !(I# lgWordSize#) = lgWordSize
        sz = sizeofBigNat# m#
        off' = (off *# sz) `iShiftL#` lgWordSize#
        len' = (len *# sz) `iShiftL#` lgWordSize#
        doSet i tkn -- = tkn
          | isTrue# (2# *# i <# len') = case internal (unsafeIOToPrim (copyBytes (Ptr (marr `plusAddr#` (off' +# i))) (Ptr (marr `plusAddr#` off')) (I# i)) :: ST s ()) tkn of
            (# tkn', () #) -> doSet (2# *# i) tkn'
          | otherwise    = case internal (unsafeIOToPrim (copyBytes (Ptr (marr `plusAddr#` (off' +# i))) (Ptr (marr `plusAddr#` off')) (I# (len' -# i))) :: ST s ()) tkn of
            (# tkn', () #) -> tkn'
  {-# INLINE setOffAddr# #-}

-- | Unboxed vectors of 'Mod' cause more nursery allocations
-- than boxed ones, but reduce pressure on garbage collector,
-- especially for large vectors.
newtype instance U.MVector s (Mod m) = ModMVec (P.MVector s (Mod m))

-- | Unboxed vectors of 'Mod' cause more nursery allocations
-- than boxed ones, but reduce pressure on garbage collector,
-- especially for large vectors.
newtype instance U.Vector    (Mod m) = ModVec  (P.Vector (Mod m))

instance KnownNat m => U.Unbox (Mod m)

instance KnownNat m => M.MVector U.MVector (Mod m) where
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
  basicLength (ModMVec v) = M.basicLength v
  basicUnsafeSlice i n (ModMVec v) = ModMVec $ M.basicUnsafeSlice i n v
  basicOverlaps (ModMVec v1) (ModMVec v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = ModMVec `liftM` M.basicUnsafeNew n
  basicInitialize (ModMVec v) = M.basicInitialize v
  basicUnsafeReplicate n x = ModMVec `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (ModMVec v) i = M.basicUnsafeRead v i
  basicUnsafeWrite (ModMVec v) i x = M.basicUnsafeWrite v i x
  basicClear (ModMVec v) = M.basicClear v
  basicSet (ModMVec v) x = M.basicSet v x
  basicUnsafeCopy (ModMVec v1) (ModMVec v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (ModMVec v1) (ModMVec v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (ModMVec v) n = ModMVec `liftM` M.basicUnsafeGrow v n

instance KnownNat m => G.Vector U.Vector (Mod m) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (ModMVec v) = ModVec `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (ModVec v) = ModMVec `liftM` G.basicUnsafeThaw v
  basicLength (ModVec v) = G.basicLength v
  basicUnsafeSlice i n (ModVec v) = ModVec $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (ModVec v) i = G.basicUnsafeIndexM v i
  basicUnsafeCopy (ModMVec mv) (ModVec v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

#endif
