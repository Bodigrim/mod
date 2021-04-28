{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Bits
import Data.Mod
import qualified Data.Mod.Word as Word
import Data.Proxy
import Data.Semigroup
import Foreign.Storable (Storable(..))
import GHC.TypeNats (KnownNat, SomeNat(..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes.Base

#ifdef MIN_VERSION_semirings
import Data.Semiring (Ring)
import Test.QuickCheck.Classes (semiringLaws, ringLaws)
#endif

#ifdef MIN_VERSION_vector
import Data.Primitive (Prim)
import Data.Vector.Unboxed (Unbox)
import Test.QuickCheck.Classes (muvectorLaws, primLaws)
#endif

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testGroup "Mod 1" $
    testProperty "fromInteger"
      (fromIntegerProp (Proxy :: Proxy 1)) :
    map lawsToTest (laws1 (Proxy :: Proxy (Mod 1)))
  , testGroup "Mod 2310" $
    testProperty "fromInteger"
      (fromIntegerProp (Proxy :: Proxy 2310)) :
    testProperty "invertMod"   (invertModProp   @2310) :
    testProperty "powMod"      (powModProp      @2310) :
    map lawsToTest (laws (Proxy :: Proxy (Mod 2310)))
  , testGroup "Mod 18446744073709551615" $
    testProperty "fromInteger"
      (fromIntegerProp (Proxy :: Proxy 18446744073709551615)) :
    testProperty "invertMod"   (invertModProp   @18446744073709551615) :
    testProperty "powMod"      (powModProp      @18446744073709551615) :
    map lawsToTest (laws (Proxy :: Proxy (Mod 18446744073709551615)))
  , testGroup "Mod 18446744073709551626" $
    testProperty "fromInteger"
      (fromIntegerProp (Proxy :: Proxy 18446744073709551626)) :
    testProperty "powMod"      (powModProp      @18446744073709551626) :
    testProperty "invertMod"   (invertModProp   @18446744073709551626) :
    map lawsToTest (laws (Proxy :: Proxy (Mod 18446744073709551626)))
  , testGroup "Mod 123456789012345678901234567890" $
    testProperty "fromInteger"
      (fromIntegerProp (Proxy :: Proxy 123456789012345678901234567890)) :
    testProperty "powMod"      (powModProp      @123456789012345678901234567890) :
    testProperty "invertMod"   (invertModProp   @123456789012345678901234567890) :
    map lawsToTest (laws (Proxy :: Proxy (Mod 123456789012345678901234567890)))
  , testGroup "Random Mod"
    [ testProperty "fromInteger" fromIntegerRandomProp
    , testProperty "invertMod"   invertModRandomProp
    , testProperty "powMod"      powModRandomProp
    , testProperty "powMod on sum" powModRandomAdditiveProp
    , testProperty "powMod special case" powModCase
    ]

  , testGroup "Word.Mod 1" $
    testProperty "fromInteger"
      (fromIntegerWordProp (Proxy :: Proxy 1)) :
    map lawsToTest (laws1 (Proxy :: Proxy (Word.Mod 1)))
  , testGroup "Word.Mod 2310" $
    testProperty "fromInteger"
      (fromIntegerWordProp (Proxy :: Proxy 2310)) :
    testProperty "powMod"    (powModWordProp    @2310) :
    testProperty "invertMod" (invertModWordProp @2310) :
    map lawsToTest (laws (Proxy :: Proxy (Word.Mod 2310)))
  , if finiteBitSize (0 :: Word) == 64 then
      testGroup "Word.Mod 18446744073709551615" $
      testProperty "fromInteger"
        (fromIntegerWordProp (Proxy :: Proxy 18446744073709551615)) :
      testProperty "powMod"    (powModWordProp    @18446744073709551615) :
      testProperty "invertMod" (invertModWordProp @18446744073709551615) :
      map lawsToTest (laws (Proxy :: Proxy (Word.Mod 18446744073709551615)))
    else
      testGroup "Word.Mod 4294967295" $
      testProperty "fromInteger"
        (fromIntegerWordProp (Proxy :: Proxy 4294967295)) :
      testProperty "powMod"    (powModWordProp    @4294967295) :
      testProperty "invertMod" (invertModWordProp @4294967295) :
      map lawsToTest (laws (Proxy :: Proxy (Word.Mod 4294967295)))
  , testGroup "Random Word.Mod"
    [ testProperty "fromInteger" fromIntegerWordRandomProp
    , testProperty "invertMod"   invertModWordRandomProp
    , testProperty "invertMod near maxBound" invertModWordRandomPropNearMaxBound
    , testProperty "powMod"      powModWordRandomProp
    , testProperty "powMod on sum" powModWordRandomAdditiveProp
    , testProperty "powMod special case" powModWordCase
    ]
  ]

#ifdef MIN_VERSION_semirings
#ifdef MIN_VERSION_vector
laws1 :: (Eq a, Ord a, Show a, Num a, Storable a, Ring a, Prim a, Unbox a, Arbitrary a) => Proxy a -> [Laws]
#else
laws1 :: (Eq a, Ord a, Show a, Num a, Storable a, Ring a, Arbitrary a) => Proxy a -> [Laws]
#endif
#else
#ifdef MIN_VERSION_vector
laws1 :: (Eq a, Ord a, Show a, Num a, Storable a, Prim a, Unbox a, Arbitrary a) => Proxy a -> [Laws]
#else
laws1 :: (Eq a, Ord a, Show a, Num a, Storable a, Arbitrary a) => Proxy a -> [Laws]
#endif
#endif
laws1 p =
    [ eqLaws          p
    , ordLaws         p
    , numLaws         p
    , showLaws        p
    , storableLaws    p
#ifdef MIN_VERSION_semirings
    , semiringLaws    p
    , ringLaws        p
#endif
#ifdef MIN_VERSION_vector
    , primLaws        p
    , muvectorLaws    p
#endif
    ]

#ifdef MIN_VERSION_semirings
#ifdef MIN_VERSION_vector
laws :: (Eq a, Ord a, Show a, Num a, Storable a, Ring a, Enum a, Bounded a, Prim a, Unbox a, Arbitrary a) => Proxy a -> [Laws]
#else
laws :: (Eq a, Ord a, Show a, Num a, Storable a, Ring a, Enum a, Bounded a, Arbitrary a) => Proxy a -> [Laws]
#endif
#else
#ifdef MIN_VERSION_vector
laws :: (Eq a, Ord a, Show a, Num a, Storable a, Enum a, Bounded a, Prim a, Unbox a, Arbitrary a) => Proxy a -> [Laws]
#else
laws :: (Eq a, Ord a, Show a, Num a, Storable a, Enum a, Bounded a, Arbitrary a) => Proxy a -> [Laws]
#endif
#endif
laws p = boundedEnumLaws p : laws1 p

lawsToTest :: Laws -> TestTree
lawsToTest (Laws name props) =
  testGroup name $ map (uncurry testProperty) props

instance KnownNat m => Arbitrary (Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, negate <$> arbitraryBoundedEnum, fromInteger <$> arbitrary]
  shrink = map fromInteger . shrink . toInteger . unMod

instance KnownNat m => Arbitrary (Word.Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, negate <$> arbitraryBoundedEnum, fromInteger <$> arbitrary]
  shrink = map fromIntegral . shrink . Word.unMod

-------------------------------------------------------------------------------
-- fromInteger

fromIntegerRandomProp :: Positive Integer -> Integer -> Property
fromIntegerRandomProp (Positive m) n = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat p -> fromIntegerProp p n

fromIntegerProp :: forall m. KnownNat m => Proxy m -> Integer -> Property
fromIntegerProp p n = unMod m === fromInteger (n `mod` toInteger (natVal p))
  where
    m :: Mod m
    m = fromInteger n

fromIntegerWordRandomProp :: Word -> Integer -> Property
fromIntegerWordRandomProp m n = m > 1 ==> case someNatVal (fromIntegral m) of
  SomeNat p -> fromIntegerWordProp p n

fromIntegerWordProp :: forall m. KnownNat m => Proxy m -> Integer -> Property
fromIntegerWordProp p n = Word.unMod m === fromInteger (n `mod` toInteger (natVal p))
  where
    m :: Word.Mod m
    m = fromInteger n

-------------------------------------------------------------------------------
-- invertMod

invertModRandomProp :: Positive Integer -> Integer -> Property
invertModRandomProp (Positive m) n = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> invertModProp (fromInteger n :: Mod m)

invertModProp :: KnownNat m => Mod m -> Property
invertModProp x = case invertMod x of
  Nothing -> g =/= 1
  Just x' -> g === 1 .&&. x * x' === 1 .&&. x' * x === 1 .&&. x' === x ^% (-1 :: Int)
  where
    g = gcd (unMod x) (fromIntegral (natVal x))

invertModWordRandomProp :: Word -> Integer -> Property
invertModWordRandomProp m n = m > 1 ==> case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> invertModWordProp (fromInteger n :: Word.Mod m)

invertModWordRandomPropNearMaxBound :: Word -> Integer -> Property
invertModWordRandomPropNearMaxBound m n = m < maxBound ==>
  case someNatVal (fromIntegral (maxBound - m)) of
    SomeNat (Proxy :: Proxy m) -> invertModWordProp (fromInteger n :: Word.Mod m)

invertModWordProp :: KnownNat m => Word.Mod m -> Property
invertModWordProp x = case Word.invertMod x of
  Nothing -> g =/= 1
  Just x' -> g === 1 .&&. x * x' === 1 .&&. x' * x === 1 .&&. x' === x Word.^% (-1 :: Int)
  where
    g = gcd (Word.unMod x) (fromIntegral (natVal x))

-------------------------------------------------------------------------------
-- powMod

powModRandomProp :: Positive Integer -> Integer -> Int -> Property
powModRandomProp (Positive m) x n = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> powModProp (fromInteger x :: Mod m) n

powModProp :: KnownNat m => Mod m -> Int -> Property
powModProp x n
  | n >= 0 = x ^% n === getProduct (stimes n (Product x))
  | otherwise = case invertMod x of
    Nothing -> property True
    Just x' -> x ^% n === getProduct (stimes (-n) (Product x'))

powModRandomAdditiveProp :: Positive Integer -> Integer -> Huge Integer -> Huge Integer -> Property
powModRandomAdditiveProp (Positive m) x (Huge n1) (Huge n2) = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> powModAdditiveProp (fromInteger x :: Mod m) n1 n2

powModAdditiveProp :: KnownNat m => Mod m -> Integer -> Integer -> Property
powModAdditiveProp x n1 n2
  | invertMod x == Nothing, n1 < 0 || n2 < 0
  = property True
  | otherwise
  = (x ^% n1) * (x ^% n2) === x ^% (n1 + n2)

powModCase :: Property
powModCase = once $ 0 ^% n === (0 :: Mod 2)
  where
    n = 1 `shiftL` 64 :: Integer

powModWordRandomProp :: Word -> Integer -> Int -> Property
powModWordRandomProp m x k = m > 1 ==> case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> powModWordProp (fromInteger x :: Word.Mod m) k

powModWordProp :: KnownNat m => Word.Mod m -> Int -> Property
powModWordProp x n
  | n >= 0 = x Word.^% n === getProduct (stimes n (Product x))
  | otherwise = case Word.invertMod x of
    Nothing -> property True
    Just x' -> x Word.^% n === getProduct (stimes (-n) (Product x'))

powModWordRandomAdditiveProp :: Word -> Integer -> Huge Integer -> Huge Integer -> Property
powModWordRandomAdditiveProp m x (Huge n1) (Huge n2) = m > 1 ==> case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> powModWordAdditiveProp (fromInteger x :: Word.Mod m) n1 n2

powModWordAdditiveProp :: KnownNat m => Word.Mod m -> Integer -> Integer -> Property
powModWordAdditiveProp x n1 n2
  | Word.invertMod x == Nothing, n1 < 0 || n2 < 0
  = property True
  | otherwise
  = (x Word.^% n1) * (x Word.^% n2) === x Word.^% (n1 + n2)

powModWordCase :: Property
powModWordCase = once $ 0 Word.^% n === (0 :: Word.Mod 2)
  where
    n = 1 `shiftL` 64 :: Integer

newtype Huge a = Huge { _getHuge :: a }
  deriving (Show)

instance (Bits a, Num a, Arbitrary a) => Arbitrary (Huge a) where
  arbitrary = do
    Positive l <- arbitrary
    ds <- vector l
    return $ Huge $ foldl1 (\acc n -> acc `shiftL` 63 + n) ds
  shrink (Huge n) = Huge <$> shrink n
