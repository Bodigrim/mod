{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Mod
import qualified Data.Mod.Word as Word
import Data.Proxy
import Data.Semigroup
import GHC.TypeNats (KnownNat, SomeNat(..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes.Base

#ifdef MIN_VERSION_semirings
import Data.Semiring (Ring)
import Test.QuickCheck.Classes
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
  , testGroup "Random Mod" $
    [ testProperty "fromInteger" fromIntegerRandomProp
    , testProperty "invertMod"   invertModRandomProp
    , testProperty "powMod"      powModRandomProp
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
  , testGroup "Word.Mod 18446744073709551615" $
    testProperty "fromInteger"
      (fromIntegerWordProp (Proxy :: Proxy 18446744073709551615)) :
    testProperty "powMod"    (powModWordProp    @18446744073709551615) :
    testProperty "invertMod" (invertModWordProp @18446744073709551615) :
    map lawsToTest (laws (Proxy :: Proxy (Word.Mod 18446744073709551615)))
  , testGroup "Random Word.Mod" $
    [ testProperty "fromInteger" fromIntegerWordRandomProp
    , testProperty "invertMod"   invertModWordRandomProp
    , testProperty "invertMod near maxBound" invertModWordRandomProp_nearMaxBound
    , testProperty "powMod"      powModWordRandomProp
    ]
  ]

#ifdef MIN_VERSION_semirings
laws1 :: (Eq a, Ord a, Show a, Num a, Ring a, Arbitrary a) => Proxy a -> [Laws]
#else
laws1 :: (Eq a, Ord a, Show a, Num a, Arbitrary a) => Proxy a -> [Laws]
#endif
laws1 p =
    [ eqLaws          p
    , ordLaws         p
    , numLaws         p
    , showLaws        p
#ifdef MIN_VERSION_semirings
    , semiringLaws    p
    , ringLaws        p
#endif
    ]

#ifdef MIN_VERSION_semirings
laws :: (Eq a, Ord a, Show a, Num a, Ring a, Enum a, Bounded a, Arbitrary a) => Proxy a -> [Laws]
#else
laws :: (Eq a, Ord a, Show a, Num a, Enum a, Bounded a, Arbitrary a) => Proxy a -> [Laws]
#endif
laws p = boundedEnumLaws p : laws1 p

lawsToTest :: Laws -> TestTree
lawsToTest (Laws name props) =
  testGroup name $ map (uncurry testProperty) props

instance KnownNat m => Arbitrary (Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, fromInteger <$> arbitrary]
  shrink = map fromInteger . shrink . toInteger . unMod

instance KnownNat m => Arbitrary (Word.Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, fromInteger <$> arbitrary]
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

invertModWordRandomProp_nearMaxBound :: Word -> Integer -> Property
invertModWordRandomProp_nearMaxBound m n = m < maxBound ==>
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
powModRandomProp (Positive m) n k = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> powModProp (fromInteger n :: Mod m) k

powModProp :: KnownNat m => Mod m -> Int -> Property
powModProp x n
  | n >= 0 = x ^% n === getProduct (stimes n (Product x))
  | otherwise = case invertMod x of
    Nothing -> property True
    Just x' -> x ^% n === getProduct (stimes (-n) (Product x'))

powModWordRandomProp :: Word -> Integer -> Int -> Property
powModWordRandomProp m n k = m > 1 ==> case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> powModWordProp (fromInteger n :: Word.Mod m) k

powModWordProp :: KnownNat m => Word.Mod m -> Int -> Property
powModWordProp x n
  | n >= 0 = x Word.^% n === getProduct (stimes n (Product x))
  | otherwise = case Word.invertMod x of
    Nothing -> property True
    Just x' -> x Word.^% n === getProduct (stimes (-n) (Product x'))
