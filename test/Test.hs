{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Exception (evaluate, try, ArithException(..))
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
import qualified Data.Euclidean as E
import Data.Semiring (Ring, Semiring(..))
import qualified Data.Set as S
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
#ifdef MIN_VERSION_semirings
    , testProperty "divide"  dividePropRandom
    , testProperty "gcd"     gcdIsPrincipalIdealRandom
    , testProperty "lcm"     lcmIsIntersectionOfIdealsRandom
    , testProperty "coprime" coprimeGeneratorsRandom
    , testProperty "quotRem" quotRemPropRandom
    , testProperty "degree"  degreePropRandom
#endif
    ]
  , testGroup "Mod 0"
    [ testProperty "0"            (isDivideByZero 0)
    , testProperty "1"            (isDivideByZero 1)
    , testProperty "minBound"     (isDivideByZero minBound)
    , testProperty "maxBound"     (isDivideByZero maxBound)
    , testProperty "toEnum"       (isDivideByZero (toEnum 0))
    , testProperty "fromRational" (isDivideByZero (fromRational 0))
#ifdef MIN_VERSION_semirings
    , testProperty "zero"         (isDivideByZero zero)
    , testProperty "one"          (isDivideByZero one)
    , testProperty "fromNatural"  (isDivideByZero (fromNatural 0))
#endif
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
#ifdef MIN_VERSION_semirings
    , testProperty "divide"  divideWordPropRandom
    , testProperty "gcd"     gcdIsPrincipalIdealWordRandom
    , testProperty "lcm"     lcmIsIntersectionOfIdealsWordRandom
    , testProperty "coprime" coprimeGeneratorsWordRandom
    , testProperty "quotRem" quotRemWordPropRandom
    , testProperty "degree"  degreeWordPropRandom
#endif
    ]
  , testGroup "Word.Mod 0"
    [ testProperty "0"            (isDivideByZeroWord 0)
    , testProperty "1"            (isDivideByZeroWord 1)
    , testProperty "minBound"     (isDivideByZeroWord minBound)
    , testProperty "maxBound"     (isDivideByZeroWord maxBound)
    , testProperty "toEnum"       (isDivideByZeroWord (toEnum 0))
    , testProperty "fromRational" (isDivideByZeroWord (fromRational 0))
#ifdef MIN_VERSION_semirings
    , testProperty "zero"         (isDivideByZeroWord zero)
    , testProperty "one"          (isDivideByZeroWord one)
    , testProperty "fromNatural"  (isDivideByZeroWord (fromNatural 0))
#endif
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

-------------------------------------------------------------------------------
-- DivideByZero

isDivideByZero :: Mod 0 -> Property
isDivideByZero x = ioProperty ((=== Left DivideByZero) <$> try (evaluate x))

isDivideByZeroWord :: Word.Mod 0 -> Property
isDivideByZeroWord x = ioProperty ((=== Left DivideByZero) <$> try (evaluate x))

-------------------------------------------------------------------------------
-- Ideals

#ifdef MIN_VERSION_semirings

dividePropRandom :: Positive (Small Integer) -> Positive Integer -> Positive Integer -> Property
dividePropRandom (Positive (Small m)) (Positive x) (Positive y) = case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> divideProp (fromInteger x :: Mod m) (fromInteger y)

divideProp :: KnownNat m => Mod m -> Mod m -> Property
divideProp x y = case E.divide x y of
  Just z -> x === y * z
  Nothing -> filter ((== x) . (* y)) [minBound .. maxBound] === []

gcdIsPrincipalIdealRandom :: Positive (Small Integer) -> Integer -> Integer -> Property
gcdIsPrincipalIdealRandom (Positive (Small m)) x y = case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> gcdIsPrincipalIdeal (fromInteger x :: Mod m) (fromInteger y)

gcdIsPrincipalIdeal :: KnownNat m => Mod m -> Mod m -> Property
gcdIsPrincipalIdeal x y = addIdeals (genIdeal x) (genIdeal y) === genIdeal (E.gcd x y)
  where
    genIdeal t = S.fromList $ map (* t) [minBound .. maxBound]
    addIdeals us vs = S.fromList [ u + v | u <- S.toList us, v <- S.toList vs ]

lcmIsIntersectionOfIdealsRandom :: Positive (Small Integer) -> Integer -> Integer -> Property
lcmIsIntersectionOfIdealsRandom (Positive (Small m)) x y = case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> lcmIsIntersectionOfIdeals (fromInteger x :: Mod m) (fromInteger y)

lcmIsIntersectionOfIdeals :: KnownNat m => Mod m -> Mod m -> Property
lcmIsIntersectionOfIdeals x y = S.intersection (genIdeal x) (genIdeal y) === genIdeal (E.lcm x y)
  where
    genIdeal t = S.fromList $ map (* t) [minBound .. maxBound]

coprimeGeneratorsRandom :: Positive (Small Integer) -> Integer -> Integer -> Property
coprimeGeneratorsRandom (Positive (Small m)) x y = case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> coprimeGenerators (fromInteger x :: Mod m) (fromInteger y)

coprimeGenerators :: KnownNat m => Mod m -> Mod m -> Property
coprimeGenerators x y = E.coprime x y === (addIdeals (genIdeal x) (genIdeal y) == S.fromList [minBound .. maxBound])
  where
    genIdeal t = S.fromList $ map (* t) [minBound .. maxBound]
    addIdeals us vs = S.fromList [ u + v | u <- S.toList us, v <- S.toList vs ]

quotRemPropRandom :: Positive (Small Integer) -> Positive Integer -> Positive Integer -> Property
quotRemPropRandom (Positive (Small m)) (Positive x) (Positive y) = case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> quotRemProp (fromInteger x :: Mod m) (fromInteger y)

quotRemProp :: KnownNat m => Mod m -> Mod m -> Property
quotRemProp x y = case E.divide x y of
  Just z -> E.quotRem x y === (z, 0)
  Nothing -> y /= 0 ==> let (q, r) = E.quotRem x y in
    counterexample (show (q, r)) $ x === q * y + r

degreePropRandom :: Positive (Small Integer) -> Positive Integer -> Positive Integer -> Property
degreePropRandom (Positive (Small m)) (Positive x) (Positive y) = case someNatVal (fromInteger m) of
  SomeNat (Proxy :: Proxy m) -> degreeProp (fromInteger x :: Mod m) (fromInteger y)

degreeProp :: KnownNat m => Mod m -> Mod m -> Property
degreeProp x y = ioProperty $ do
  ret <- try (evaluate (E.quotRem x y))
  pure $ case ret of
    Left DivideByZero -> property True
    Left{}            -> property False
    Right (_, r)      -> r === 0 .||. property (E.degree r < E.degree y)

divideWordPropRandom :: Positive Word -> Word -> Word -> Property
divideWordPropRandom (Positive m) x y = case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> divideWordProp (fromIntegral x :: Word.Mod m) (fromIntegral y)

divideWordProp :: KnownNat m => Word.Mod m -> Word.Mod m -> Property
divideWordProp x y = case E.divide x y of
  Just z -> x === y * z
  Nothing -> filter ((== x) . (* y)) [minBound .. maxBound] === []

gcdIsPrincipalIdealWordRandom :: Positive Word -> Word -> Word -> Property
gcdIsPrincipalIdealWordRandom (Positive m) x y = case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> gcdIsPrincipalIdealWord (fromIntegral x :: Word.Mod m) (fromIntegral y)

gcdIsPrincipalIdealWord :: KnownNat m => Word.Mod m -> Word.Mod m -> Property
gcdIsPrincipalIdealWord x y = addIdeals (genIdeal x) (genIdeal y) === genIdeal (E.gcd x y)
  where
    genIdeal t = S.fromList $ map (* t) [minBound .. maxBound]
    addIdeals us vs = S.fromList [ u + v | u <- S.toList us, v <- S.toList vs ]

lcmIsIntersectionOfIdealsWordRandom :: Positive Word -> Word -> Word -> Property
lcmIsIntersectionOfIdealsWordRandom (Positive m) x y = case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> lcmIsIntersectionOfIdealsWord (fromIntegral x :: Word.Mod m) (fromIntegral y)

lcmIsIntersectionOfIdealsWord :: KnownNat m => Word.Mod m -> Word.Mod m -> Property
lcmIsIntersectionOfIdealsWord x y = S.intersection (genIdeal x) (genIdeal y) === genIdeal (E.lcm x y)
  where
    genIdeal t = S.fromList $ map (* t) [minBound .. maxBound]

coprimeGeneratorsWordRandom :: Positive Word -> Word -> Word -> Property
coprimeGeneratorsWordRandom (Positive m) x y = case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> coprimeGeneratorsWord (fromIntegral x :: Word.Mod m) (fromIntegral y)

coprimeGeneratorsWord :: KnownNat m => Word.Mod m -> Word.Mod m -> Property
coprimeGeneratorsWord x y = E.coprime x y === (addIdeals (genIdeal x) (genIdeal y) == S.fromList [minBound .. maxBound])
  where
    genIdeal t = S.fromList $ map (* t) [minBound .. maxBound]
    addIdeals us vs = S.fromList [ u + v | u <- S.toList us, v <- S.toList vs ]

quotRemWordPropRandom :: Positive Word -> Word -> Word -> Property
quotRemWordPropRandom (Positive m) x y = case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> quotRemWordProp (fromIntegral x :: Word.Mod m) (fromIntegral y)

quotRemWordProp :: KnownNat m => Word.Mod m -> Word.Mod m -> Property
quotRemWordProp x y = case E.divide x y of
  Just z -> E.quotRem x y === (z, 0)
  Nothing -> y /= 0 ==> let (q, r) = E.quotRem x y in
    counterexample (show (q, r)) $ x === q * y + r

degreeWordPropRandom :: Positive Word -> Word -> Word -> Property
degreeWordPropRandom (Positive m) x y = case someNatVal (fromIntegral m) of
  SomeNat (Proxy :: Proxy m) -> degreeWordProp (fromIntegral x :: Word.Mod m) (fromIntegral y)

degreeWordProp :: KnownNat m => Word.Mod m -> Word.Mod m -> Property
degreeWordProp x y = ioProperty $ do
  ret <- try (evaluate (E.quotRem x y))
  pure $ case ret of
    Left DivideByZero -> property True
    Left{}            -> property False
    Right (_, r)      -> r === 0 .||. property (E.degree r < E.degree y)

#endif
