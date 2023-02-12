{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-name-shadowing #-}

module Main where

import Data.Proxy
import Test.Tasty.Bench

import qualified Data.Mod
import qualified Data.Mod.Word
#ifdef MIN_VERSION_finite_field
import qualified Data.FiniteField.PrimeField
#endif
#ifdef MIN_VERSION_finite_typelits
import qualified Data.Finite
#endif
#ifdef MIN_VERSION_modular_arithmetic
import qualified Data.Modular
#endif
#ifdef MIN_VERSION_modular
import qualified Numeric.Modular
#endif

type P = 20000003

#ifdef MIN_VERSION_modular
forceModular :: Numeric.Modular.Mod P -> Numeric.Modular.Mod P
forceModular a = (a == a) `seq` a
#endif

benchSum :: Benchmark
benchSum = bgroup "Sum"
  [ measure "Data.Mod" (Proxy @Data.Mod.Mod)
  , cmp $ measure "Data.Mod.Word" (Proxy @Data.Mod.Word.Mod)
#ifdef MIN_VERSION_finite_field
  , cmp $ measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_finite_typelits
  , cmp $ measure "finite-typelits" (Proxy @Data.Finite.Finite)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , cmp $ measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
#ifdef MIN_VERSION_modular
  , cmp $ bench "modular" $ nf (show . sumNModular) lim
#endif
  ]
  where
    cmp = bcompare "$NF == \"Data.Mod\" && $(NF-1) == \"Sum\""
    lim = 20000000

    measure :: (Eq (t P), Num (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (sumN p) lim
    {-# INLINE measure #-}

    sumN :: forall t. (Eq (t P), Num (t P)) => Proxy t -> Int -> t P
    sumN = const $ \n -> go 0 (fromIntegral n)
      where
        go :: t P -> t P -> t P
        go !acc 0 = acc
        go acc n = go (acc + n) (n - 1)
    {-# INLINE sumN #-}

#ifdef MIN_VERSION_modular
    sumNModular :: Int -> Numeric.Modular.Mod P
    sumNModular = \n -> go 0 (fromIntegral n)
      where
        go :: Numeric.Modular.Mod P -> Numeric.Modular.Mod P -> Numeric.Modular.Mod P
        go acc@(forceModular -> !_) 0 = acc
        go acc n = go (acc + n) (n - 1)
    {-# INLINE sumNModular #-}
#endif

benchProduct :: Benchmark
benchProduct = bgroup "Product"
  [ measure "Data.Mod" (Proxy @Data.Mod.Mod)
  , cmp $ measure "Data.Mod.Word" (Proxy @Data.Mod.Word.Mod)
#ifdef MIN_VERSION_finite_field
  , cmp $ measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_finite_typelits
  , cmp $ measure "finite-typelits" (Proxy @Data.Finite.Finite)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , cmp $ measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
#ifdef MIN_VERSION_modular
  , cmp $ bench "modular" $ nf (show . productNModular) lim
#endif
  ]
  where
    cmp = bcompare "$NF == \"Data.Mod\" && $(NF-1) == \"Product\""
    lim = 20000000

    measure :: (Eq (t P), Num (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (productN p) lim
    {-# INLINE measure #-}

    productN :: forall t. (Eq (t P), Num (t P)) => Proxy t -> Int -> t P
    productN = const $ \n -> go 1 (fromIntegral n)
      where
        go :: t P -> t P -> t P
        go !acc 0 = acc
        go acc n = go (acc * n) (n - 1)
    {-# INLINE productN #-}

#ifdef MIN_VERSION_modular
    productNModular :: Int -> Numeric.Modular.Mod P
    productNModular = \n -> go 1 (fromIntegral n)
      where
        go :: Numeric.Modular.Mod P -> Numeric.Modular.Mod P -> Numeric.Modular.Mod P
        go acc@(forceModular -> !_) 0 = acc
        go acc n = go (acc * n) (n - 1)
    {-# INLINE productNModular #-}
#endif

benchInversion :: Benchmark
benchInversion = bgroup "Inversion"
  [ measure "Data.Mod" (Proxy @Data.Mod.Mod)
  , cmp $ measure "Data.Mod.Word" (Proxy @Data.Mod.Word.Mod)
#ifdef MIN_VERSION_finite_field
  , cmp $ measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , cmp $ measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
  ]
  where
    cmp = bcompare "$NF == \"Data.Mod\" && $(NF-1) == \"Inversion\""
    lim = 1500000

    measure :: (Eq (t P), Fractional (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (invertN p) lim
    {-# INLINE measure #-}

    invertN :: forall t. (Eq (t P), Fractional (t P)) => Proxy t -> Int -> t P
    invertN = const $ \n -> go 0 (fromIntegral n)
      where
        go :: t P -> t P -> t P
        go !acc 0 = acc
        go acc n = go (acc + recip n) (n - 1)
    {-# INLINE invertN #-}

benchPower :: Benchmark
benchPower = bgroup "Power"
  [ bench "Data.Mod" $ nf powerNMod lim
  , cmp $ bench "Data.Mod.Word" $ nf powerNModWord lim
#ifdef MIN_VERSION_finite_field
  , cmp $ measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_finite_typelits
  , cmp $ measure "finite-typelits" (Proxy @Data.Finite.Finite)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , cmp $ measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
#ifdef MIN_VERSION_modular
  , cmp $ bench "modular" $ nf (show . powerNModular) lim
#endif
  ]
  where
    cmp = bcompare "$NF == \"Data.Mod\" && $(NF-1) == \"Power\""
    lim = 1000000

    powerNMod :: Int -> Data.Mod.Mod P
    powerNMod = go 0
      where
        go :: Data.Mod.Mod P -> Int -> Data.Mod.Mod P
        go !acc 0 = acc
        go acc n = go (acc + 2 Data.Mod.^% n) (n - 1)
    {-# INLINE powerNMod #-}

    powerNModWord :: Int -> Data.Mod.Word.Mod P
    powerNModWord = go 0
      where
        go :: Data.Mod.Word.Mod P -> Int -> Data.Mod.Word.Mod P
        go !acc 0 = acc
        go acc n = go (acc + 2 Data.Mod.Word.^% n) (n - 1)
    {-# INLINE powerNModWord #-}

#if defined(MIN_VERSION_finite_field) || defined(MIN_VERSION_modular_arithmetic)
    measure :: (Eq (t P), Num (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (powerN p) lim
    {-# INLINE measure #-}

    powerN :: forall t. (Eq (t P), Num (t P)) => Proxy t -> Int -> t P
    powerN = const $ go 0
      where
        go :: t P -> Int -> t P
        go !acc 0 = acc
        go acc n = go (acc + 2 ^ n) (n - 1)
    {-# INLINE powerN #-}
#endif

#ifdef MIN_VERSION_modular
    powerNModular :: Int -> Numeric.Modular.Mod P
    powerNModular = go 0
      where
        go :: Numeric.Modular.Mod P -> Int -> Numeric.Modular.Mod P
        go acc@(forceModular -> !_) 0 = acc
        go acc n = go (acc + 2 ^ n) (n - 1)
    {-# INLINE powerNModular #-}
#endif

main :: IO ()
main = defaultMain
  [ benchSum
  , benchProduct
  , benchInversion
  , benchPower
  ]
