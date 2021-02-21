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

type P = 1000000007

#ifdef MIN_VERSION_modular
forceModular :: Numeric.Modular.Mod P -> Numeric.Modular.Mod P
forceModular a = (a == a) `seq` a
#endif

benchSum :: Benchmark
benchSum = bgroup "Sum"
  [ measure "Data.Mod" (Proxy @Data.Mod.Mod)
  , measure "Data.Mod.Word" (Proxy @Data.Mod.Word.Mod)
#ifdef MIN_VERSION_finite_field
  , measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_finite_typelits
  , measure "finite-typelits" (Proxy @Data.Finite.Finite)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
#ifdef MIN_VERSION_modular
  , bench "modular" $ nf (show . sumNModular) lim
#endif
  ]
  where
    lim = 100000000

    measure :: (Eq (t P), Num (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (sumN p) lim
    {-# INLINE measure #-}

    sumN :: (Eq (t P), Num (t P)) => Proxy t -> Int -> t P
    sumN = const $ \n -> go 0 (fromIntegral n)
      where
        go !acc 0 = acc
        go acc n = go (acc + n) (n - 1)
    {-# INLINE sumN #-}

#ifdef MIN_VERSION_modular
    sumNModular :: Int -> Numeric.Modular.Mod P
    sumNModular = \n -> go 0 (fromIntegral n)
      where
        go acc@(forceModular -> !_) 0 = acc
        go acc n = go (acc + n) (n - 1)
    {-# INLINE sumNModular #-}
#endif

benchProduct :: Benchmark
benchProduct = bgroup "Product"
  [ measure "Data.Mod" (Proxy @Data.Mod.Mod)
  , measure "Data.Mod.Word" (Proxy @Data.Mod.Word.Mod)
#ifdef MIN_VERSION_finite_field
  , measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_finite_typelits
  , measure "finite-typelits" (Proxy @Data.Finite.Finite)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
#ifdef MIN_VERSION_modular
  , bench "modular" $ nf (show . productNModular) lim
#endif
  ]
  where
    lim = 100000000

    measure :: (Eq (t P), Num (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (productN p) lim
    {-# INLINE measure #-}

    productN :: (Eq (t P), Num (t P)) => Proxy t -> Int -> t P
    productN = const $ \n -> go 1 (fromIntegral n)
      where
        go !acc 0 = acc
        go acc n = go (acc * n) (n - 1)
    {-# INLINE productN #-}

#ifdef MIN_VERSION_modular
    productNModular :: Int -> Numeric.Modular.Mod P
    productNModular = \n -> go 1 (fromIntegral n)
      where
        go acc@(forceModular -> !_) 0 = acc
        go acc n = go (acc * n) (n - 1)
    {-# INLINE productNModular #-}
#endif

benchInversion :: Benchmark
benchInversion = bgroup "Inversion"
  [ measure "Data.Mod" (Proxy @Data.Mod.Mod)
  , measure "Data.Mod.Word" (Proxy @Data.Mod.Word.Mod)
#ifdef MIN_VERSION_finite_field
  , measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
  ]
  where
    lim = 3000000

    measure :: (Eq (t P), Fractional (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (invertN p) lim
    {-# INLINE measure #-}

    invertN :: (Eq (t P), Fractional (t P)) => Proxy t -> Int -> t P
    invertN = const $ \n -> go 0 (fromIntegral n)
      where
        go !acc 0 = acc
        go acc n = go (acc + recip n) (n - 1)
    {-# INLINE invertN #-}

benchPower :: Benchmark
benchPower = bgroup "Power"
  [ measure "Data.Mod" (Proxy @Data.Mod.Mod)
  , measure "Data.Mod.Word" (Proxy @Data.Mod.Word.Mod)
#ifdef MIN_VERSION_finite_field
  , measure "finite-field" (Proxy @Data.FiniteField.PrimeField.PrimeField)
#endif
#ifdef MIN_VERSION_finite_typelits
  , measure "finite-typelits" (Proxy @Data.Finite.Finite)
#endif
#ifdef MIN_VERSION_modular_arithmetic
  , measure "modular-arithmetic" (Proxy @(Data.Modular.Mod Integer))
#endif
#ifdef MIN_VERSION_modular
  , bench "modular" $ nf (show . powerNModular) lim
#endif
  ]
  where
    lim = 1000000

    measure :: (Eq (t P), Num (t P)) => String -> Proxy t -> Benchmark
    measure name p = bench name $ whnf (powerN p) lim
    {-# INLINE measure #-}

    powerN :: (Eq (t P), Num (t P)) => Proxy t -> Int -> t P
    powerN = const $ go 0
      where
        go !acc 0 = acc
        go acc n = go (acc + 2 ^ n) (n - 1)
    {-# INLINE powerN #-}

#ifdef MIN_VERSION_modular
    powerNModular :: Int -> Numeric.Modular.Mod P
    powerNModular = go 0
      where
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
