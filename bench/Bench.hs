{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-name-shadowing #-}

module Main where

import Data.Maybe
import Data.Time.Clock
import System.IO

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

import Text.Printf

normalize :: NominalDiffTime -> NominalDiffTime -> String
normalize unit t = printf "%.2fx" (fromRational (toRational t / toRational unit) :: Double)

benchAddition :: IO ()
benchAddition = do
  putStrLn "Sum"

  t0 <- getCurrentTime
  print (sum [1..10^8] :: Data.Mod.Mod 1000000007)
  t1 <- getCurrentTime
  let unit = diffUTCTime t1 t0

  t0 <- getCurrentTime
  print (sum [1..10^8] :: Data.Mod.Word.Mod 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "Data.Mod.Word      " ++ normalize unit (diffUTCTime t1 t0)

  putStrLn $ "Data.Mod           1x"

#ifdef MIN_VERSION_finite_field
  t0 <- getCurrentTime
  print (sum [1..10^8] :: Data.FiniteField.PrimeField.PrimeField 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "finite-field       " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_finite_typelits
  t0 <- getCurrentTime
  print (sum [1..10^8] :: Data.Finite.Finite 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "finite-typelits    " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_modular_arithmetic
  t0 <- getCurrentTime
  print (sum [1..10^8] :: Data.Modular.Mod Integer 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "modular-arithmetic " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_modular
  t0 <- getCurrentTime
  print (sum (map fromIntegral [1..10^8]) :: Numeric.Modular.Mod 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "modular            " ++ normalize unit (diffUTCTime t1 t0)
#endif

benchProduct :: IO ()
benchProduct = do
  putStrLn "Product"

  t0 <- getCurrentTime
  print (product [1..10^8] :: Data.Mod.Mod 1000000007)
  t1 <- getCurrentTime
  let unit = diffUTCTime t1 t0

  t0 <- getCurrentTime
  print (product [1..10^8] :: Data.Mod.Word.Mod 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "Data.Mod.Word      " ++ normalize unit (diffUTCTime t1 t0)

  putStrLn $ "Data.Mod           1x"

#ifdef MIN_VERSION_finite_field
  t0 <- getCurrentTime
  print (product [1..10^8] :: Data.FiniteField.PrimeField.PrimeField 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "finite-field       " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_finite_typelits
  t0 <- getCurrentTime
  print (product [1..10^8] :: Data.Finite.Finite 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "finite-typelits    " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_modular_arithmetic
  t0 <- getCurrentTime
  print (product [1..10^8] :: Data.Modular.Mod Integer 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "modular-arithmetic " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_modular
  t0 <- getCurrentTime
  print (product (map fromIntegral [1..10^8]) :: Numeric.Modular.Mod 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "modular            " ++ normalize unit (diffUTCTime t1 t0)
#endif

benchInversion :: IO ()
benchInversion = do
  putStrLn "Inversion"

  t0 <- getCurrentTime
  print (sum (map (fromJust . Data.Mod.invertMod) [1..10^7]) :: Data.Mod.Mod 1000000007)
  t1 <- getCurrentTime
  let unit = diffUTCTime t1 t0

  t0 <- getCurrentTime
  print (sum (map (fromJust . Data.Mod.Word.invertMod) [1..10^7]) :: Data.Mod.Word.Mod 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "Data.Mod.Word      " ++ normalize unit (diffUTCTime t1 t0)

  putStrLn $ "Data.Mod           1x"

#ifdef MIN_VERSION_finite_field
  t0 <- getCurrentTime
  print (sum (map recip [1..10^7]) :: Data.FiniteField.PrimeField.PrimeField 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "finite-field       " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_modular_arithmetic
  t0 <- getCurrentTime
  print (sum (map Data.Modular.inv [1..10^7]) :: Data.Modular.Mod Integer 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "modular-arithmetic " ++ normalize unit (diffUTCTime t1 t0)
#endif

benchPower :: IO ()
benchPower = do
  putStrLn "Power"

  t0 <- getCurrentTime
  print (sum (map (2 ^) [1..10^6]) :: Data.Mod.Mod 1000000007)
  t1 <- getCurrentTime
  let unit = diffUTCTime t1 t0

  t0 <- getCurrentTime
  print (sum (map (2 ^) [1..10^6]) :: Data.Mod.Word.Mod 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "Data.Mod.Word      " ++ normalize unit (diffUTCTime t1 t0)

  putStrLn $ "Data.Mod           1x"

#ifdef MIN_VERSION_finite_field
  t0 <- getCurrentTime
  print (sum (map (2 ^) [1..10^6]) :: Data.FiniteField.PrimeField.PrimeField 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "finite-field       " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_finite_typelits
  t0 <- getCurrentTime
  print (sum (map (2 ^) [1..10^6]) :: Data.Finite.Finite 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "finite-typelits    " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_modular_arithmetic
  t0 <- getCurrentTime
  print (sum (map (2 ^) [1..10^6]) :: Data.Modular.Mod Integer 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "modular-arithmetic " ++ normalize unit (diffUTCTime t1 t0)
#endif

#ifdef MIN_VERSION_modular
  t0 <- getCurrentTime
  print (sum (map (2 ^) [1..10^6]) :: Numeric.Modular.Mod 1000000007)
  t1 <- getCurrentTime
  putStrLn $ "modular            " ++ normalize unit (diffUTCTime t1 t0)
#endif

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  benchAddition
  putStrLn ""
  benchProduct
  putStrLn ""
  benchInversion
  putStrLn ""
  benchPower
