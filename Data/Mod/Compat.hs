-- | See https://gitlab.haskell.org/ghc/ghc/-/issues/22933
--   and https://gitlab.haskell.org/ghc/ghc/-/issues/22966
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Mod.Compat
  ( timesWord2#
  , remWord2#
  ) where

#if defined(aarch64_HOST_ARCH) && __GLASGOW_HASKELL__ < 912

import GHC.Exts (Word(..), Word#)

#if __GLASGOW_HASKELL__ < 904

import GHC.Exts (timesWord#)

timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
timesWord2# x y = (# z, timesWord# x y #)
  where
    !(W# z) = c_umulh (W# x) (W# y)
{-# INLINE timesWord2# #-}

foreign import capi unsafe "aarch64.h umulh" c_umulh :: Word -> Word -> Word

#else

import GHC.Exts (timesWord2#)

#endif

remWord2# :: Word# -> Word# -> Word# -> Word#
remWord2# lo hi m = r
  where
    !(W# r) = c_umodh (W# lo) (W# hi) (W# m)
{-# INLINE remWord2# #-}

foreign import capi unsafe "aarch64.h umodh" c_umodh :: Word -> Word -> Word -> Word

#else

import GHC.Exts (Word#, timesWord2#, quotRemWord2#)

remWord2# :: Word# -> Word# -> Word# -> Word#
remWord2# lo hi m = r
  where
    !(# _, r #) = quotRemWord2# hi lo m
{-# INLINE remWord2# #-}

#endif
