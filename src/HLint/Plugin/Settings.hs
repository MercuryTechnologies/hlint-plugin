{-# LANGUAGE BlockArguments #-}

-- | This module is inspired by:
--
-- https://github.com/tfausak/splint/blob/9028a8b631568dc5d16a74153b1a9b6e3cde0fe6/src/lib/Splint/Settings.hs
--
-- … in order to work around this issue (just like @splint@ does):
--
-- <https://gitlab.haskell.org/ghc/ghc/issues/18261>
--
-- Without this workaround the GHC plugin will fail with something like this
-- error message:
--
-- > ghc-9.6.2(82937,0x16e83b000) malloc: *** error for object 0x600000a4d5c0: pointer being freed was not allocated
-- > ghc-9.6.2(82937,0x16e83b000) malloc: *** set a breakpoint in malloc_error_break to debug

module HLint.Plugin.Settings
    ( -- * Settings
      argsSettings
    ) where

import Control.Concurrent.MVar (MVar)
import Data.Map (Map)
import Language.Haskell.HLint (Classify, Hint, ParseFlags)

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Language.Haskell.HLint as HLint
import qualified System.IO.Unsafe as Unsafe

cache :: MVar (Map [String] (ParseFlags, [Classify], Hint))
cache = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)
{-# NOINLINE cache #-}

semaphore :: MVar ()
semaphore = Unsafe.unsafePerformIO (MVar.newMVar ())
{-# NOINLINE semaphore #-}

-- | This is a drop-in replacement for
--   "Language.Haskell.HLint".`HLint.argsSettings`, except that this is safe to
--   run in parallel.
argsSettings
    :: [String]
    -> IO (ParseFlags, [Classify], Hint)
argsSettings key =
    MVar.modifyMVar cache \m -> do
        case Map.lookup key m of
            Nothing -> do
                value <- Unsafe.unsafeInterleaveIO do
                    MVar.withMVar semaphore \_ -> HLint.argsSettings key

                pure (Map.insert key value m, value)

            Just value ->
                pure (m, value)
