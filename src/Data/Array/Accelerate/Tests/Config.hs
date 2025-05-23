{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | A small module containing CPP things.
module Data.Array.Accelerate.Tests.Config where

#ifdef ACC_TEST_NATIVE
import qualified Data.Array.Accelerate.LLVM.Native as Native
#endif
#ifdef ACC_TEST_PTX
import qualified Data.Array.Accelerate.LLVM.PTX as PTX
#endif

import Data.Array.Accelerate.Tests.Types

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


data Backend = Backend RunN

backends :: Map Target Backend
backends = Map.fromList $ concat
  []
#ifdef ACC_TEST_NATIVE
  ++ [(Native, Backend Native.runN)]
#endif
#ifdef ACC_TEST_PTX
  ++ [(PTX, Backend PTX.runN)]
#endif
