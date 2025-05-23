{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fno-full-laziness -fno-cse #-}
module Data.Array.Accelerate.Tests.Prog.ADBenchGMMGrad where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.Sugar.Shape as Sugar
import Data.Array.Accelerate.Tests.Prog.ADBenchGMMGrad.Acc
import Data.Array.Accelerate.Tests.Types
import Data.Array.Accelerate.Tests.Util

import Control.Monad (forM_, when)
import Data.Bits
import System.IO


-- A really stupid bad hash, but it's good enough
hash :: Word -> Word
hash n =
  let a = (n * 0x897621678432180) `xor` n + 17
  in a `shiftR` 2 + (a `shiftL` 7) `xor` (a `shiftL` 56)

genFloat :: Word -> Float
genFloat n = fromIntegral n / fromIntegral ((maxBound `quot` 2) :: Word) - 1

genArray :: A.Shape sh => Int -> sh -> A.Array sh Float
genArray salt sh = A.fromFunction sh (\idx -> genFloat (fromIntegral (salt + Sugar.toIndex sh idx)))

class CmpSimilar a where
  maxDiff :: a -> a -> Float

instance CmpSimilar Float where
  maxDiff x y = abs (x - y)

instance (A.Shape sh, Eq sh) => CmpSimilar (A.Array sh Float) where
  maxDiff arr1 arr2
    | A.arrayShape arr1 == A.arrayShape arr2 =
        let sz = A.arraySize arr1
        in maximum [A.linearIndexArray arr1 i `maxDiff` A.linearIndexArray arr2 i | i <- [0 .. sz-1]]
    | otherwise =
        error "maxDiff: arrays have unequal shape"

instance (CmpSimilar a, CmpSimilar b, CmpSimilar c) => CmpSimilar (a, b, c) where
  maxDiff (a,b,c) (x,y,z) = maxDiff a x `max` maxDiff b y `max` maxDiff c z

data Size = Small | Medium | Manual Int Int Int  -- d, k, n
  deriving (Show)

genInput :: Int -> Size -> GMMIn
genInput salt size =
  let (d, k, n) = case size of
                    Small -> (2, 5, 1000)
                    Medium -> (32, 25, 1000)
                    Manual d' k' n' -> (d', k', n')
      alphas = genArray salt (A.Z A.:. k)
      means = genArray salt (A.Z A.:. k A.:. d)
      icf = genArray salt (A.Z A.:. k A.:. (d + d * (d - 1) `quot` 2))
      x = genArray salt (A.Z A.:. n A.:. d)
      gamma = A.fromList A.Z [1]
      m = A.fromList A.Z [0]
  in (alphas, means, icf, x, gamma, m)

-- The extra Int argument is to make sure each invocation is different. The
-- no-full-laziness etc. options are to ensure that the body isn't lifted out.
{-# NOINLINE runtest #-}
runtest :: Int -> RunN -> GMMIn -> GMMOut
runtest _ runN input = runN gradientFunction input

test :: Test
test = mkTest "adbench-gmmgrad" "Suboptimal, large gradient of ADBench GMM task" $ \runN -> runExitEarly $ do
  -- Warning: 'Medium' takes a VERY long time in the interpreter.
  let !input = genInput 42 Small
  let !ref = I.runN gradientFunction input
  forM_ [0..100] $ \i -> do
    -- lift $ putChar '*' >> hFlush stdout
    let !out = runtest i runN input
    let diff = maxDiff ref out
    lift $ print diff
    when (diff > 2e-3) $ do
      lift $ putStrLn $ "\nResults unequal: max difference " ++ show diff
      exitEarly False
  lift $ putChar '\n'
  return True
