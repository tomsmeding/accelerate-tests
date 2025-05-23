{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fno-full-laziness -fno-cse #-}
module Data.Array.Accelerate.Tests.Prog.NondetStencil (test) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Array.Accelerate.Tests.Types
import Data.Array.Accelerate.Tests.Util

import Control.Monad (forM_, when)
import System.IO


stencil_correct_answer :: Int -> Int -> [Int]
stencil_correct_answer len x = x + x + x+1 : [i-1 + i + i+1 | i <- [x + 1 .. x + len-2]] ++ [x+len-2 + x+len-1 + x+len-1]
-- stencil_correct_answer len x = replicate len (3*(x+2))
-- stencil_correct_answer len x = [4,6,6,4]

{-# NOINLINE runtest_stencil #-}
runtest_stencil :: RunN -> Int -> Int -> ExitEarlyT Bool IO ()
runtest_stencil runN len x = do
  let margin = 0
  let res =
        let array = A.use (A.fromList (Z :. (len + 2*margin)) [x - margin .. x + len-1 + margin :: Int])
        -- let array = A.use (A.fromList (Z :. (len + 2*margin)) (repeat (x+2)))
        in runN $ A.stencil (\(a,b,c) -> a + b + c :: A.Exp Int) A.clamp $
                     -- A.backpermute (A.I1 (A.constant len)) (\(A.I1 i) -> A.I1 (i + A.constant margin)) $
                     -- A.backpermute (A.I1 (A.constant len)) (\(A.I1 i) -> A.I1 i) $
                     -- A.generate (A.I1 (A.constant len)) (\(A.I1 i) -> array A.! A.I1 i)
                     -- A.map id $
                     array

  if A.toList res == stencil_correct_answer len x
    then return ()
    else do lift $ do putChar '\n' >> print (A.toList res)
            exitEarly False

test :: Test
test = mkTestOn "nondet-stencil" "Inter-kernel synchronisation bug" [PTX] $ \runN -> runExitEarly $ do
  let len = 4
  lift $ putStrLn $ show (stencil_correct_answer len 0) ++ " (<- correct)"
  forM_ [1..50000::Int] $ \i -> do
    runtest_stencil runN len 0
    when (i `mod` 100 == 0) $ lift $ putChar '*' >> hFlush stdout
  lift $ putChar '\n'
  return True
