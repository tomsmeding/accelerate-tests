{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fno-full-laziness -fno-cse #-}
module Data.Array.Accelerate.Tests.Prog.NondetScanlseg (test) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Array.Accelerate.Tests.Types
import Data.Array.Accelerate.Tests.Util

import Control.Monad (forM_, when)
import Data.Int
import System.IO

{-# NOINLINE runtest_scanlseg #-}
runtest_scanlseg :: RunN -> Int64 -> ExitEarlyT Bool IO ()
runtest_scanlseg runN x = do
  let toi64 = fromIntegral @_ @Int64

  -- let (singleSegLen, nsegs, nouter) = (20, 30, 32)
  -- let (singleSegLen, nsegs, nouter) = (32, 512, 96)
  let (singleSegLen, nsegs, nouter) = (16, 10, 10)
  -- let (singleSegLen, nsegs, nouter) = (5, 3, 1)
  let ninner = singleSegLen * nsegs
      seg = A.fromList (Z :. nsegs) (replicate nsegs singleSegLen)
      arr = A.fromFunction (Z :. nouter :. ninner) $ \(Z :. _ :. i) ->
                toi64 (i `mod` singleSegLen + 1)

  let expected1 = A.fromFunction (Z :. nouter :. ninner) $ \(Z :. _ :. i) ->
                      let j = i `mod` singleSegLen
                      in toi64 (j * (j + 1) `div` 2)
      expected2 = A.fromFunction (Z :. nouter :. nsegs) $ \(Z :. _ :. _) ->
                      toi64 (singleSegLen * (singleSegLen + 1) `div` 2)

  let result = runN (\v -> A.scanl'Seg (+) (A.the v)) (A.fromList A.Z [x]) arr seg
  -- print result
  let correct1 = fst result == expected1
  let correct2 = snd result == expected2
  if correct1 && correct2
    then lift $ putChar '*' >> hFlush stdout
    else do lift $ putStrLn $ "\n" ++ show (correct1, correct2)
            exitEarly False

test :: Test
test = mkTestOn "nondet-scanlseg" [PTX] $ \runN -> runExitEarly $ do
  forM_ [1..500::Int] $ \i -> do
    runtest_scanlseg runN 0
    when (i `mod` 100 == 0) $ lift $ putChar '\n'
  return True
