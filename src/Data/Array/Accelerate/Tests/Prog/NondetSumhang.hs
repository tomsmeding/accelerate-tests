module Data.Array.Accelerate.Tests.Prog.NondetSumhang (test) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Array.Accelerate.Tests.Types


test :: Test
test = mkTestOn "nondet-sumhang" [PTX] $ \runN -> do
  let arr = A.fromList (Z :. 257 :. 31) [1::Int ..]
  putStrLn "running:"
  print $ length $ show $ runN (A.scanl1 (+)) arr
  putStrLn "done."
  return True
