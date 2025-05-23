module Data.Array.Accelerate.Tests.Prog.NondetSumhang (test) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Array.Accelerate.Tests.Types


test :: Test
test = mkTestOn "nondet-sumhang" "Hangs with accelerate-llvm#97 on jizo" [PTX] $ \runN -> do
  let arr = A.fromList (Z :. 257 :. 31) [1::Int ..]
  putStrLn "running (this should complete in <1sec, otherwise it's hanging):"
  print $ length $ show $ runN (A.scanl1 (+)) arr
  putStrLn "done."
  return True
