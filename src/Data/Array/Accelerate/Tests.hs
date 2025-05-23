module Data.Array.Accelerate.Tests where

import Data.Array.Accelerate.Tests.Types

import qualified Data.Array.Accelerate.Tests.Prog.ADBenchGMMGrad as ADBenchGMMGrad
import qualified Data.Array.Accelerate.Tests.Prog.NondetScanlseg as NondetScanlseg
import qualified Data.Array.Accelerate.Tests.Prog.NondetStencil as NondetStencil
import qualified Data.Array.Accelerate.Tests.Prog.NondetSumhang as NondetSumhang


tests :: [Test]
tests =
  [ADBenchGMMGrad.test
  ,NondetScanlseg.test
  ,NondetStencil.test
  ,NondetSumhang.test
  ]
