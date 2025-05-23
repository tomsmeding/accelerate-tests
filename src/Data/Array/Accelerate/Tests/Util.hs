module Data.Array.Accelerate.Tests.Util (
  module Data.Array.Accelerate.Tests.Util,
  lift
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont


type ExitEarlyT = ContT

runExitEarly :: Monad m => ExitEarlyT r m r -> m r
runExitEarly = evalContT

exitEarly :: Monad m => r -> ExitEarlyT r m a
exitEarly x = ContT (\_ -> return x)
