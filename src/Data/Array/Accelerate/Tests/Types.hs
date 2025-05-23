{-# LANGUAGE RankNTypes #-}
module Data.Array.Accelerate.Tests.Types where

import qualified Data.Array.Accelerate.Trafo.Sharing as A

import qualified Data.Set as Set
import Data.Set (Set)


type RunN = forall f. A.Afunction f => f -> A.AfunctionR f

data Target = Native | PTX
  deriving (Show, Eq, Ord)

parseTarget :: String -> Maybe Target
parseTarget "native" = Just Native
parseTarget "ptx" = Just PTX
parseTarget _ = Nothing

-- | This ought to be the inverse of 'parseTarget'.
unparseTarget :: Target -> String
unparseTarget Native = "native"
unparseTarget PTX = "ptx"

-- | A test-like thing. If 'testTargets' is 'Nothing', this test is to be run
-- on all targets. The function should return whether the test was successful.
data Test = Test
  { testName :: String
  , testDescription :: String
  , testTargets :: Maybe (Set Target)
  , testFunction :: RunN -> IO Bool }

-- | A test that runs on all targets.
mkTest :: String -> String -> (RunN -> IO Bool) -> Test
mkTest name descr f = Test name descr Nothing f

-- | A test that runs on some targets only.
mkTestOn :: String -> String -> [Target] -> (RunN -> IO Bool) -> Test
mkTestOn name descr targets f = Test name descr (Just (Set.fromList targets)) f
