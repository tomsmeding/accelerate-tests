{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Data.Array.Accelerate.Tests as Tests
import qualified Data.Array.Accelerate.Tests.Config as Config
import Data.Array.Accelerate.Tests.Types

import Control.Monad
import Data.Either (partitionEithers)
import qualified Data.Foldable as Foldable
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.IsList
import System.Environment
import System.Exit
import System.IO


data SnocList a = Lin | SnocList a :< a
  deriving (Show, Foldable)

instance Semigroup (SnocList a) where
  l <> Lin = l
  l <> (l' :< x) = (l <> l') :< x

instance Monoid (SnocList a) where
  mempty = Lin

instance IsList (SnocList a) where
  type Item (SnocList a) = a
  fromList = foldl' (:<) Lin
  toList = Foldable.toList

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep l
  | (pre, _ : post) <- span (/= sep) l = pre : splitOn sep post
  | otherwise                          = [l]


data Options = Options
  { optsHelp :: Bool
  , optsList :: Bool
  , optsListTargets :: Bool
  , optsContinue :: Bool
  , optsTests :: SnocList String
  , optsTargets :: SnocList Target
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options False False False False mempty mempty

showHelp :: IO ()
showHelp = putStrLn
  "Usage: accelerate-tests [OPTIONS]\n\
  \\n\
  \By default, all tests are run on all backends they are meant for. If both --test\n\
  \and --using are passed, we assume you know what you're doing and run the given\n\
  \tests on the given targets regardless of what the test's metadata says.\n\
  \\n\
  \  -h/--help       Show this help.\n\
  \  -l/--list       List known tests.\n\
  \  --list-targets  List known targets.\n\
  \  -c/--continue   Continue running tests even if one failed.\n\
  \  -t/--test NAME  Run only the given test(s). May be given multiple times.\n\
  \                  Additionally, multiple test names may be given per '--test'\n\
  \                  flag by separating the names with commas. Tests will only be\n\
  \                  run on the target they are meant for, if the test specifies\n\
  \                  any, unless --using is also passed explicitly.\n\
  \  -u/--using TGT  Run only on the given target(s). May be given multiple times,\n\
  \                  and multiple targets may be given per '--using' by separating\n\
  \                  with commas. Only tests meant for this target will be run,\n\
  \                  unless --test is also passed explicitly."

parseOptions :: [String] -> Options -> Either String Options
parseOptions [] opts = Right opts
parseOptions (arg : rest) opts
  | arg `elem` ["-h", "--help"] = parseOptions rest opts { optsHelp = True }
  | arg `elem` ["-l", "--list"] = parseOptions rest opts { optsList = True }
  | arg `elem` ["--list-targets"] = parseOptions rest opts { optsListTargets = True }
  | arg `elem` ["-c", "--continue"] = parseOptions rest opts { optsContinue = True }
  | arg `elem` ["-t", "--test"] = case rest of
      str : rest' ->
        parseOptions rest' opts { optsTests = optsTests opts <> fromList (splitOn ',' str) }
      _ -> Left $ "Expected argument after '" ++ arg ++ "'"
  | arg `elem` ["-u", "--using"] = case rest of
      str : rest'
        | Just tgts <- traverse parseTarget (splitOn ',' str) ->
            parseOptions rest' opts { optsTargets = optsTargets opts <> fromList tgts }
        | otherwise ->
            Left $ "Could not parse targets: '" ++ str ++ "'"
      _ -> Left $ "Expected argument after '" ++ arg ++ "'"
  | otherwise =
      Left $ "Unrecognised argument: '" ++ arg ++ "'"


main :: IO ()
main = do
  opts <- getArgs >>= \args -> case parseOptions args defaultOptions of
            Right opts -> return opts
            Left err -> die (err ++ "\nPass '--help' for help.")

  when (optsHelp opts) $ showHelp >> exitSuccess

  when (optsList opts) $ do
    forM_ Tests.tests $ \test ->
      putStrLn $ testName test ++
                 (case testTargets test of
                    Nothing -> ""
                    Just tgts -> "  for: " ++ intercalate ", " (map unparseTarget (Foldable.toList tgts)))
    exitSuccess

  when (optsListTargets opts) $ do
    forM_ (Map.keys Config.backends) $ \tgt ->
      putStrLn $ unparseTarget tgt
    exitSuccess

  (tests, explicitTestList) <-
    let testsMap = Map.fromList [(testName test, test) | test <- Tests.tests]
        results = [maybe (Left name) Right (Map.lookup name testsMap)
                  | name <- Foldable.toList (optsTests opts)]
    in case partitionEithers results of
         ([], []) -> return (Tests.tests, False)
         ([], tests) -> return (tests, True)
         (name:_, _) -> die $ "Test not found: " ++ name

  (backends, explicitBackendList) <-
    let results = [maybe (Left tgt) (\b -> Right (tgt, b)) (Map.lookup tgt Config.backends)
                  | tgt <- Foldable.toList (optsTargets opts)]
    in case partitionEithers results of
         ([], []) -> return (Map.assocs Config.backends, False)
         ([], backends) -> return (backends, True)
         (tgt:_, _) -> die $ "Target not compiled: " ++ unparseTarget tgt ++
                             " (perhaps the appropriate cabal flag needs to be set)"

  let decideBackends :: Maybe (Set Target) -> [(Target, Config.Backend)]
      decideBackends spec
        | explicitTestList, explicitBackendList =
            backends
        | otherwise =
            case spec of
              Nothing -> backends
              Just intent -> filter ((`Set.member` intent) . fst) backends

  let multipleTests = case tests of
                        _:_:_ -> True
                        _ -> False

  istty <- hIsTerminalDevice stdout
  let fmtMsg s | istty = "\x1B[1m" ++ s ++ "\x1B[0m"
               | otherwise = s
      fmtErr s | istty = "\x1B[1;31m" ++ s ++ "\x1B[0m"
               | otherwise = s

  allok <-
    fmap and . forM tests $ \test ->
      fmap and . forM (decideBackends (testTargets test)) $ \(tgt, Config.Backend runN) -> do
        when multipleTests $
          putStrLn $ fmtMsg $ "# " ++ testName test ++ " on " ++ unparseTarget tgt
        ok <- testFunction test runN
        when (not ok) $ do
          putStrLn $ fmtErr $ "## Failed test " ++ testName test
          when (not (optsContinue opts)) exitFailure
        return ok

  when (not allok) exitFailure
