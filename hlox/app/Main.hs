module Main (main) where

import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import System.Environment qualified as Environment
import System.Exit qualified as Exit
import System.IO qualified as IO
import System.IO.Error qualified as IOError

import Scanner qualified

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [sourceFile] -> runFile sourceFile
    [] -> runPrompt
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage: hlox [script]"
  Exit.exitWith (Exit.ExitFailure 64)

runFile :: FilePath -> IO ()
runFile sourceFile = do
  str <- readFile sourceFile
  st <- run str
  Monad.when (Scanner.hadError st) $
    Exit.exitWith (Exit.ExitFailure 65)

runPrompt :: IO ()
runPrompt = hello >> go
  where
    hello :: IO ()
    hello = putStrLn "Welcome to hlox, an interpreter for the programming language lox."

    go :: IO ()
    go = do
      putStr "> " >> IO.hFlush IO.stdout
      res <- Exception.try readEvalPrint
      case res of
        Left e
          | IOError.isEOFError e -> putStrLn "Bye!"
          | otherwise -> error (show e)
        Right () -> go

    readEvalPrint :: IO ()
    readEvalPrint = do
      line <- getLine
      _ <- run line
      pure ()

run :: String -> IO Scanner.Result
run str = do
  let res = Scanner.scanTokens str
  mapM_ print $ Scanner.tokens res
  mapM_ print $ Scanner.errors res
  print res
  pure res
