module Main (main) where

import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import System.Environment qualified as Environment
import System.Exit qualified as Exit
import System.IO qualified as IO
import System.IO.Error qualified as IOError

import Scanner qualified
import Parser qualified

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
  let sRes = Scanner.scanTokens str
      pRes = Parser.parse (Scanner.tokens sRes)
  putStrLn "TOKENS:"
  mapM_ print $ Scanner.tokens sRes
  putStrLn "SCANNER ERRORS:"
  mapM_ print $ Scanner.errors sRes
  putStrLn "AST:"
  mapM_ print $ Parser.ast pRes
  putStrLn "PARSER ERRORS:"
  mapM_ print $ Parser.errors pRes
  pure sRes
