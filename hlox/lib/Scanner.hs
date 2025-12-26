module Scanner
  ( St
  , hadError
  , tokens
  , scanTokens
  , errors
  , Error
  , errorLine
  , errorWhere
  , errorMessage
  ) where

import Prelude hiding ( error )
import Control.Monad.State.Lazy qualified as State
import Control.Monad.State.Lazy ( State )
import Data.List qualified as List
import System.IO qualified as IO
import Data.Kind ( Type )
import Data.Char qualified as Char

type TokenType :: Type
data TokenType
  -- Single-character tokens.
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  -- One or two character tokens.
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  -- Literals.
  | Identifier String
  | TString String
  | Number Double
  -- Keywords.
  | And
  | Class
  | Else
  | TFalse
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | TTrue
  | Var
  | While

  | Eof
  deriving stock (Eq, Read, Show)

type Token :: Type
data Token =
  MkToken
  { ttype :: TokenType
  , line :: Int
  }
  deriving stock (Show)

tokenToString :: Token -> String
tokenToString = show
  -- show (ttype t) ++ " " ++ lexeme t ++ " " ++ show (literal t)

type Error :: Type
data Error =
  MkError
  { errorLine :: Int
  , errorWhere :: String
  , errorMessage :: String
  }
  deriving stock (Show)

type St :: Type
data St =
  MkSt
  { scannerLine :: Int
  , tokens :: [Token]
  , errors :: [Error]
  }

initSt :: St
initSt =
  MkSt
  { scannerLine = 1
  , tokens = []
  , errors = []
  }

scanTokens :: String -> St
scanTokens = flip State.execState initSt . go
  where
    addToken :: TokenType -> State St ()
    addToken tt = State.modify $ \st ->
      st { tokens = tokens st ++ [ MkToken tt (scannerLine st) ] }

    reverseTokens :: St -> St
    reverseTokens st = st { tokens = reverse (tokens st) }

    go :: String -> State St ()
    go str =
      case str of
        [] -> addToken Eof
        '(' : r -> addToken LeftParen >> go r
        ')' : r -> addToken RightParen >> go r
        '{' : r -> addToken LeftBrace >> go r
        '}' : r -> addToken RightBrace >> go r
        ',' : r -> addToken Comma >> go r
        '.' : r -> addToken Dot >> go r
        '-' : r -> addToken Minus >> go r
        '+' : r -> addToken Plus >> go r
        ';' : r -> addToken Semicolon >> go r
        '*' : r -> addToken Star >> go r
        '!' : '=' : r -> addToken BangEqual >> go r
        '!' : r -> addToken Bang >> go r
        '=' : '=' : r -> addToken EqualEqual >> go r
        '=' : r -> addToken Equal >> go r
        '<' : '=' : r -> addToken LessEqual >> go r
        '<' : r -> addToken Less >> go r
        '>' : '=' : r -> addToken GreaterEqual >> go r
        '>' : r -> addToken Greater >> go r
        '/' : '/' : r -> go (eatInlineComment r)
        '/' : '*' : r -> go (eatBlockComment 1 r)
        '/' : r -> addToken Slash >> go r
        ' ' : r -> go r
        '\r' : r -> go r
        '\t' : r -> go r

        '\n' : r -> do
          State.modify $ \st -> st { scannerLine = scannerLine st + 1 }
          go r

        '"' : r0 -> do
          let (tstr, r1) = List.break (== '"') r0
          case r1 of
            '"' : r2 -> addToken (TString tstr) >> go r2
            _ -> do
              st <- State.get
              error (scannerLine st) $ "String not terminated by \""
              go r1

        cs@(c : _)
          | isDigit c -> do
            let (istr, r1) = List.span isDigit cs
                (rstr, rs) =
                  case r1 of
                    '.' : r2 ->
                      let (fstr, r3) = List.span isDigit r2
                      in ('.' : fstr, r3)
                    r2 -> ("", r2)
                dstr = istr ++ rstr
            addToken (Number (read dstr)) >> go rs

        'a' : 'n' : 'd' : cs | isKeywordSep cs -> addToken And >> go cs
        'c' : 'l' : 'a' : 's' : 's' : cs | isKeywordSep cs -> addToken Class >> go cs
        'e' : 'l' : 's' : 'e' : cs | isKeywordSep cs -> addToken Else >> go cs
        'f' : 'a' : 'l' : 's' : 'e' : cs | isKeywordSep cs -> addToken TFalse >> go cs
        'f' : 'o' : 'r' : cs | isKeywordSep cs -> addToken For >> go cs
        'f' : 'u' : 'n' : cs | isKeywordSep cs -> addToken Fun >> go cs
        'i' : 'f' : cs | isKeywordSep cs -> addToken If >> go cs
        'n' : 'i' : 'l' : cs | isKeywordSep cs -> addToken Nil >> go cs
        'o' : 'r' : cs | isKeywordSep cs -> addToken Or >> go cs
        'p' : 'r' : 'i' : 'n' : 't' : cs | isKeywordSep cs -> addToken Print >> go cs
        'r' : 'e' : 't' : 'u' : 'r' : 'n' : cs | isKeywordSep cs -> addToken Return >> go cs
        's' : 'u' : 'p' : 'e' : 'r' : cs | isKeywordSep cs -> addToken Super >> go cs
        't' : 'h' : 'i' : 's' : cs | isKeywordSep cs -> addToken This >> go cs
        't' : 'r' : 'u' : 'e' : cs | isKeywordSep cs -> addToken TTrue >> go cs
        'v' : 'a' : 'r' : cs | isKeywordSep cs -> addToken Var >> go cs
        'w' : 'h' : 'i' : 'l' : 'e' : cs | isKeywordSep cs -> addToken While >> go cs

        cs@(c : _)
          | isAlpha c -> do
            let (istr, r1) = List.span isAlphaNumeric cs
            addToken (Identifier istr) >> go r1

        c : r -> do
          st <- State.get
          error (scannerLine st) $ "Unexpected character " ++ show c
          go r

    eatInlineComment :: String -> String
    eatInlineComment = dropWhile (/= '\n')

    eatBlockComment :: Int -> String -> String
    eatBlockComment = bgo
      where
        bgo :: Int -> String -> String
        bgo 0 cs = cs
        bgo n ('*' : '/' : cs) = bgo (n - 1) cs
        bgo n ('/' : '*' : cs) = bgo (n + 1) cs
        bgo n (_ : cs) = bgo n cs
        bgo _ [] = []

    isKeywordSep :: String -> Bool
    isKeywordSep = \case
      c : _ | Char.isSpace c -> True
      [] -> True
      _ -> False

    isDigit :: Char -> Bool
    isDigit c = '0' <= c && c <= '9'

    isAlpha :: Char -> Bool
    isAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'

    isAlphaNumeric :: Char -> Bool
    isAlphaNumeric c = isDigit c || isAlpha c

error :: Int -> String -> State St ()
error line message =
  report line "" message

report :: Int -> String -> String -> State St ()
report line where' message = do
  let err = MkError line where' message
  State.modify $ \st -> st { errors = errors st ++ [ err ] }

hadError :: St -> Bool
hadError = not . null . errors
