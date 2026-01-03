module Scanner
  ( scanTokens,
    Result,
    errors,
    tokens,
    hadError,
    Error,
    eLine,
    eWhere,
    eMessage,
    Token,
    tType,
    tLine,
    TokenType (..),
    tokenToString,
  )
where

import Control.Monad.State.Lazy (State)
import Control.Monad.State.Lazy qualified as State
import Data.Char qualified as Char
import Data.Kind (Type)
import Data.List qualified as List
import Prelude hiding (error)

type TokenType :: Type
data TokenType
  = -- Single-character tokens.
    LeftParen
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
  | -- One or two character tokens.
    Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | -- Literals.
    Identifier String
  | TString String
  | Number Double
  | -- Keywords.
    And
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
data Token
  = MkToken
  { tType :: TokenType,
    tLine :: Int
  }
  deriving stock (Show)

tokenToString :: Token -> String
tokenToString = show

-- show (tType t) ++ " " ++ lexeme t ++ " " ++ show (literal t)

type Error :: Type
data Error
  = MkError
  { eLine :: Int,
    eWhere :: String,
    eMessage :: String
  }
  deriving stock (Show)

type Scanner :: Type
data Scanner
  = MkScanner
  { sLine :: Int,
    sTokens :: [Token],
    sErrors :: [Error]
  }

initScanner :: Scanner
initScanner =
  MkScanner
    { sLine = 1,
      sTokens = [],
      sErrors = []
    }

type Result :: Type
data Result
  = MkResult
  { tokens :: [Token],
    errors :: [Error]
  }
  deriving stock (Show)

toResult :: Scanner -> Result
toResult s =
  MkResult
  { tokens = sTokens s,
    errors = sErrors s
  }

hadError :: Result -> Bool
hadError = not . null . errors

-- | The main function of the Scanner module. Scans text into Tokens
-- while also accumulating all Errors.
--
-- Note that this is a streaming scanner, meaning that it will only
-- scan as far into the text as it needs to produce the tokens you
-- consume from it. In order to fully scan a text, the callee must
-- fully evaluate all tokens. This also means you can stop at the
-- first error if you want, simply by checking for a first error and
-- branching off to do something else if found.
scanTokens :: String -> Result
scanTokens = toResult . flip State.execState initScanner . go
  where
    go :: String -> State Scanner ()
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
          State.modify $ \st -> st {sLine = sLine st + 1}
          go r
        '"' : r0 -> do
          let (tstr, r1) = List.break (== '"') r0
          case r1 of
            '"' : r2 -> addToken (TString tstr) >> go r2
            _ -> do
              error "String not terminated by \""
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
          error $ "Unexpected character " ++ show c
          go r

addToken :: TokenType -> State Scanner ()
addToken tt = State.modify $ \st ->
  st {sTokens = sTokens st ++ [MkToken tt (sLine st)]}

error :: String -> State Scanner ()
error message = do
  line <- State.gets sLine
  report $ MkError line "" message

report :: Error -> State Scanner ()
report err = State.modify $ \st -> st {sErrors = sErrors st ++ [err]}

eatInlineComment :: String -> String
eatInlineComment = dropWhile (/= '\n')

eatBlockComment :: Int -> String -> String
eatBlockComment = go
  where
    go :: Int -> String -> String
    go 0 cs = cs
    go n ('*' : '/' : cs) = go (n - 1) cs
    go n ('/' : '*' : cs) = go (n + 1) cs
    go n (_ : cs) = go n cs
    go _ [] = []

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
