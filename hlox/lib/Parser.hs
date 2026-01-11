module Parser
  ( parse
  , Result
  , ast
  , errors
  )
where

import Control.Monad.State.Lazy (State)
import Control.Monad.State.Lazy qualified as State
import Data.Kind (Type)

import Scanner ( Token )
import Scanner qualified

type EqOp :: Type
data EqOp = NotEqual | Equal
  deriving stock (Eq, Show)

type CmpOp :: Type
data CmpOp = Greater | GreaterEqual | Less | LessEqual
  deriving stock (Eq, Show)

type TermOp :: Type
data TermOp = Minus | Plus
  deriving stock (Eq, Show)

type FactOp :: Type
data FactOp = Div | Mul
  deriving stock (Eq, Show)

type UnOp :: Type
data UnOp = Not | UnaryMinus
  deriving stock (Eq, Show)

type Prim :: Type
data Prim
  = PNumber Double
  | PString String
  | PTrue
  | PFalse
  | Nil
  | Paren Expr
  deriving stock (Eq, Show)

type Expr :: Type
data Expr
  = Equality EqOp Expr Expr
  | Comparison CmpOp Expr Expr
  | Term TermOp Expr Expr
  | Factor FactOp Expr Expr
  | Unary UnOp Expr
  | Primary Prim
  deriving stock (Eq, Show)

type Error :: Type
data Error
  = MkError
  { eLine :: Int,
    eWhere :: String,
    eMessage :: String
  }
  deriving stock (Show)

type Parser :: Type
data Parser
  = MkParser
  { pExprs :: [Expr],
    pErrors :: [Error]
  }

initParser :: Parser
initParser =
  MkParser
    { pExprs = [],
      pErrors = []
    }

type Result :: Type
data Result
  = MkResult
  { ast :: [Expr],
    errors :: [Error]
  }
  deriving stock (Show)

toResult :: Parser -> Result
toResult s =
  MkResult
  { ast = pExprs s,
    errors = pErrors s
  }

parse :: [Token] -> Result
parse = toResult . flip State.execState initParser . go
  where
    go :: [Token] -> State Parser ()
    go = maybe (pure ()) (\(e, _ts) -> State.modify $ \st -> st { pExprs = pure e }) . expression

expression :: [Token] -> Maybe (Expr, [Token])
expression = equality

equality :: [Token] -> Maybe (Expr, [Token])
equality ts0 = do
  (lhs, opToken : ts1) <- comparison ts0
  let yield op = do
        (rhs, ts2) <- equality ts1
        pure (Equality op lhs rhs, ts2)
  case Scanner.tType opToken of
    Scanner.BangEqual -> yield NotEqual
    Scanner.EqualEqual -> yield Equal
    _ -> pure (lhs, opToken : ts1)

comparison :: [Token] -> Maybe (Expr, [Token])
comparison ts0 = do
  (lhs, opToken : ts1) <- term ts0
  let yield op = do
        (rhs, ts2) <- comparison ts1
        pure (Comparison op lhs rhs, ts2)
  case Scanner.tType opToken of
    Scanner.Greater -> yield Greater
    Scanner.GreaterEqual -> yield GreaterEqual
    Scanner.Less -> yield Less
    Scanner.LessEqual -> yield LessEqual
    _ -> pure (lhs, opToken : ts1)

term :: [Token] -> Maybe (Expr, [Token])
term ts0 = do
  (lhs, opToken : ts1) <- factor ts0
  let yield op = do
        (rhs, ts2) <- term ts1
        pure (Term op lhs rhs, ts2)
  case Scanner.tType opToken of
    Scanner.Minus -> yield Minus
    Scanner.Plus -> yield Plus
    _ -> pure (lhs, opToken : ts1)

factor :: [Token] -> Maybe (Expr, [Token])
factor ts0 = do
  (lhs, opToken : ts1) <- unary ts0
  let yield op = do
        (rhs, ts2) <- factor ts1
        pure (Factor op lhs rhs, ts2)
  case Scanner.tType opToken of
    Scanner.Slash -> yield Div
    Scanner.Star -> yield Mul
    _ -> pure (lhs, opToken : ts1)

unary :: [Token] -> Maybe (Expr, [Token])
unary ts0@(opToken : ts1) = do
  let yield op = do
        (expr, ts2) <- unary ts1
        pure (Unary op expr, ts2)
  case Scanner.tType opToken of
    Scanner.Bang -> yield Not
    Scanner.Minus -> yield UnaryMinus
    _ -> primary ts0
unary [] = Nothing

primary :: [Token] -> Maybe (Expr, [Token])
primary (t : ts0) = do
  case Scanner.tType t of
    Scanner.Number x -> pure (Primary (PNumber x), ts0)
    Scanner.TString str -> pure (Primary (PString str), ts0)
    Scanner.TTrue -> pure (Primary PTrue, ts0)
    Scanner.TFalse -> pure (Primary PFalse, ts0)
    Scanner.LeftParen -> do
      (expr, ptok : ts1) <- expression ts0
      case Scanner.tType ptok of
        Scanner.RightParen -> do
          pure (Primary (Paren expr), ts1)
        _ -> Nothing
    _ -> Nothing
primary [] = Nothing
