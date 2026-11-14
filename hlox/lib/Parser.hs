module Parser
  ( parse
  , Result
  , hadError
  , ast
  , errors
  )
where

import Control.Applicative (empty)
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
  | Grouping Expr
  deriving stock (Eq, Show)

type Expr :: Type
data Expr
  = Equality Expr EqOp Expr
  | Comparison Expr CmpOp Expr
  | Term Expr TermOp Expr
  | Factor Expr FactOp Expr
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

hadError :: Result -> Bool
hadError = not . null . errors

parse :: [Token] -> Result
parse = toResult . flip State.execState initParser . go
  where
    addExpr :: State.MonadState Parser m => Expr -> m ()
    addExpr expr = do
      State.modify $ \st -> st { pExprs = pExprs st ++ pure expr }

    addErr :: State.MonadState Parser m => Int -> String -> String -> m ()
    addErr eLine' eWhere' eMessage' = do
      let err = MkError eLine' eWhere' eMessage'
      State.modify $ \st -> st { pErrors = pErrors st ++ pure err }

    end :: State.MonadState Parser m => m ()
    end = pure ()

    go :: [Token] -> State Parser ()
    go ts =
      case expression ts of
        Nothing ->
          case ts of
            [] -> end
            t : _ -> do
              let tType = Scanner.tType t
              case tType of
                Scanner.Eof -> end
                _ -> addErr (Scanner.tLine t) (show tType) "unexpected token"
        Just (expr, ts1) -> do
          addExpr expr
          go ts1

expression :: [Token] -> Maybe (Expr, [Token])
expression = equality

binary ::
   (Expr -> op -> Expr -> Expr) -- ^ constructor
   -> ([Token] -> Maybe (Expr, [Token])) -- ^ lhs parser
   -> (Scanner.TokenType -> Maybe op) -- ^ operator parser
   -> ([Token] -> Maybe (Expr, [Token])) -- ^ rhs parser
   -> [Token] -- ^ token stream
   -> Maybe (Expr, [Token])
binary con lhsP opP rhsP ts0 = do
  (lhs, opToken : ts1) <- lhsP ts0
  let
    match op = do
      (rhs, ts2) <- rhsP ts1
      pure (con lhs op rhs, ts2)
    miss =
      pure (lhs, opToken : ts1)
  maybe miss match (opP (Scanner.tType opToken))

eqOp :: Scanner.TokenType -> Maybe EqOp
eqOp = \case
  Scanner.BangEqual -> pure NotEqual
  Scanner.EqualEqual -> pure Equal
  _ -> empty

equality :: [Token] -> Maybe (Expr, [Token])
equality = binary Equality comparison eqOp comparison

cmpOp :: Scanner.TokenType -> Maybe CmpOp
cmpOp = \case
  Scanner.Greater -> pure Greater
  Scanner.GreaterEqual -> pure GreaterEqual
  Scanner.Less -> pure Less
  Scanner.LessEqual -> pure LessEqual
  _ -> empty

comparison :: [Token] -> Maybe (Expr, [Token])
comparison = binary Comparison term cmpOp term

termOp :: Scanner.TokenType -> Maybe TermOp
termOp = \case
  Scanner.Minus -> pure Minus
  Scanner.Plus -> pure Plus
  _ -> empty

term :: [Token] -> Maybe (Expr, [Token])
term = binary Term factor termOp factor

factOp :: Scanner.TokenType -> Maybe FactOp
factOp = \case
  Scanner.Slash -> pure Div
  Scanner.Star -> pure Mul
  _ -> empty

factor :: [Token] -> Maybe (Expr, [Token])
factor = binary Factor unary factOp unary

unOp :: Scanner.TokenType -> Maybe UnOp
unOp = \case
  Scanner.Bang -> pure Not
  Scanner.Minus -> pure UnaryMinus
  _ -> empty

unary :: [Token] -> Maybe (Expr, [Token])
unary ts0@(opToken : ts1) = do
  let
    match op = do
      (expr, ts2) <- unary ts1
      pure (Unary op expr, ts2)
    miss =
      primary ts0
  maybe miss match (unOp (Scanner.tType opToken))
unary [] = empty

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
          pure (Primary (Grouping expr), ts1)
        _ -> empty
    _ -> empty
primary [] = empty

synchronize :: [Token] -> [Token]
synchronize = go
  where
    go ts0 =
      case ts0 of
        t : ts1 ->
          case Scanner.tType t of
            Scanner.Semicolon -> ts1
            Scanner.Class -> ts0
            Scanner.Fun -> ts0
            Scanner.Var -> ts0
            Scanner.For -> ts0
            Scanner.If -> ts0
            Scanner.While -> ts0
            Scanner.Print -> ts0
            Scanner.Return -> ts0
            _ -> go ts1
        [] -> []
