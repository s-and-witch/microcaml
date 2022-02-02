{-# LANGUAGE TupleSections #-}

module Lexer.Lexer where

import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Text qualified as T
import Data.Void
import Control.Monad (void)
import Data.Vector qualified as V
import Control.Applicative hiding (some, many)
import Data.Maybe (isJust)

type Parser = Parsec Void T.Text


lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "{-" "-}"

ident :: Parser Char
ident = char ' ' <|> char '\t'

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (void $ some ident) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

moduleParser :: Parser Module -- header and list items
moduleParser = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      empty

term :: Parser a -> Parser (Term a)
term p = do
  posStart <- getSourcePos
  payload  <- p
  posEnd   <- getSourcePos
  pure $ Term payload posStart posEnd

terms :: Parser a -> Parser (V.Vector (Term a))
terms p = V.fromList <$> many (term p)

nameParser :: Parser T.Text
nameParser = T.pack <$> liftA2 (:) lowerChar (many alphaNumChar)

capNameParser :: Parser T.Text
capNameParser = liftA2 (<>) (T.singleton <$> upperChar) nameParser

pragmaParser :: Parser Pragma
pragmaParser = Pragma <$> term (between (symbol "[")
                                        (symbol "]")
                                        (T.pack <$> some alphaNumChar))

moduleDeclParser :: Parser ModuleDecl
moduleDeclParser = do
  pragmas  <- terms pragmaParser
  symbol "module"
  declName <- term capNameParser
  pure $ ModuleDecl declName pragmas

moduleImportParser :: Parser ModuleImport
moduleImportParser = do
  pragmas <- terms pragmaParser
  symbol "import"
  name    <- term capNameParser
  
  qualified <- isJust <$> optional (symbol "qualified")
  asName    <- optional $ term $  symbol "as"
                               *> capNameParser

  pure $ ModuleImport { name       = name
                      , qualified  = qualified
                      , asName     = asName
                      , pragmas    = pragmas
                      }

dataDeclParser :: Parser DataDecl
dataDeclParser = do
  pragmas <- terms pragmaParser  
  symbol "data"
  name    <- term capNameParser
  params  <- terms nameParser
  symbol ":="
  
  headCons <- term dataConstructorDeclParser
  tailCons <- terms $  symbol "|"
                    *> dataConstructorDeclParser
  
  pure $ DataDecl { name         = name
                  , params       = params
                  , constructors = headCons `V.cons` tailCons
                  , pragmas      = pragmas
                  }

dataConstructorDeclParser :: Parser DataConstructorDecl
dataConstructorDeclParser = do
  name   <- term capNameParser
  params <- terms nameParser
  pure $ DataConstructorDecl { name   = name
                             , params = params
                             }
  
data DataDecl = DataDecl
  { name         :: Term T.Text
  , params       :: V.Vector (Term T.Text)
  , constructors :: V.Vector (Term DataConstructorDecl)
  , pragmas      :: V.Vector (Term Pragma)
  }

data DataConstructorDecl = DataConstructorDecl
  { name :: Term T.Text
  , params  :: V.Vector (Term T.Text)
  }
{- 
[pragmas]
data Type a b c := Type1 a b | Type2 c | Type3

-}                      
{-
[pragmas]
module Name.Name.Name

[pragmas]
import Module.Name (qualified)? (as X)?

[pragmas]
data Type a b c := Type1 a b | Type2 c | Type3

[pragmas] 
toplevel (x : List m) : List n -> List (m + n) := \y -> bingings
-}

data Term a = Term
  { payload :: a
  , posStart :: SourcePos
  , posEnd :: SourcePos
  } deriving (Show)

data Module = Module
  { moduleDecl :: Term T.Text
  , moduleImports :: V.Vector (Term ModuleImport)
  , pragmas :: V.Vector (Term Pragma)
  } deriving (Show)

data Pragma = Pragma
  { name :: Term T.Text
  } deriving (Show)

data ModuleDecl = ModuleDecl
  { name :: Term T.Text
  , pragmas :: V.Vector (Term Pragma)
  } deriving (Show)

data ModuleImport = ModuleImport
  { name :: Term T.Text
  , qualified :: Bool
  , asName ::  Maybe (Term T.Text)
  , pragmas :: V.Vector (Term Pragma)
  } deriving (Show)




data Expr
  = Id T.Text
  | ExprLit Literal
  | Let [(Pattern, Expr)] Expr
  | LetRec [(Pattern, Expr)] Expr
  | Lam Pattern Expr
  | Case Expr [(Pattern, Expr)]
  | App Expr [Expr]
  deriving (Show)

data Literal
  = Number Int
  | String T.Text
  | Tuple [Literal]
  deriving (Show)

data Pattern 
  = Binding T.Text
  | Lit Literal
  | Deconstruct T.Text [Pattern]
  deriving (Show)


parseExpr :: Parser Expr 
parseExpr = makeApp <$> some (choice 
  [ between (symbol "(") (symbol ")") parseExpr
  , parseCase
  , parseLam
  , parseLet
  , Id <$> nameParser
  ])
  where
    makeApp [x] = x
    makeApp (x:xs) =  App x xs


parseLam :: Parser Expr
parseLam = do
  symbol "\\"
  pat <- parsePattern
  symbol "->"
  expr <- parseExpr
  pure (Lam pat expr)

parseCase :: Parser Expr 
parseCase = L.indentBlock scn do
  symbol "case"
  expr <- parseExpr
  symbol "of"
  pure $ L.IndentMany Nothing (pure . Case expr) parseBranch

parseLet :: Parser Expr
parseLet = do
  symbol "let"
  r <- optional $ symbol "rec"
  bindings <- L.indentBlock scn do
    pure $ L.IndentSome Nothing pure do
      pat <- parsePattern
      symbol ":="
      expr <- parseExpr
      pure (pat, expr)
  symbol "in"
  expr <- parseExpr
  pure case r of
    Just{} -> LetRec bindings expr
    _ -> Let bindings expr

parseBranch :: Parser (Pattern, Expr)
parseBranch = do
  pat <- parsePattern
  symbol "->"
  expr <- parseExpr
  pure (pat, expr)

parsePattern :: Parser Pattern  
parsePattern = undefined

parseLit :: Parser Literal
parseLit = choice 
  [ 
  ]

