{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lexer.Lexer where

import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Text qualified as T
import Data.Void
import Control.Monad (void)
import Data.Vector qualified as V
import Control.Applicative hiding (some, many)
import Data.Fix
import Data.Maybe (isJust)
import Data.List

import Data.Functor.Syntax((<~$>), (<~~~$>))

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

betweenStr :: T.Text -> T.Text -> Parser a -> Parser a
betweenStr open close = between (symbol open) (symbol close)

parens :: Parser a -> Parser a
parens = betweenStr "(" ")"

squareParens :: Parser a -> Parser a
squareParens = betweenStr "[" "]"

moduleParser :: Parser Module -- header and list items
moduleParser = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      empty

term :: Parser a -> Parser (Term a)
term p = Term <~$> getSourcePos <*> lexeme p <*> getSourcePos
-- term p = do
--   posStart <- getSourcePos
--   payload  <- lexeme p
--   posEnd   <- getSourcePos
--   pure $ Term payload posStart posEnd

terms :: Parser a -> Parser (V.Vector (Term a))
terms p = V.fromList <$> many (term p)

nameParser :: Parser T.Text
nameParser = T.pack <$> liftA2 (:) lowerChar (many alphaNumChar)

capNameParser :: Parser T.Text
capNameParser = liftA2 (<>) (T.singleton <$> upperChar) (T.pack <$> many alphaNumChar)

moduleNameParser :: Parser ModuleName
moduleNameParser = V.cons <$> term capNameParser
                          <*> terms (string "." *> capNameParser)

inlinePragmaParser :: Parser Pragma
inlinePragmaParser = Pragma <$> term (squareParens $ T.pack <$> some alphaNumChar)

pragmaParser :: Parser Pragma
pragmaParser = inlinePragmaParser <* scn

moduleDeclParser :: Parser ModuleDecl
moduleDeclParser = ModuleDecl <~$> (terms pragmaParser <* symbol "module")
                               <*> term moduleNameParser

moduleImportParser :: Parser ModuleImport
moduleImportParser = ModuleImport <~~~$> terms pragmaParser
                                     <*> (symbol "import" *> term moduleNameParser)
                                     <*> (isJust <$> optional (symbol "qualified"))
                                     <*> optional (term $ symbol "as" *> moduleNameParser)

dataDeclParser :: Parser DataDecl
dataDeclParser = do
  pragmas <- terms pragmaParser
  symbol "data"
  name    <- term capNameParser
  params  <- terms nameParser
  symbol ":="

  headCons <- term dataConstructorDeclParser
  tailCons <- terms $ symbol "|" *> dataConstructorDeclParser
  let constructors = headCons `V.cons` tailCons

  pure $ DataDecl {..}

dataConstructorDeclParser :: Parser DataConstructorDecl
dataConstructorDeclParser =  DataConstructorDecl
                         <$> term capNameParser
                         <*> terms nameParser


data DataDecl = DataDecl
  { name         :: Term T.Text
  , params       :: V.Vector (Term T.Text)
  , constructors :: V.Vector (Term DataConstructorDecl)
  , pragmas      :: V.Vector (Term Pragma)
  }
  deriving (Show)

data DataConstructorDecl = DataConstructorDecl
  { name :: Term T.Text
  , params  :: V.Vector (Term T.Text)
  }
  deriving (Show)
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
  }

instance Show a => Show (Term a) where
  show Term{..} = "{" <> show payload <> "} [" <> showSourcePos posStart <> "," <> showSourcePos posEnd <> "]"
    where
      showSourcePos SourcePos{..} = showPos sourceLine <> ":" <> showPos sourceColumn
      showPos = drop 4 . show



type ModuleName = V.Vector (Term T.Text)

data Module = Module
  { moduleDecl :: Term ModuleName
  , moduleImports :: V.Vector (Term ModuleImport)
  , pragmas :: V.Vector (Term Pragma)
  } deriving (Show)

data Pragma = Pragma
  { name :: Term T.Text
  } deriving (Show)

data ModuleDecl = ModuleDecl
  { name :: Term ModuleName
  , pragmas :: V.Vector (Term Pragma)
  } deriving (Show)

data ModuleImport = ModuleImport
  { name :: Term ModuleName
  , qualified :: Bool
  , asName ::  Maybe (Term ModuleName)
  , pragmas :: V.Vector (Term Pragma)
  } deriving (Show)



type Expr = Fix ExprF

data ExprF next
  = ExprLit Literal
  | Let [(Pattern, next)] next
  | LetRec [(Pattern, next)] next
  | Lam Pattern next
  | Case next [(Pattern, next)]
  | App next [next]
  | Pat (PatternF next)
  deriving Functor


instance {-# OVERLAPPING #-} Show Expr where
  show = foldFix \case
      ExprLit l -> show l
      Let bindings next -> letBuilder "let " bindings next
      LetRec bindings next -> letBuilder "let rec " bindings next
      Lam pat next -> "\\" <> show pat <> " -> " <> next
      Case expr branches -> "case " <> expr <> " of {\n" <> concatMap showBranch branches <> "}\n"
      App func args -> "[app] " <> func <> " " <> unwords args
      Pat p -> showPattern p
    where
      letBuilder :: String -> [(Pattern, String)] -> String -> String
      letBuilder prefix bindings next = prefix <> "{\n" <> concatMap showBind bindings <> "}\n in " <> next
      showBind :: (Pattern, String) -> String
      showBind (pat, res) = show pat <> " := " <> res <> ";\n"
      showBranch (pat, res) = show pat <> " -> " <> res <> ";\n"

data Literal
  = Number Int
  | String T.Text
  deriving (Show)

type Pattern = Fix PatternF

type Construct = PatternF Expr

data PatternF next
  = Id T.Text
  | Lit Literal
  | Tuple [next]
  | Deconstruct T.Text [next]
  deriving Functor

showPattern :: PatternF String -> String
showPattern = \case
  Id t -> T.unpack t
  Lit l -> show l
  Tuple xs -> "(" <> intercalate ", " xs <> ")"
  Deconstruct constr vals -> "(" <> T.unpack constr <> " " <> unwords vals <> ")"


instance {-# OVERLAPPING #-} Show Pattern where
  show = foldFix showPattern


parseExpr :: Parser Expr
parseExpr =
  makeApp <$> some (lexeme (choice
    [ try $ between (symbol "(") (symbol ")") parseExpr
    , parseCase
    , parseLam
    , parseLet
    , wrapFix . Pat <$> parseConstruct
    ]))
  where
    makeApp [x] =  x
    makeApp (x:xs) = wrapFix $ App x xs


parseLam :: Parser Expr
parseLam = do
   symbol "\\"
   pat <- parseDeconstruct
   symbol "->"
   expr <- parseExpr
   pure . Fix $ Lam pat expr


parseCase :: Parser Expr
parseCase = L.indentBlock scn do
  symbol "case"
  --expr <- parseExpr
  symbol "of"
  pure $ L.IndentMany
    Nothing
    (pure . wrapFix . Case (wrapFix (ExprLit (Number 5))))
    parseBranch

parseLet :: Parser Expr
parseLet = do
  symbol "let"
  recur <- fmap isJust . optional $ symbol "rec"
  bindings <- L.indentBlock scn do
    pure $ L.IndentSome Nothing pure do
      pat <- parseDeconstruct
      symbol ":="
      expr <- parseExpr
      pure (pat, expr)
  symbol "in"
  wrapFix . ( if recur then LetRec else Let )  bindings <$> parseExpr

parseBranch :: Parser (Pattern, Expr)
parseBranch =  do
  pat <- parseDeconstruct
  symbol "->"
  expr <- parseLit
  --expr <- parseExpr
  pure ( pat, wrapFix $ ExprLit expr)


parseDeconstruct :: Parser Pattern
parseDeconstruct = Fix <$> parsePattern parseDeconstruct

parseConstruct :: Parser Construct
parseConstruct = parsePattern parseExpr

parsePattern :: Parser a -> Parser (PatternF a)
parsePattern p = lexeme $ choice
  [ Tuple <$> parseTuple p
  , Id <$> nameParser
  , Lit <$> parseLit
  , Deconstruct <$> capNameParser <*> many p
  ]

parseLit :: Parser Literal
parseLit = choice
  [ Number <$> L.signed sc (lexeme L.decimal)
  , String . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))
  ]

parseTuple :: Parser a -> Parser [a]
parseTuple p = between (symbol "(") (symbol ")") (liftA2 (:) p (some (symbol "," *> p)))


