module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr qualified as Expr

import AST
import Lexer qualified as L

identifier :: Parser T.Text
identifier = L.lexeme $ T.pack <$> str
  where
    str = (:) <$> symbolChar

integer :: Parser Expr
integer = Literal . Int <$> L.integer

string :: Parser Expr
string = stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

tuple :: Pareser Expr
tuple = L.parens $ expr >> many $ comma >> expr

letExpr :: Parser Expr
letExpr = do
  symbol "let"
  p <- pattern
  symbol "="
  e <- expr
  pure Let (Type "LOL") p e

expr :: Parser Expr
expr = term

app :: Parser Expr
app = App <$> expr
          <*> some expr

term :: Parser Expr
term =  try integer
    <|> try call
    <|> L.parens expr


