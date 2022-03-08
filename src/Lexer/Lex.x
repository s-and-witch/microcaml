{
module Lexer.Lex where
import Data.DList as D
}


%wrapper "monadUserState-bytestring"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  ".."					{ const \n -> pure [DoubleDot n] }
  "."					  { const \n -> dotLex }

{
-- Each action has type :: String -> Token


data AlexUserState = AlexUserState

alexInitUserState = AlexUserState

alexEOF = pure [EOF]

-- The token type:
data Token 
  = Dot ByteString.ByteString
  | DoubleDot Int64
  | EOF
  deriving (Eq,Show)

dotLex = Alex \st -> Right (st, [Dot (alex_inp st)])

lex :: ByteString.ByteString -> Either String [Token]
lex s = D.toList <$> runAlex s do
  let 
    loop toks = do
      tok <- alexMonadScan
      if tok == [EOF] then pure toks
      else loop (toks <> D.fromList tok)
  loop empty


}