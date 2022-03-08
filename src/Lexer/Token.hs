module Lexer.Token where
import Data.ByteString.Lazy
import Control.Lens

data Pos = Pos 
  { _line   :: !Int
  , _column :: !Int 
  } deriving Show


data Loc = Loc 
  { _posStart :: !Pos
  , _posEnd   :: !Pos
  } deriving Show


data Token 
  = Lam             !Loc            -- \
  | Let             !Loc            -- let
  | In              !Loc            -- in
  | Case            !Loc            -- case
  | Of              !Loc            -- of
  | ArrRigth        !Loc            -- ->
  | ArrLeft         !Loc            -- <-
  | Col             !Loc            -- :
  | BlockStart      !Loc            -- {
  | DeclEnd         !Loc            -- ;
  | BlockEnd        !Loc            -- }
  | Module          !Loc            -- module
  | Where           !Loc            -- where
  | Data            !Loc            -- data
  | Bind            !Loc            -- :=
  | PLus            !Loc            -- +
  | Minus           !Loc            -- -
  | Div             !Loc            -- /
  | Mul             !Loc            -- \*
  | Less            !Loc            -- <
  | More            !Loc            -- >
  | Eq              !Loc            -- ==
  | LessEq          !Loc            -- <=
  | MoreEq          !Loc            -- >=
  | LeftBrace       !Loc            -- {
  | RightBrace      !Loc            -- }
  | LeftBracket     !Loc            -- [
  | RightBracket    !Loc            -- ]
  | LeftParenthese  !Loc            -- (
  | RightParenthese !Loc            -- )
  | Comma           !Loc            -- ,
  | Int             !Loc Integer    -- 0|1|2|...
  | Double          !Loc Double     -- 0.1 | 0.2 | 0.3 | ...
  | String          !Loc ByteString -- "string"
  | Id              !Loc ByteString -- dsdsl 
  deriving Show


{-
braces {}
brackets []
parentheses ()
-}


makeLenses ''Pos 
makeLenses ''Loc


{-

data AlexUserState = AlexUserState

alexInitUserState = AlexUserState

alexEOF = pure [NoOp]

-- The token type:
data Token 
  = Dot Int64
  | DoubleDot Int64
  | NoOp
  deriving (Eq,Show)

lex :: ByteString.ByteString -> Either String [Token]
lex s = runAlex s alexMonadScan
-}