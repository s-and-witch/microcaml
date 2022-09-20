{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lexer.Combinators where

import Lexer.Token
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.DList as D
import Prelude hiding (last)

class LexerMonad m where
  getPos :: m Pos
  getEntryLoc :: m Loc
  getLexemeContent :: m ByteString
  getLexemeSize :: m Int64
  getLastLayout :: m Int64
  setNewLayout :: Int64 -> m ()
  endLayout :: m ()
  expectedNewLayout :: m Bool
  requestNewLayout :: m ()
  closeLayoutRequest :: m ()
  throwError :: Error -> m a


data Error = UnexpectedBlockEnd 
  deriving (Show, Eq)

getCurrentLayout :: (Functor f, LexerMonad f) => f Int64
getCurrentLayout = getLexemeSize <&> \size -> size - 1

constPos :: (Functor f, LexerMonad f) => Int -> f Loc
constPos n = getPos <&> 
  \pos -> Loc pos (pos & column +~ n)

unaryPos :: (Functor f, LexerMonad f) => f Loc
unaryPos = constPos 1

layoutTok :: (Monad m, LexerMonad m) => m [Token]
layoutTok = do
  current <- getCurrentLayout
  last <- getLastLayout
  loc <- getEntryLoc 
            <&> posStart.line +~ 1
            <&> posStart.column .~ 1
  newL <- expectedNewLayout
  if | current == last && not newL -> 
        pure [DeclEnd loc] 
     | current > last && newL -> do
        setNewLayout current
        pure [BlockStart loc]
     | current > last -> pure []
     | current < last && not newL -> do
        let
          loop xs = do
            endLayout
            last <- getLastLayout
            if current == last 
              then pure (xs <> D.fromList [BlockEnd loc, DeclEnd loc])
              else loop (xs <> singleton (BlockEnd loc))
        D.toList <$> loop empty
     | otherwise -> 
        throwError UnexpectedBlockEnd



