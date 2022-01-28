
{-# LANGUAGE OverloadedStrings #-}

module Main where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.Text

data AST = 
  Id 


main :: IO ()
main = putStrLn "hui"

