module Compiler
  ( CompilationState (..),
  )
where

import Parser (PathKind)

data CompilationState = CompilationState
  { _file :: String,
    _pwd :: String,
    _kind :: PathKind
  }
  deriving (Show)
