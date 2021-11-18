module Compiler
  ( CompilationState (..),
    DocumentState (..),
  )
where

import qualified Data.Map as M
import Data.Map (Map)

import Parser (PathKind, Identifier (..), StringLiteral (..))

data CompilationState = CompilationState
  { _file :: String,
    _pwd :: String,
    _kind :: PathKind
  }
  deriving (Show)

data DocumentState = DocumentState
  { _variableMap :: Map Identifier StringLiteral,
    _stack :: [String],
    _hasClass :: Bool,
    _initialized :: Bool,
    _compilation :: CompilationState
  }
  deriving (Show)
