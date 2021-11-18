{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler
  ( CompilationState (..),
    DocumentState (..),
    runCompileT,
    runCompile,
  )
where

import qualified Data.Map as M
import Data.Map (Map)

import Data.Functor.Identity (Identity (runIdentity))

import Control.Lens.TH (makeLenses)

import Control.Monad.Trans.State.Lazy (StateT , runStateT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

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

makeLenses ''CompilationState
makeLenses ''DocumentState

-- | Custom StateT with support for error-handling
type CompileT e s m = StateT s (ExceptT e m)

type Compile e s = CompileT e s Identity

type CompileIO e s = CompileT e s IO

runCompileT :: (Monad m) => CompileT e s m a -> s -> m (Either e (a, s))
runCompileT m = runExceptT . runStateT m 

runCompile :: CompileT e s Identity a -> s -> Either e (a,s)
runCompile m = runIdentity . runCompileT m
