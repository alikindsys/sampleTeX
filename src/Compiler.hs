{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler
  ( CompilationState (..),
    DocumentState (..),
    runCompileT,
    runCompile,
    texify,
  )
where

import qualified Data.Map as M
import Data.Map (Map)

import Data.Functor.Identity (Identity (runIdentity))

import Control.Lens.TH (makeLenses)
import Control.Lens ((.~), (&))

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Lazy (StateT , runStateT, get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import Parser (PathKind, Identifier (..), StringLiteral (..), Object (..), Variable (..))

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

texify :: Object  -> Compile String DocumentState String

texify (Variable' v) = do
  state <- get
  let ident = (identifier :: Variable -> Identifier) v
  let val = (value :: Variable -> StringLiteral) v
  let vmap = _variableMap state
  if M.member ident vmap
    then lift . throwE $ "Variable " <> toStr ident <> " was already defined."
    else put $ state & variableMap .~ M.insert ident val vmap
  pure ""
