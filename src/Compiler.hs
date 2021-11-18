{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

import qualified Data.Text as T

import Data.Functor.Identity (Identity (runIdentity))

import Control.Lens.TH (makeLenses)
import Control.Lens ((.~), (&))

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Lazy (StateT , runStateT, get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import Parser 

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

texify (VariableExport' (VariableExport [])) =
  lift . throwE $ "Attempted compiling an empty list"
texify (VariableExport' (VariableExport xs)) = do
  state <- get
  let _map = _variableMap state
  let kvps = map (\ident -> (,) ident $ M.member ident _map) xs
  let invalids = filter (not . snd) kvps
  if not . null $ invalids
    then
      lift . throwE $
        "The following variable(s) were never declared but tried exporting:"
          <> concatMap ((<>) "\n" . toStr . fst) invalids
    else pure $ concatMap tex $ M.toAscList _map
  where
    tex (ident, value) =
      "\\newcommand{\\" <> toStr ident <> "}{" <> text value <> "}\n"
    getVar _map key = _map M.! key

-- | Pragmas

texify (Pragma' Init) = do
  state <- get
  if _initialized state
    then
      lift . throwE $
        "Attempted initializing an already initialized document."
    else do
      put $ state & initialized .~ True & stack .~ "document" : _stack state
      pure "\\begin{document}\n"

texify (Pragma' (Begin k)) = do
  state <- get
  let func = toStr k
  let stk = _stack state
  let lowerCased = T.toLower (T.pack func)
  if lowerCased == "document" && _initialized state
    then lift . throwE $ "Duplicate Document Initialization."
    else
      if lowerCased == "document"
        then put $ state & stack .~ (toStr k : stk) & initialized .~ True
        else put $ state & stack .~ (toStr k : stk)
  pure $ "\\begin{" <> toStr k <> "}\n"

texify (Pragma' End) = do
  state <- get
  let stk = _stack state
  if null stk
    then
      lift . throwE $
        "Unescaped <End>. Please check if you have more <end>s then <begin>s."
    else do
      put $ state & stack .~ tail stk
      pure $ "\\end{" <> head stk <> "}\n"

texify (Pragma' (Import pack funcs)) = do
  pure $ "\\usepackage" <> funcStr <> "{" <> toStr pack <> "}\n"
  where
    funcStr =
      if null funcs
        then ""
        else "[" <> concatMap ((<>) "," . texifyFunctionKind) funcs <> "]"

texifyFunctionKind :: FunctionKind -> String
texifyFunctionKind (Setting k v) = toStr k <> "=" <> v
texifyFunctionKind (Function i) = toStr i
texifyFunctionKind (Value v) = v

texifyStringComponent ::  StringComponent -> Compile String DocumentState String
texifyStringComponent (EscapeSequence c) = pure [Parser.getChar c]
texifyStringComponent (Literal s) = pure s
texifyStringComponent (VariableReplacement (FString id)) = do
  state <- get
  let _map = _variableMap state
  let member = M.member id _map
  if member
    then pure . text $ _map M.! id
    else error $ "The variable " <> toStr id <> " was not declared."
