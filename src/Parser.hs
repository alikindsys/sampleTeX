{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser
  (    
  )
where

import Text.Megaparsec
import Data.Text
import Data.Void
import Text.Megaparsec.Char (char, alphaNumChar)
import Control.Monad
import qualified Text.Megaparsec.Char.Lexer as L

-- | The core spec types
newtype Identifier = Identifier {toStr :: Text}
    deriving (Show)
newtype CharEscape = CharEscape {getChar :: Char}
    deriving (Show)
newtype FString = FString {identifier :: Identifier}
    deriving (Show)
newtype StringLiteral = StringLiteral {text :: Text}
    deriving (Show)
data Variable = Variable {identifier :: Identifier, value :: StringLiteral}
    deriving (Show)

-- | The parser type.
type Parser = Parsec Void Text

-- | Parsers

-- | '\' - Character Escaping
-- | '$' - FString
-- | StringLiteral
-- | '\"' <inner> '\"'

-- | Keywords
-- | `out` `var`

-- | Pragmas
-- | `include` `import` `class`
