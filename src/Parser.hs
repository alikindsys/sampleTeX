{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser
  ( parseEscapeSequence,
    parseIdentifier,
    parseFString,
    parseStringLiteral,
    parseVariableDefinition,
  )
where

import Control.Monad
import Data.Text
import Data.Void
import Data.String
import Text.Megaparsec.Char
    ( char, alphaNumChar, alphaNumChar, char, letterChar, string, space1 )
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- | The core spec types
newtype Identifier = Identifier {toStr :: String}
    deriving (Show)
newtype CharEscape = CharEscape {getChar :: Char}
    deriving (Show)
newtype FString = FString {identifier :: Identifier}
    deriving (Show)
newtype StringLiteral = StringLiteral {text :: String}
    deriving (Show)
data Variable = Variable {identifier :: Identifier, value :: StringLiteral}
    deriving (Show)

-- | The parser type.
type Parser = Parsec Void Text

-- | Parsers

-- | '\' - Character Escaping
parseEscapeSequence :: Parser CharEscape
parseEscapeSequence = do
    void  $ single '\\'
    char <- L.charLiteral
    pure CharEscape{getChar=char}

-- | Identifier
parseIdentifier :: Parser Identifier
parseIdentifier = do
    ident <- (:) <$> letterChar <*> many alphaNumChar
    pure Identifier{toStr=ident}

-- | '$' - FString
parseFString :: Parser FString
parseFString = do
    void $ single '$'
    ident <- parseIdentifier
    pure FString{identifier=ident}

-- | StringLiteral
-- | '\"' <inner> '\"'
parseStringLiteral :: Parser StringLiteral
parseStringLiteral = do
    x <- char '"' >> manyTill L.charLiteral (char '"')
    pure StringLiteral {text=x}

parseKeyword :: String -> Parser Text
parseKeyword keyword =  string (fromString keyword) <* notFollowedBy alphaNumChar

-- | Keywords
-- | `out` `var`
parseVariableDefinition :: Parser Variable
parseVariableDefinition = do
    void $ parseKeyword "var"
    void space1
    ident <- parseIdentifier
    void space1
    void $ char '='
    void space1
    value <- parseStringLiteral
    pure Variable {identifier=ident, value=value}

-- | Pragmas
-- | `include` `import` `class`
