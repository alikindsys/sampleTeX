{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser
  ( parseEscapeSequence,
    parseIdentifier,
    parseFString,
    parseStringLiteral,
    parseVariableDefinition,
    parseStringComponent,
    parseCompoundString,
    parseVariableExport,
  )
where

import Control.Monad
import Data.Text
import Data.Void
import Data.String
import Text.Megaparsec.Char
    ( char, alphaNumChar, alphaNumChar, char, letterChar, string, space1, eol )
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

-- | Abstractions
data StringComponent = Literal String | VariableReplacement FString | EscapeSequence CharEscape
    deriving (Show)
newtype CompoundString = CompoundString {components :: [StringComponent]}
    deriving (Show)
newtype VariableExport = VariableExport {identifiers :: [Identifier]}
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

-- | String Components
parseStringComponent :: Parser StringComponent
parseStringComponent = choice
    [ EscapeSequence <$> parseEscapeSequence,
      VariableReplacement <$> parseFString,
      Literal <$> parseWord
    ]

reservedSymbols :: Parser ()
reservedSymbols = try $ choice [
        void parseEscapeSequence,
        void parseFString
    ]

parseWord :: Parser String
parseWord = someTill L.charLiteral $ choice [
        reservedSymbols,
        void $ char ' ',
        eof
    ]

-- | Compound String
parseCompoundString :: Parser CompoundString
parseCompoundString = do
    components <- someTill parseStringComponent (void eol <|> eof)
    pure CompoundString{components=components}

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

parseIdentifierWithComma :: Parser Identifier
parseIdentifierWithComma = space1 *> parseIdentifier <* optional (char ',')

-- | Variable Export
parseVariableExport :: Parser VariableExport
parseVariableExport = do
    void $ parseKeyword "out"
    void space1
    VariableExport <$> someTill parseIdentifierWithComma (void eol <|> eof)

-- | Pragmas
-- | `include` `import` `class`
