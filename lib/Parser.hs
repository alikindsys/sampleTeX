module Parser(
    TexObject(..)
) where

import Utils
import Text.Parsec
import Control.Monad

data TexObject = Section String
               | NewPage
               | SimpleText String
               | List [TexObject]

data SimpleTexObject = Variable { name :: String , value :: String }
                     | StringInterpolation SimpleTexFunction
                     | ComplexString [Either TexObject SimpleTexObject]

data SimpleTexFunction = Chem String 
                       | Identifier String 

---
--- SimpleTexFunction Parsers
---

parseFunction :: Parser SimpleTexFunction
parseFunction = try parseChemFunction <|> parseIdentifierFunction

parseIdentifierFunction :: Parser SimpleTexFunction
parseIdentifierFunction = do
    fc <- firstChar
    nfc <- many nonFirstChar
    void spaces
    return $ Identifier (fc:nfc)
    where
        firstChar = letter <|> char '_'
        nonFirstChar = alphaNum <|> char '_'

parseChemFunction :: Parser SimpleTexFunction
parseChemFunction = do
    void $ string "chem"
    void spaces
    Chem <$> parseStringLiteral

parseStringLiteral :: Parser String
parseStringLiteral = do
    void $ string "\""
    value <- parseUntil '"'
    void $ string "\""
    void spaces
    return value