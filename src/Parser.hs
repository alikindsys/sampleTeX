module Parser ( TexObject(..)
              , SimpleTexObject(..)
              , SimpleTexFunction(..)
              , parseObject
              , parseSampleTex
              ) where

import Utils
import Text.Parsec
import Control.Monad

data TexObject = Section String
               | NewPage
               | Text String
               | List [Either SimpleTexObject TexObject]
               deriving Show

data SimpleTexObject = Variable { name :: String , value :: String }
                     | StringInterpolation SimpleTexFunction
                     | ComplexString [Either SimpleTexObject TexObject]
                     | ImportStatement String
                     | OutStatment [String]
                     deriving Show

data SimpleTexFunction = Chem String 
                       | Identifier String 
                       deriving Show


parseObject = parseEither parseSimpleTexObject parseTexObject 

parseSampleTex :: String -> [Either SimpleTexObject TexObject]

parseSampleTex [] = []

parseSampleTex str = do
    let either = parseWithLeftOver parseObject str
    case either of
        Right (item, rest) ->  item : parseSampleTex rest
        Left err -> error $ show err
---
--- TexObject Parsers
---

parseTexObject = try parseSection <|> try parsePage <|> try parseList <|> parseText

parseList :: Parser TexObject
parseList = do
    void $ string "li"
    void spaces
    value <- manyTill parseObject (string "il")
    void spaces
    return $ List value 

parseText :: Parser TexObject
parseText = do
    str <- parseBounded (\c -> c /= '\n' && c /= '{')
    spaces
    return $ Text str

parseTextTillEndOfLiteral :: Parser TexObject
parseTextTillEndOfLiteral = do
    str <- parseBounded (\c -> c /= '"' && c /= '{')
    return $ Text str

parsePage :: Parser TexObject
parsePage = do
    void $ string "page"
    void spaces 
    return NewPage

parseSection :: Parser TexObject
parseSection = do
    void $ string "sec"
    void spaces
    value <- try parseStringLiteral <|> parseTillSeparator
    void spaces
    return $ Section value

---
--- SimpleTexObject Parsers
---

parseSimpleTexObject = try parseVariable <|> try parseImportStatement <|> try parseOutStatment <|> try parseComplexString <|> parseStringInterpolation

complexStringAllowedTexObjects = try parseTextTillEndOfLiteral <|> parseText
complexStringAllowedSimpleTexObjects = parseStringInterpolation

complexStringAllowedObjects = parseEither complexStringAllowedSimpleTexObjects complexStringAllowedTexObjects


parseOutStatment = try parseListOutStatment <|> parseSingletonOutStatment

parseSingletonOutStatment :: Parser SimpleTexObject
parseSingletonOutStatment = do
    void $ string "out"
    void spaces 
    ident <- parseIdentifier
    return $ OutStatment [ident]

parseListOutStatment :: Parser SimpleTexObject
parseListOutStatment = do
    void $ string "out"
    void spaces 
    idents <- manyTill parseIdentifier (char ';')
    void spaces
    return $ OutStatment idents

parseImportStatement :: Parser SimpleTexObject
parseImportStatement = do
    void $ string "import"
    void spaces
    path <- parseStringLiteral
    void spaces
    return $ ImportStatement path


parseComplexString :: Parser SimpleTexObject
parseComplexString = do
    void $ string "$\""
    value <- manyTill complexStringAllowedObjects (char '"')
    void spaces
    return $ ComplexString value 


parseStringInterpolation :: Parser SimpleTexObject
parseStringInterpolation = do
    void $ char '{'
    expr <- parseUntil '}'
    void $ string "}"
    void spaces
    let result = parse parseFunction "" expr
    case result of
        Right func -> return $ StringInterpolation func
        Left err -> fail $ show err


parseVariable :: Parser SimpleTexObject
parseVariable = do
    string "var"
    spaces
    ident <- parseIdentifier
    void $ char '='
    void spaces
    Variable ident <$> parseStringLiteral


---
--- SimpleTexFunction Parsers
---

parseFunction :: Parser SimpleTexFunction
parseFunction = try parseChemFunction <|> parseIdentifierFunction

parseIdentifierFunction :: Parser SimpleTexFunction
parseIdentifierFunction = 
    Identifier <$> parseIdentifier
parseChemFunction :: Parser SimpleTexFunction
parseChemFunction = do
    void $ string "chem"
    void spaces
    Chem <$> parseStringLiteral

---
--- Internal Helpers
---

parseStringLiteral :: Parser String
parseStringLiteral = do
    void $ string "\""
    value <- parseUntil '"'
    void $ string "\""
    void spaces
    return value

parseIdentifier :: Parser String
parseIdentifier = do
    c <- firstChar
    str <- many nonFirstChar
    void spaces
    return (c:str)
    where
        firstChar = letter <|> char '_'
        nonFirstChar = alphaNum <|> char '_'


parseTillSeparator :: Parser String
parseTillSeparator = parseBounded (\c -> c /= ';' && c /= '\n')