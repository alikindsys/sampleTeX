{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser
  ( parseEscapeSequence,
    parseIdentifier,
    parseFString,
    parseStringLiteral,
    parseVariableDefinition,
    parseStringComponent,
    parseCompoundString,
    parseVariableExport,
    parseAnyPragma,
    parseList,
    parseBlock,
  )
where

import Control.Monad
import Control.Lens hiding (List)
import Data.Text
import Data.Void
import Data.Maybe
import Data.String
import Text.Megaparsec.Char
    ( char, alphaNumChar, alphaNumChar, char, letterChar, string, hspace1, eol )
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
data Pragma = Include {path :: String, kind :: PathKind} 
            | Import {package :: Identifier, functions :: [FunctionKind]} 
            | Begin {function :: Identifier} 
            | Class {package :: Identifier, functions :: [FunctionKind]}
            | End 
            | Init
            deriving (Show)
data PathKind = SampleTex | LaTeX
    deriving (Show)
data FunctionKind = Setting {key :: Identifier, value :: String} | Function {identifier :: Identifier} | Value {value :: String}
    deriving (Show)
data ListItem = StringLit StringLiteral | CompString CompoundString | InnerList List
    deriving (Show)
data List = List {items :: [ListItem], _name :: Maybe CompoundString, _ordered :: Bool}
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
reservedSymbols = choice [
        void parseEscapeSequence,
        void parseFString
    ]

parseWord :: Parser String
parseWord = someTill C.printChar $ choice [
        reservedSymbols,
        hspace1,
        eof
    ]

-- | Compound String
parseCompoundString :: Parser CompoundString
parseCompoundString = do
    components <- string "$\"" >> someTill parseStringComponent (void (char '"'))
    pure CompoundString{components=components}

parseKeyword :: String -> Parser Text
parseKeyword keyword =  string (fromString keyword) <* notFollowedBy alphaNumChar

-- | Keywords
-- | `out` `var`
parseVariableDefinition :: Parser Variable
parseVariableDefinition = do
    void $ parseKeyword "var"
    void hspace1
    ident <- parseIdentifier
    void hspace1
    void $ char '='
    void hspace1
    value <- parseStringLiteral
    pure Variable {identifier=ident, value=value}

parseIdentifiers :: Parser [Identifier]
parseIdentifiers = sepBy1 parseIdentifier (char ',') 

-- | Variable Export
parseVariableExport :: Parser VariableExport
parseVariableExport = do
    void $ parseKeyword "out"
    void hspace1
    VariableExport <$> parseIdentifiers

-- | Pragmas
-- | `include` `import` `class`
parseAnyPragma :: Parser Pragma
parseAnyPragma = char '#' *> choice [
        parseEnd,
        parseInit,
        parseImport,
        parseInclude,
        parseBegin,
        parseClass
    ]

parseInclude :: Parser Pragma
parseInclude = do 
    void $ parseKeyword "include"
    void hspace1 
    strLit <- parseStringLiteral
    let path = text strLit
    let packed = pack path
    if ".sample" `isSuffixOf` packed then
        pure Include {path= path, kind=SampleTex}
    else
        if ".tex" `isSuffixOf` packed then
            pure Include {path= path, kind=LaTeX}
        else
            fail "Invalid File Type. Expected either `.tex` or `.sample`"

-- | Import Pragma
parseImport :: Parser Pragma
parseImport = do
    void $ parseKeyword "import"
    void hspace1 
    ident <- parseIdentifier
    inner <- optional $ hspace1 *> char '(' *> some parseFunctionKindWithComma <* char ')'
    pure Import {package=ident, functions=fromMaybe [] inner} 

parseFunctionKindWithComma :: Parser FunctionKind
parseFunctionKindWithComma = optional hspace1 *> parseFunctionKind <* optional (char ',')

-- | Function Kind
parseFunctionKind :: Parser FunctionKind 
parseFunctionKind = choice [
        -- Backtracking info :
        -- Required due to the first instruction of `parseSetting` being `parseIdentifier`.
        try parseSetting,
        Function <$> parseIdentifier,
        Value <$> some alphaNumChar
    ] 

parseSetting :: Parser FunctionKind
parseSetting = do
    ident <- parseIdentifier 
    void $ optional hspace1
    void $ char '='
    void $ optional hspace1 
    value <- some alphaNumChar
    pure Setting{key=ident, value=value}

-- | Begin Pragma
parseBegin :: Parser Pragma
parseBegin = Begin <$> (parseKeyword "begin" *> hspace1 *> parseIdentifier)

-- | Class Pragma
parseClass :: Parser Pragma
parseClass = do
    void $ parseKeyword "class"
    void hspace1 
    ident <- parseIdentifier
    inner <- optional $ hspace1 *> char '(' *> some parseFunctionKindWithComma <* char ')'
    pure Class {package=ident, functions=fromMaybe [] inner} 

-- | End Pragma
parseEnd :: Parser Pragma
parseEnd = End <$ parseKeyword "end"

-- | Init Pragma
parseInit :: Parser Pragma
parseInit = Init <$ parseKeyword "init"

makeLenses ''List

-- | List Item 
parseListItem :: Parser ListItem 
-- Backtracking Info:
-- `parseList` contains parsers that use `parseStringLiteral` and `parseCompoundString`
parseListItem = try (InnerList <$> parseList) <|> (StringLit <$> parseStringLiteral) <|> (CompString <$> parseCompoundString)

-- | List Datum
-- Helper parser for getting an array (comma-separated) of ListItem
parseListDatum :: Parser [ListItem]
parseListDatum = sepBy1 parseListItem (char ',')

-- | Parses any list on the spec. 
parseList :: Parser List 
-- Backtracking info:
-- Backtracking is required between `parseUnorderedNamedList` and `parseNamedList`
-- due to their initial instructions being equal.
parseList = try parseUnorderedNamedList <|> parseNamedList <|> parseUnorderedList <|> parseSimpleOrderedList

-- | Simple Oredered List 
-- `[list datum]`
parseSimpleOrderedList :: Parser List
parseSimpleOrderedList = do 
    xs <- between (char '[') (char ']') parseListDatum
    pure List {items=xs, _name=Nothing, _ordered=True}

-- | Unordered List
-- `u[list datum]`
parseUnorderedList :: Parser List
parseUnorderedList = do
    list <- char 'u' >> parseSimpleOrderedList
    pure $ list & ordered .~ False 

-- | Named List
-- `(String Literal | Compound String):[]`
parseNamedList :: Parser List
parseNamedList = do
    x <- (wrap <$> parseStringLiteral) <|> parseCompoundString
    list <- char ':' >> parseSimpleOrderedList
    pure $ list & name ?~ x

-- | Unordered Named List
-- `(String Literal | Compound String)(u: | :u)[]`
parseUnorderedNamedList :: Parser List
parseUnorderedNamedList = do
    x <- (wrap <$> parseStringLiteral) <|> parseCompoundString
    list <- (string "u:" <|> string ":u") >> parseSimpleOrderedList
    pure $ list & name ?~ x & ordered .~ False

-- | Parse a block item
parseBlockItem :: Parser ListItem
parseBlockItem = (InnerList <$> (optional hspace1 >> parseBlock)) <|> (char '-' >> optional hspace1 >> parseListItem)

-- | Block Datum
parseBlockDatum :: Parser [ListItem]
parseBlockDatum =  optional hspace1 >> sepBy1 parseBlockItem (char '\n')

-- | Parses any block
parseBlock :: Parser List 
-- Backtracking Info:
-- Backtracking is required between `parseUnorderedNamedBlock` and `parseNamedBlock`
-- due to their initial instructions being equal.
parseBlock = try parseUnorderedNamedBlock <|> parseNamedBlock <|> parseUnorderedBlock <|> parseSimpleOrderedBlock

-- | Parses a simple block
-- {BlockDatum}
parseSimpleOrderedBlock :: Parser List
parseSimpleOrderedBlock = do
    xs <- between open close parseBlockDatum
    pure $ List {items=xs, _name=Nothing, _ordered=True}
    where
        -- If this manual handling of whitespace gives a problem i'll look into a better solution
        -- for now it works, and as such. I don't care.
        open = char '{' >> optional hspace1 >> char '\n' :: Parser Char
        close = char ';' >> optional hspace1 >> char '\n' >> optional hspace1 >> char '}'  :: Parser Char

-- | Unordered Block
-- `u{}`
parseUnorderedBlock :: Parser List
parseUnorderedBlock = do
    list <- char 'u' >> parseSimpleOrderedBlock
    pure $ list & ordered .~ False 

-- | Named Block
-- `(String Literal | Compound String):{}`
parseNamedBlock :: Parser List
parseNamedBlock = do
    x <- (wrap <$> parseStringLiteral) <|> parseCompoundString
    list <- char ':' >> parseSimpleOrderedBlock
    pure $ list & name ?~ x

-- | Unordered Named Block
-- `(String Literal | Compound String)(u: | :u){}`
parseUnorderedNamedBlock :: Parser List
parseUnorderedNamedBlock = do
    x <- (wrap <$> parseStringLiteral) <|> parseCompoundString
    list <- (string "u:" <|> string ":u") >> parseSimpleOrderedBlock
    pure $ list & name ?~ x & ordered .~ False

wrap :: StringLiteral -> CompoundString
wrap a = CompoundString [Literal $ text a]
