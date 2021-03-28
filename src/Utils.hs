module Utils (parseBounded
             , parseUntil
             , parseEither
             , parseWithLeftOver
             , Parser
             ) where

import Text.Parsec ( satisfy, many1, manyTill, anyToken, eof, (<|>), ParseError, parse )
import Text.Parsec.String (Parser)

parseBounded :: (Char -> Bool) -> Parser String
parseBounded f = many1 nonX
    where nonX = satisfy f

parseUntil :: Char -> Parser String
parseUntil x = parseBounded (/= x)

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither pa pb = (Left <$> pa) <|> (Right <$> pb)

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
   where leftOver = manyTill anyToken eof