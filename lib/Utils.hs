module Utils (parseBounded
             , parseUntil
             , parseEither
             , Parser
             ) where

import Text.Parsec ( satisfy, many1, (<|>) ) 
import Text.Parsec.String (Parser)

parseBounded :: (Char -> Bool) -> Parser String
parseBounded f = many1 nonX
    where nonX = satisfy f

parseUntil :: Char -> Parser String
parseUntil x = parseBounded (/= x)

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither pa pb = (Left <$> pa) <|> (Right <$> pb)