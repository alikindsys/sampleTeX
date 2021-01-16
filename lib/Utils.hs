module Utils (parseBounded
             , parseUntil
             , Parser
             ) where

import Text.Parsec ( satisfy, many1 )
import Text.Parsec.String (Parser)

parseBounded :: (Char -> Bool) -> Parser String
parseBounded f = many1 nonX
    where nonX = satisfy f

parseUntil :: Char -> Parser String
parseUntil x = parseBounded (/= x)