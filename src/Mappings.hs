module Mappings (toTex) where

import Parser
import Data.Maybe 

toTex :: TexObject -> Maybe String
toTex NewPage = Just "\\newpage"
toTex (Section name) = Just $ "\\section{"++ name ++"}"
toTex (Chapter x) = Just $ "\\chapter{" ++ x ++ "}"
toTex (Text text) = Just text
toTex (List x) = Just $ "\\begin{enumerate}" ++ concatMap itemize x ++ "\\end{enumerate}"
toTex Noop = Nothing

itemize :: Either SimpleTexObject TexObject -> String
itemize (Left x) = error $ "Invalid Operation : Attempted Mapping a SimpleTexObject.\nObject : " ++ show x
itemize (Right x) = maybe "" ("\\item " ++) result
  where
    result = toTex x

