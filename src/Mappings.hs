module Mappings (toTex) where

import Parser

toTex :: TexObject -> String
toTex NewPage = "\\newpage"
toTex (Section name) = "\\section{"++ name ++"}"
toTex (Text text) = text
toTex (List x) = "\\begin{enumerate}" ++ concatMap itemize x ++ "\\end{enumerate}"

itemize :: Either SimpleTexObject TexObject -> String
itemize (Left x) = error $ "Invalid Operation : Attempted Mapping a SimpleTexObject.\nObject : " ++ show x
itemize (Right x) = "\\item " ++ toTex x

