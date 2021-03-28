module Grammar  ( Document(..)
                , blankDocument
                , checkObjects
                ) where

import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath (takeExtension)

data Document
    = SampleTex {variables :: Map String String, imports :: Map ValidFile String , body :: [Either SimpleTexObject TexObject]}

data ValidFile = Tex String 
               | Sample String
               deriving Show

blankDocument :: Document
blankDocument = SampleTex Map.empty Map.empty []

checkGrammar :: Document -> Either SimpleTexObject TexObject -> Document

checkGrammar doc (Right b) = SampleTex  (variables doc) (imports doc) (Right b : body doc)

checkGrammar doc (Left (Variable name value)) =
    if Map.notMember name (variables doc) then
        SampleTex (Map.insert name value $ variables doc) (imports doc) $ body doc
    else
        error $ "Duplicated Variable Declaration : " ++ name

checkGrammar doc (Left b) = SampleTex (variables doc) (imports doc) (Left b : body doc)

checkObjects :: [Either SimpleTexObject TexObject] -> Document
checkObjects x = SampleTex (variables doc) (imports doc) (reverse $ body doc)
    where doc = foldl checkGrammar blankDocument x


checkFile :: String -> Maybe ValidFile
checkFile filename
    | ext == ".tex" = Just $ Tex filename
    | ext == ".sample" = Just $ Sample filename
    | otherwise  = Nothing 
    where ext = takeExtension filename 