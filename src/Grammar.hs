module Grammar  ( Document(..)
                , blankDocument
                , checkObjects
                ) where

import Parser
import Data.Map (Map)
import qualified Data.Map as Map

data Document
    = SampleTex {variables :: Map String String, body :: [Either SimpleTexObject TexObject]}

blankDocument :: Document
blankDocument = SampleTex Map.empty []

checkGrammar :: Document -> Either SimpleTexObject TexObject -> Document

checkGrammar doc (Right b) = SampleTex (variables doc) (Right b : body doc)

checkGrammar doc (Left (Variable name value)) =
    if Map.notMember name (variables doc) then
        SampleTex (Map.insert name value $ variables doc) $ body doc
    else
        error $ "Duplicated Variable Declaration : " ++ name


checkGrammar doc (Left b) = SampleTex (variables doc) (Left b : body doc)

checkObjects :: [Either SimpleTexObject TexObject] -> Document
checkObjects x = SampleTex (variables doc) (reverse $ body doc)
    where doc = foldl checkGrammar blankDocument x
