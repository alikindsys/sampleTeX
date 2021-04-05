module Grammar  ( Document(..)
                , ValidFile(..)
                , blankDocument
                , checkObjects
                ) where

import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath (takeExtension)
import Data.Maybe (isJust, fromJust)

data Document
    = SampleTex {variables :: Map String String, imports :: Map String ValidFile , body :: [Either SimpleTexObject TexObject]}

data ValidFile = Tex { path :: String, serialize :: String}
               | Sample{ path :: String, serialize :: String}
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

checkGrammar doc (Left (ImportStatement path)) = 
    if isJust file then
        if Map.notMember path (imports doc) then
            SampleTex (variables doc) (Map.insert path (fromJust file) $ imports doc) (Left (ImportStatement path) : body doc)
        else
            error $ "Duplicated Import Statement : " ++ path
    else
        error $ "Unsuported file type : " ++ path
    where file = checkFile path

checkGrammar doc (Left (OutStatment v)) =
    if null failure then
        SampleTex (variables doc) (imports doc) ( successes ++ body doc)
    else
        error $ "Attempted exporting undeclared variables. Variables :\n" ++ unlines failure
    where (success,failure) = checkVariables v (variables doc)
          successes = map (\x -> Right . Text $ "\\newcommand{\\" ++ x ++ "}{"++ variables doc Map.! x ++ "}") success

checkGrammar doc (Left b) = SampleTex (variables doc) (imports doc) (Left b : body doc)

checkObjects :: [Either SimpleTexObject TexObject] -> Document
checkObjects x = SampleTex (variables doc) (imports doc) (reverse $ body doc)
    where doc = foldl checkGrammar blankDocument x


checkFile :: String -> Maybe ValidFile
checkFile filename
    | ext == ".tex" = Just $ Tex filename ""
    | ext == ".sample" = Just $ Sample filename ""
    | otherwise  = Nothing 
    where ext = takeExtension filename 

checkVariables :: [String] -> Map String String -> ([String], [String])
checkVariables [] _ = ([],[])
checkVariables (var:xs) m =
    if Map.notMember var m then
        (success, var:failure)
    else
        (var:success, failure)
    where success = fst $ checkVariables xs m
          failure = snd $ checkVariables xs m 