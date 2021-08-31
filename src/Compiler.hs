module Compiler (compile, link, reduce, replace, replaceOrReduce) where

import Grammar
import Parser
import Mappings

import Data.Map (Map)
import System.PosixCompat.Files (fileExist)
import qualified Data.Map as Map
import Data.Maybe

compile :: Document -> String 
compile doc = unlines (map toTex reduced)
    where reduced = map (replaceOrReduce doc) (body doc)

replaceOrReduce :: Document -> Either SimpleTexObject TexObject -> TexObject
replaceOrReduce doc (Left x) = reduce doc x
replaceOrReduce doc (Right x) = replace doc x

reduce :: Document -> SimpleTexObject -> TexObject
reduce doc (StringInterpolation (Identifier i)) =
    if Map.notMember i (variables doc) then
        error $ "Undeclared Variable : " ++ i
    else
        Text $ variables doc Map.! i

reduce doc (StringInterpolation (Chem c)) =
    Text $ "\\ce{"++c++"}"

reduce doc InlineListSeparator = Noop

reduce doc (ImportStatement path) =
    if Map.notMember path (imports doc) then
        error $ "Path not found on ImportTable : " ++ path ++ "\nHave you forgotten to call link?"
    else
        Text . serialize $ imports doc Map.! path

reduce doc (ComplexString xs) = Text $ concatMap (composedThing . replaceOrReduce doc) xs
  where
    composedThing = fromMaybe "" . toTex
replace :: Document -> TexObject -> TexObject
replace doc (Text s)
            | Map.member s vars = Text $ vars Map.! s
            | s == ";" = Text ""
            | otherwise = Text s
            where vars = variables doc

replace doc (List xs) = List $ map (Right <$> replaceOrReduce doc) xs 

replace doc x = x

link :: Document -> IO Document 
link doc =
    if Map.null (imports doc) then
        pure doc
    else
        do
            x <- mapM fileExist keys
            let invalidBois = filter (not . fst) (zip x keys)
            if  not $ null invalidBois then
                error $ "Linker Errror : Files doesn't exist : " ++ unlines (map snd invalidBois)
            else
                do
                    y <- mapM parseFile values
                    pure $ SampleTex (variables doc) (Map.fromAscList $ zip keys y) (body doc)
                    
    where keys = Map.keys (imports doc)
          values = Map.elems (imports doc)


parseFile :: ValidFile -> IO ValidFile 
parseFile (Tex a _) =
    do 
        contents <- readFile a
        pure $ Tex a contents

parseFile (Sample a _) = 
    do 
        contents <- readFile a
        let unsafe = parseSampleTex contents
        let safe = checkObjects unsafe
        -- This can generate an hellhole and i wont deal with dependency hell now.
        -- This is stupid C-like import. You really need to be dumb to cause that by accident.
        -- Therefore, if you end up hanging yourself and the file doesn't compile or whatever.
        -- In the words of Linus Torvalds towards Nvdia: fuck you.
        -- I'll deal with that later.
        lnk <- link safe
        let tex = compile lnk
        pure $ Tex a tex

