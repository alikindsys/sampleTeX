module Main where

import System.Environment
import System.Directory
import System.FilePath.Posix

import Parser
import Grammar
import Compiler


main :: IO ()

main = do
    args <- getArgs 
    if null args then
        putStrLn "Please input at least one file."
    else
        mapM_ stxToTex args


stxToTex :: FilePath -> IO()
stxToTex path = do
    putStrLn $ "["++ path ++"] Compiling " ++ takeFileName path ++ "..."
    contents <- readFile path
    let unsafe = parseSampleTex contents
    let safe = checkObjects unsafe
    let tex = compile safe
    createAndWriteFile ("./compiled/" ++ takeBaseName path ++ ".tex") tex
    putStrLn $ "Finished compiling " ++ takeBaseName path


createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path

  writeFile path content