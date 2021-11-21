{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.Console.CmdArgs
import System.IO
import Prettyprinter
import Prettyprinter.Render.Terminal
import Parser
import System.FilePath
import Compiler
import System.Directory
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import System.Process.Typed
import Data.Foldable
import Control.Monad ((>=>))

main :: IO ()
main = do
    it <- cmdArgs spec
    exec it

data SampleC = SampleC {files :: [FilePath], engine :: Maybe String}
  deriving (Show, Data, Typeable)

spec =
  SampleC
    { files = def &= args &= typ "FILES",
      engine = def &= help "The LaTeX engine used for PDF compilation"
    }
    &= summary "The SampleTex Compiler"
    &= details
      [ "samplec will compile to TeX by default",
        "",
        "to compile to PDF set your preferred LaTeX engine",
        "and make sure its on %PATH%"
      ]

exec :: SampleC -> IO ()
exec (SampleC [] _) = err "[samplec] No files were provided. Program exited.\n" <> info "Hint: Use --help\n"
exec (SampleC xs Nothing) = traverse_ toTex xs
exec (SampleC xs (Just x)) = traverse_ (toTex >=> toPDF x) xs  

toTex :: FilePath -> IO (Maybe FilePath)
toTex path = do
    absolutePath <- makeAbsolute path
    let (file, dir,base) = ((,,) <$> takeFileName <*> takeDirectory <*> takeBaseName) absolutePath
    let texDir = dir </> "compilation" </> "tex"
    ext <- getKind' file
    state <- runCompileT compileFile $ CompilationState {_file=file, _pwd=dir, _kind=ext}
    case state of
      Left s -> do {err "[samplec] Cannot compile " <> putStr file <> err " to LaTeX.\n" <> putStr s; pure Nothing}
      Right (x, _) -> do {
          createDirectoryIfMissing True texDir;
          writeFile (texDir </> base <> ".tex") x;
          info "[samplec] Compilation of " <> putStr file <> info " to LaTeX was successful.\n";
          pure $ Just (texDir </> base <> ".tex")
      }

toPDF :: String -> Maybe FilePath -> IO ()
toPDF engine Nothing = err "[samplec] Attemped compiling an unexisting file with " <> putStr engine <> err " LaTeX engine\n"
toPDF engine (Just tex) = do
    exec <- findExecutable engine
    let (file, dir,base) = ((,,) <$> takeFileName <*> takeDirectory <*> takeBaseName) tex
    pdfDir <- makeAbsolute $ dir </> ".." </> "pdf"
    logDir <- makeAbsolute $ dir </> ".." </> "logs"
    case exec of
      Nothing -> err "[samplec] " <> putStr engine <> err " couldn't be found. Program exited"
      Just executable -> do {
          info "[samplec] PDF Conversion of " <> putStr file <> info " with " <> putStr engine <> info " started.\n";
          createDirectoryIfMissing True pdfDir;
          prog <- readProcess $  setWorkingDir pdfDir $ proc executable [tex];
          handleThis (file,engine,logDir) prog;
      }

handleThis :: (String, String,String) -> (ExitCode, ByteString, ByteString) -> IO ()
handleThis (file, engine,_) (ExitSuccess, _, _) = done "[samplec] PDF Conversion of " <> putStr file <> done " with " <> putStr engine <> done " finished.\n"
handleThis (file, engine,logDir) (ExitFailure x, log, stderr) = do
    createDirectoryIfMissing True $ logDir </> engine
    err "[" <> putStr engine <> err "] An error occoured! See the log files under log/"<> putStr engine <> err "/\n"
    err "[" <> putStr engine <> err "] Exited with code " <> putStr (show x) <> err ".\n" 
    BL.writeFile  (logDir </> engine </> "latest-stdout.log") log
    BL.writeFile (logDir </> engine </> "latest-stderr.log") stderr

getKind :: FilePath -> Maybe PathKind
getKind ".sample" = Just SampleTex
getKind ".tex" = Just LaTeX
getKind _ = Nothing

-- | Blocking `getKind` using the IO Monad
-- | Wrote it that way since i could expand `getKind` alone and never mess with this.
getKind' :: FilePath -> IO PathKind
getKind' path = do
    let kind = getKind $ takeExtension path
    case kind of
      Nothing -> error "Unsupported file type."
      Just pk -> pure pk

-- Logging Stuff --
err = generic style stderr
    where style = color Red <> bold

warn = generic style stdout
    where style = color Yellow <> italicized

info = generic style stdout
    where style = color Cyan <> underlined

done = generic style stdout 
    where style = color Green
generic style fd text  = renderIO fd . layoutPretty defaultLayoutOptions $ annotate style text