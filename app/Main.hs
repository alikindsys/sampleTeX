{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.Console.CmdArgs
import System.IO
import Prettyprinter
import Prettyprinter.Render.Terminal

main :: IO ()
main = someFunc

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