module Parser(
    TexObject(..)
) where

data TexObject = Section String
               | NewPage
               | SimpleText String
               | List [TexObject]

data SimpleTexObject = Variable { name :: String , value :: String }
                     | StringInterpolation SimpleTexFunction
                     | ComplexString [Either TexObject SimpleTexObject]

data SimpleTexFunction = Chem String 
                       | Identifier String 

