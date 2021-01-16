module Parser(
    TexObject(..)
) where

data TexObject = Section String
               | NewPage
               | SimpleText String
               | List [TexObject]

