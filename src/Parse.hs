module Parse where

import RIO

import GHC.SyntaxHighlighter

data Completion
    = Module Text
             Loc
    | ModuleExport Text
                   Text
                   Loc
    | Variable Text
               Loc
    | Extension Text
                Loc
    deriving (Eq, Show)

data Match = Match
    { match :: Text
    , type_ :: Text
    , info :: Text
    } deriving (Show)
