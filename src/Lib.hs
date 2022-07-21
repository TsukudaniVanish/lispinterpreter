module Lib
    ( 
        SExpression(A, Pair),
        Atom(..),
    ) where


data Atom = 
    Null
    deriving (Eq, Show, Read)

data SExpression = A Atom | Pair SExpression SExpression
    deriving (Eq, Show, Read)


