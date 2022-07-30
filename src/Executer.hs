{-# LANGUAGE BlockArguments, OverloadedStrings #-}
module Executer (
    evalAtom,
    eval
) where
    import Lexer
    import Node 
    import Parser 

    import qualified Data.Text as T

    isNumber :: Atom -> Bool
    isNumber a = case a of
      NULL -> False
      Number n -> True 
      Operator sym -> False

    isOperator :: Atom -> Bool
    isOperator a = case a of 
      NULL -> False 
      Number n -> False 
      Operator sym -> case sym of 
        ParenthesesOpen -> False
        ParenthesesClose -> False
        _ -> True 

    evalAtom :: ST -> [Atom]
    evalAtom (Leaf x) = [x]
    evalAtom (Tree ny nz) = do 
        let y = evalAtom ny
            z = evalAtom nz
        if length y == 1 && isOperator (head y) then
            if length z == 2 && all isNumber z then do 
                let operator = case head y of
                        Operator sym -> sym
                        _ -> undefined
                    [Number n, Number m] = z
                    res =  case operator of 
                        Plus -> Number (n + m)
                        Minus -> Number (n - m)
                        Asterisk -> Number (n * m)
                        BackSlash -> Number (n `div` m)
                        _ -> undefined
                [res]
            else 
                []
        else 
            y <> z

    eval :: ST -> IO ()
    eval st = print (evalAtom st)

