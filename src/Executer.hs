{-# LANGUAGE BlockArguments, OverloadedStrings #-}
module Executer (
    eval,
    simplifyAST
) where
    import Lexer
    import Node 
    import Parser 

    import qualified Data.Text as T
    
    isNumberR :: RuntimeData -> Bool
    isNumberR d = case d of 
      NullR -> False 
      NumberR n -> True 
      BiOperatorR rbo -> False
      RuntimeError txt -> False

    unwrapNumberR :: RuntimeData -> Maybe Int 
    unwrapNumberR d = case d of 
        NumberR n -> Just n
        _ -> Nothing

    isNumberS :: AST -> Bool
    isNumberS (S x) = isNumberR x
    isNumberS _ = False

    unwrapNumberS :: AST -> Maybe Int
    unwrapNumberS (S d) = unwrapNumberR d
    unwrapNumberS _ = Nothing

    unwrapNumbers :: [AST] -> Maybe [Int]
    unwrapNumbers = mapM unwrapNumberS 


    simplifySBI :: RuntimeBiOperator -> [AST] -> AST 
    simplifySBI op asts = if length asts < 2 then S (RuntimeError "too few operands") else
        let 
            asts' = map simplifyAST asts
            mNumbers = unwrapNumbers asts'
        in case mNumbers of
          Nothing -> S (RuntimeError "invalid operands")
          Just ns -> case op of 
            PlusR -> S (NumberR (sum ns))
            MinusR -> S (NumberR (foldl (-) (head ns) (tail ns)))
            MultiplicationR -> S (NumberR (product ns))
            DivisionR -> S (NumberR (foldl div (head ns) (tail ns)))

    simplifyAST :: AST -> AST
    simplifyAST (S rte) = S rte
    simplifyAST (SBI op asts) = simplifySBI op asts
    simplifyAST (List asts) = List (map simplifyAST asts)

    prettyPrintAST :: AST -> T.Text
    prettyPrintAST (S d) = T.pack $ show d
    prettyPrintAST (SBI op asts) = let postfix = T.unwords (map prettyPrintAST asts) <> ")" in case op of 
      PlusR -> "(+" <> postfix
      MinusR -> "(-" <> postfix
      MultiplicationR -> "(*" <> postfix
      DivisionR -> "(/" <> postfix
    prettyPrintAST (List asts) = 
            "(" <> T.unwords (map prettyPrintAST asts) <> ")"


    eval :: AST -> IO ()
    eval ast = putStrLn $ T.unpack $ prettyPrintAST $ simplifyAST ast

