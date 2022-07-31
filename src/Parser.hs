{-# LANGUAGE BlockArguments, OverloadedStrings #-}
module Parser (
    genST,
    Atom(..),
    ST,
    RuntimeBiOperator(..),
    RuntimeData(..),
    AST(..),
    stToAST,
)where

    import qualified Data.Text as T 
    import Data.Function 
    import Lexer
    import Node

    import Debug.Trace

    data RuntimeBiOperator = 
        PlusR
        | MinusR
        | MultiplicationR
        | DivisionR
        deriving (Show, Read, Eq)

    data Atom = 
       NULL |  Number Int | Operator Symbol
       deriving (Show , Read, Eq)
    type ST = Node Atom

    type ProcessedObject = (ST, [LexicalObject])

    data RuntimeData =
        NullR | NumberR Int | BiOperatorR RuntimeBiOperator |RuntimeError T.Text
        deriving (Show, Read , Eq)
    
    data AST = S RuntimeData | SBI RuntimeBiOperator [AST] | List [AST] 
      deriving (Show, Read, Eq)
    
    atomToRuntimeData :: Atom -> RuntimeData
    atomToRuntimeData a = case a of 
      NULL -> NullR 
      Number n -> NumberR n
      Operator sym -> case sym of 
        Plus -> BiOperatorR PlusR
        Minus -> BiOperatorR MinusR
        BackSlash -> BiOperatorR DivisionR
        Asterisk -> BiOperatorR MultiplicationR
        ParenthesesOpen -> RuntimeError "( is not expression"
        ParenthesesClose -> RuntimeError ") is not expression"
    
    isOperand :: AST -> Bool
    isOperand (S rd) = case rd of 
      NullR -> False
      NumberR n -> True
      BiOperatorR rbo -> False
      RuntimeError txt -> False
    isOperand (SBI _ _) = True
    isOperand (List asts) = all isOperand asts 

    -- (a1 a2 ... an) == (a1 (a2 (... an)...)))
    consumeParentheses :: [LexicalObject] -> [LexicalObject]
    consumeParentheses [] = []
    consumeParentheses (lex:lexs) = case lex of 
        ValidSymbol ParenthesesOpen -> consumeParentheses lexs 
        ValidSymbol ParenthesesClose -> consumeParentheses lexs 
        _ -> lex:lexs

    chopBlock :: [LexicalObject] -> Maybe ([LexicalObject], [LexicalObject])
    chopBlock [] = Just ([], [])
    chopBlock (lo:los) = case lo of 
      NLiteral txt -> Just ([lo], los)
      ValidSymbol sym -> case sym of 
        ParenthesesOpen -> flip fix (los, 1, 0) \loop (tokens, n, result) ->
            if n <= 0 then Just (take (result -1) los, drop result los)
            else case tokens of
              [] -> Nothing 
              lo' : los' -> case lo' of 
                ValidSymbol ParenthesesOpen -> loop (los', n + 1, result + 1)
                ValidSymbol ParenthesesClose -> loop (los', n -1, result + 1)
                _ -> loop (los', n, result + 1)
        ParenthesesClose -> Nothing
        _ -> Just ([lo], los)
    

    genST :: [LexicalObject] -> Either T.Text ST
    genST [] = Right $ Leaf NULL
    genST (lo:los) = case lo of 
    -- if number then check los contains only parentheses or not  
      NLiteral txt -> case consumeParentheses los of  
        [] -> Right $ Leaf (Number $ read $ T.unpack txt)
        _ -> do 
            str <- genST los
            Right $ Tree (Leaf (Number $ read $ T.unpack txt)) str 
    -- if symbol check this is parentheses
      ValidSymbol sym -> case sym of 
        ParenthesesOpen -> genST los
        ParenthesesClose -> genST los
        -- if this is not parentheses then check 
        _ -> do 
                right <- genST los
                Right $ Tree (Leaf $ Operator sym) right 

    stToAST :: ST -> AST
    stToAST (Leaf x) = S (atomToRuntimeData x)
    stToAST (Tree nl nr) = 
      let 
        l = stToAST nl 
        r = stToAST nr 
      in 
        case l of
          S rd -> case rd of -- left is singleton case 
            NullR -> List [l , r]
            NumberR n -> List [l, r]
            BiOperatorR rbo -> case r of -- left is bioperator and exprect List of operands in right
              S rd' -> S $ RuntimeError ("operator" <> T.pack (show rbo) <> "needs at least two operands") 
              SBI rbo' asts -> S $ RuntimeError ("operator" <> T.pack (show rbo) <> "needs at least two operands")
              List asts -> 
                if all isOperand asts && length asts >= 2 then 
                  SBI rbo asts
                else
                  S (RuntimeError "invalid operands") 
            RuntimeError txt -> S (RuntimeError txt)
          List asts -> List (asts ++ [r])
          SBI _ _ -> List [l, r]
