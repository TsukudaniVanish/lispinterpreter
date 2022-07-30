{-# LANGUAGE BlockArguments, OverloadedStrings #-}
module Parser (
   genST,
    Atom(..),
    ST
)where

    import qualified Data.Text as T 
    import Data.Function 
    import Lexer
    import Node

    import Debug.Trace

    data Atom = 
       NULL |  Number Int | Operator Symbol
       deriving (Show , Read, Eq)
    type ST = Node Atom

    type ProcessedObject = (ST, [LexicalObject])

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
      NLiteral txt -> case consumeParentheses los of  
        [] -> Right $ Leaf (Number $ read $ T.unpack txt)
        _ -> do 
            str <- genST los
            Right $ Tree NULL (Leaf (Number $ read $ T.unpack txt)) str 
      ValidSymbol sym -> case sym of 
        ParenthesesOpen -> genST los
        ParenthesesClose -> genST los
        _ -> case chopBlock los of 
          Nothing -> Left "mismatch )"
          Just (block, res) -> do 
            stl <- genST block
            str <- genST res 
            trace ("block: " <> show block <> "\nres: " <> show res) $ 
                Right $ Tree (Operator sym) stl str

