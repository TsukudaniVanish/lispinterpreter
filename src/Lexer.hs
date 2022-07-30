{-#LANGUAGE OverloadedStrings #-}
module Lexer
    (
        lexicalAnalyze,
        lexicalAnalyzeOneWord,
        Symbol(..),
        LexicalObject(..)
    )where 

import Lib
import qualified Data.Text as T


data Symbol = 
    Plus -- +
    | Minus  -- -
    | BackSlash  -- /
    | Asterisk  -- *
    | ParenthesesOpen -- (
    | ParenthesesClose -- )
    deriving (Eq, Show, Read)

data LexicalObject = 
    NLiteral T.Text | ValidSymbol Symbol
    deriving (Eq, Show, Read)

isWhiteSpace :: Char -> Bool
isWhiteSpace t = t `elem` ['\t', ' ', '\n', '\v', '\f']

isPunctuator :: Char -> Bool
isPunctuator t = t `elem` ['(', ')']

isNLiteral :: T.Text -> Bool 
isNLiteral t = T.foldl (\a c -> '0' <= c && c <= '9' && a) (not $ T.null t) t

getWordEndIndex :: T.Text -> Int
getWordEndIndex t = getter t 0
    where
        getter text n | T.null text = n
                   | n >= T.length text - 1 = T.length text - 1
                   | isWhiteSpace (T.index text (n + 1)) || isPunctuator (T.index text (n + 1)) = n
                   | isPunctuator $ T.index text n = n
                   | otherwise = getter text (n + 1)

wordGetter :: T.Text -> T.Text
wordGetter t = T.take (getWordEndIndex t + 1) t

wordCutter :: T.Text -> T.Text
wordCutter t = T.drop (getWordEndIndex t + 1) t

skip :: T.Text -> T.Text
skip t | isWhiteSpace (T.head t) = skip (T.tail t)
       | otherwise = t

getSymbol :: T.Text -> Maybe Symbol
getSymbol t = case t of 
    "+" -> Just  Plus 
    "-" -> Just Minus
    "*" -> Just Asterisk
    "/" -> Just BackSlash
    "(" -> Just ParenthesesOpen
    ")" -> Just ParenthesesClose
    _ -> Nothing

lexicalAnalyzeOneWord :: T.Text -> Maybe LexicalObject
lexicalAnalyzeOneWord t | isNLiteral t = Just  (NLiteral t)
                        | otherwise = do 
                            s <- getSymbol t
                            Just (ValidSymbol s)

lexicalAnalyze :: T.Text -> Either T.Text [LexicalObject]
lexicalAnalyze t = analyzer t []
    where
        analyzer :: T.Text -> [LexicalObject] -> Either T.Text [LexicalObject]
        analyzer text list | T.null text = Right list
                           | isWhiteSpace $ T.head text = analyzer (skip text) list
                           | otherwise = 
                                case lexicalAnalyzeOneWord (wordGetter text) of
                                    Just lex -> analyzer (wordCutter text) (list <> [lex])
                                    Nothing -> Left ("fail to analyze at: " <> text)


