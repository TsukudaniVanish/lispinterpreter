{-#LANGUAGE OverloadedStrings #-}
import Lib
import Lexer
import Node 
import Parser
import Test.HUnit

main = 
    runTestTT tests

tests = TestList $ concat [testLexicalAnalyzerOneWord, testLexicalAnalyze, testGenST]
testLexicalAnalyzerOneWord = [
    TestCase (assertEqual "for lexicalAnalyzeOneWord" (lexicalAnalyzeOneWord "+") (Just $ ValidSymbol Plus)),
    TestCase (assertEqual "for lexicalAnalyzeOneWord" (lexicalAnalyzeOneWord "FAIL") Nothing)
    ]

testLexicalAnalyze = [  
    TestCase (
        assertEqual "for lexicalAnalyzer" 
            (lexicalAnalyze "(+ 1 2)") 
            (Right [ValidSymbol ParenthesesOpen, ValidSymbol Plus, NLiteral "1", NLiteral "2", ValidSymbol ParenthesesClose])
    ),
    TestCase ( -- 失敗ケース
        assertBool "for lexicalAnalyzer" 
            (case lexicalAnalyze "あいうて )" of
                Right t -> False
                Left s -> True
            ))

    ]

testGenST = [
    TestCase (
        assertEqual "check parser works"
            (
                let -- (+ 1 (- 1 2))
                    arg = [ValidSymbol ParenthesesOpen, ValidSymbol Plus, NLiteral "1", ValidSymbol ParenthesesOpen, ValidSymbol Minus, NLiteral "1", NLiteral "2", ValidSymbol ParenthesesClose, ValidSymbol ParenthesesClose] 
                in genST arg
            )
            (Right $ Tree (Operator Plus) (Leaf $ Number 1) (Tree (Operator Minus) (Leaf $ Number 1) (Leaf $ Number 2)))
    )
    ]