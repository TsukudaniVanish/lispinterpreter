{-#LANGUAGE OverloadedStrings #-}
import Lib
import Lexer
import Node 
import Parser
import Executer
import Test.HUnit

main = 
    runTestTT tests

tests = TestList $ concat [testLexicalAnalyzerOneWord, testLexicalAnalyze, testGenST, testSimplifyAST]
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
            (Right $ Tree (Leaf $ Operator Plus) (Tree (Leaf $ Number 1) (Tree (Leaf $ Operator Minus) (Tree (Leaf $ Number 1) (Leaf $ Number 2)))))
    ),
    TestCase (
        assertEqual "check st to ast"
            (
                let
                    st = Tree (Leaf $ Operator Plus) (Tree (Leaf $ Number 1) (Tree (Leaf $ Operator Minus) (Tree (Leaf $ Number 1) (Leaf $ Number 2))))
                in 
                    stToAST st
            )
            (
                SBI PlusR [S (NumberR 1), SBI MinusR [S (NumberR 1), S (NumberR 2)]]
            )
    )
    ]

testSimplifyAST = 
    [
        TestCase (
            assertEqual "check simplifyAST" 
                (let 
                    ast = SBI PlusR [S (NumberR 1), SBI MinusR [S (NumberR 1), S (NumberR 2)]]
                in simplifyAST ast
                )
                (
                    S (NumberR 0)
                )
        )
    ]