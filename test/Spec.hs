{-#LANGUAGE OverloadedStrings #-}
import Lib
import Parser
import Test.HUnit

main = 
    runTestTT tests

tests = TestList $ concat [testLexicalAnalyzerOneWord, testLexicalAnalyze]
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