module Main where

import Lexer
import Node 
import Parser
import Executer

import qualified Data.Text as T

quitCommand = [":q", ":quit"]

main :: IO ()
main = do
    putStrLn "Hello"
    putStrLn "this is micro Lisp interpreter"
    loop 

loop :: IO ()
loop = do
    arg <- getLine
    putStr ">> "
    doArg arg 


doArg :: String -> IO ()
doArg arg | head arg == ':' = 
    if arg `elem` quitCommand then do 
        putStr "\n"
        putStrLn "Bye"
    else do 
        putStrLn "invalid command"
        loop
doArg [] = loop
doArg arg =   do 
        let eTokens =  lexicalAnalyze (T.pack arg)
        case eTokens of 
            Left s -> do 
                putStrLn (T.unpack s)
                loop
            Right tokens -> do 
                let eSt = genST tokens
                case eSt of 
                  Left txt -> do 
                    putStrLn (T.unpack txt)
                    loop
                  Right no -> do
                    let ast = stToAST no
                    eval ast
                    loop 

