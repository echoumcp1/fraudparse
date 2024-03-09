{-# LANGUAGE OverloadedStrings #-}
module Main where
import Parse
import Prelude hiding (getLine)
import Data.SCargot
import System.IO hiding (getLine)
import Data.Text.IO (getLine) 
import Data.Char (toLower)
import System.Process (readProcess, callCommand)

main :: IO ()
main = do
    expr <- getLine

    let path = "../arith/Arith/Example.lean"
    let pos = 57
    handle <- openFile path ReadWriteMode
    hSetFileSize handle pos
    hSeek handle AbsoluteSeek pos
    
    case decode myLangParser expr of
        Left _ -> print "parse error" >> hClose handle
        Right xs -> do 
                    hPutStr handle (map toLower (show $ head xs))
                    hClose handle
                    callCommand "cd ../arith/; lake build; cd .lake; ./build/bin/arith"