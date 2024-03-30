{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (getLine)
import Data.SCargot
import System.IO
    ( hClose,
      hSeek,
      hSetFileSize,
      hPutStr,
      openFile,
      SeekMode(AbsoluteSeek),
      IOMode(ReadWriteMode) )
import Data.Text.IO (getLine) 
import Data.Char (toLower)
import System.Process (readProcess, callCommand)
import SExpr

main :: IO ()
main = do
    expr <- getLine

    let path = "../arith/Arith/Example.lean"
    let pos = 57
    handle <- openFile path ReadWriteMode
    hSetFileSize handle pos
    hSeek handle AbsoluteSeek pos

    case readExpr expr of
        Left err -> print err >> hClose handle
        Right xs -> do 
                    hPutStr handle (map toLower (show xs))
                    hClose handle
                    callCommand "cd ../arith/; lake build; cd .lake; ./build/bin/arith"