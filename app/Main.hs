{-# LANGUAGE OverloadedStrings #-}
module Main where
import Parse ( parser )
import Prelude hiding (getLine)
import Data.SCargot
import System.IO
    ( hClose,
      hSeek,
      hSetFileSize,
      hPutStr,
      openFile,
      SeekMode(AbsoluteSeek),
      IOMode(AppendMode), stderr )
import Data.Text.IO (getLine) 
import Data.Char (toLower)
import System.Process (readProcess, callCommand)

main :: IO ()
main = do
    expr <- getLine
    case decode parser expr of
        Left err -> hPutStr stderr err
        Right val -> putStrLn (map toLower (show $ head val))
    
    -- let path = "../arith/Arith/Example.lean"
    -- let pos = 57
    -- handle <- openFile path AppendMode
    -- hSetFileSize handle pos

    -- case decode parser expr of
    --     Left err -> print err >> hClose handle
    --     Right xs -> do 
    --             hPutStr handle (map toLower (show $ head xs))
    --             hClose handle
    --             callCommand "cd ../arith/; lake build; cd .lake; ./build/bin/arith"


    