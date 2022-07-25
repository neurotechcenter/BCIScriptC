
module Main where

import System.Environment
import Data.List
import Parse
import Verify


main :: IO ()
main = do
    args <- getArgs
    putStrLn (parse bcProgram "" "actor amogus {}")
