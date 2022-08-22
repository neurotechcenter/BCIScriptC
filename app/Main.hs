module Main where

import System.Environment
import Data.List
import Types
import Parse (bcProgram)
import Verify (verifyAllDeclarations, validateDefinitions)
import Generate (generateProgram)


main :: IO ()
main = do
    args <- getArgs;
    program <- parse bcProgram "" "";
    programverdecs <- verifyAllDeclarations program;
    verdecmessages <- filter (liftA2 (||) isErr isWarn) programverdecs;  
    map putStrLn verdecmessages;
    when (length (filter isErr verdecmessages) == 0) $ do
	map putStrLn verdecmessages;
	programvaldefs <- validateDefinitions program;
	valdefmessages <- filter (liftA2 (||) isErr isWarn) programvaldefs;
	map putStrLn valdefmessages;
	when (length (filter isErr valdefmessages) == 0) $ do
	    finalprogram <- generateProgram program;
	    --writeFile (args !! 1 ++ "/src/custom/BCIEvent/src/AppInitPartial.cpp") finalprogram
	    writeFile ("AppInitPartial.cpp") finalprogram
	    return (); 

isErr (Err _) = True
isErr _ = False

isWarn (Warn _) = True
isWarn _ = False
