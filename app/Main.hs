module Main where

import System.Environment
import Data.List
import Types
import Parse (bcProgram)
import Verify (verifyAllDeclarations, validateDefinitions)
import Generate (generateProgram)
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, IOMode (ReadMode))
import Control.Applicative (liftA2)
import Control.Monad (when, unless, liftM)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
    args <- getArgs;
    putStrLn "begin parse"
    case args of 
        [] -> error "Enter a filename to compile"
        [fn] -> (parseFromFile bcProgram fn) >>= printOrCompile 

printOrCompile :: Either ParseError BCProgram -> IO ()
printOrCompile = either print compileProgram

compileProgram :: BCProgram -> IO ()
compileProgram program = do
    putStrLn "parsed, verifying";
    let programverdecs = verifyAllDeclarations program;
    let verdecmessages = filter (liftA2 (||) isErr isWarn) (frs programverdecs);  
    putStrLn $ show (length $ frs programverdecs);
    mapM (putStrLn . show) verdecmessages;
    when (length (filter isErr verdecmessages) == 0) $ do
	putStrLn "verified, validating definitions";
	let programvaldefs = validateDefinitions (scd programverdecs) (thd programverdecs);
	let valdefmessages = filter (liftA2 (||) isErr isWarn) (fst programvaldefs);
	mapM (putStrLn . show) valdefmessages;
	when (length (filter isErr valdefmessages) == 0) $ do
	    putStrLn "validated, generating";
	    let finalprogram = generateProgram (snd programvaldefs);
	    putStrLn "writing to file AppInitPartial.cpp"
	    writeFile ("AppInitPartial.cpp") finalprogram;
	    putStrLn "finished."
	    return (); 



frs (a,b,c) = a;
scd (a,b,c) = b;
thd (a,b,c) = c;

retProgram :: Either ParseError BCProgram -> (IO (BCProgram) -> IO ()) -> IO ()
retProgram (Left p) f = putStrLn (show p) >> return ()
retProgram (Right p) f = f $ pure p 


isErr (Err _) = True
isErr _ = False

isWarn (Warn _) = True
isWarn _ = False
