import System.Environment
import Data.List

import Text.Parsec

import Types


module Main where

main :: IO ()
main = do
    args <- getArgs

scriptFile = endBy line char ';'
line = statement <|> expression
statement = many (noneOf ";")

bcSpaces = many oneOf [' ', '\t']
bcDigit = oneOf [0 .. 9]

arguments = bcValue `sepBy` char ' '
bcValue = bcNumber <|> bcBool


bcNumber :: Parsec String st BCNumber
bcNumber = try bcFloat <|> bcInt

bcFloat :: Parsec String st BCFloat
bcFloat = do s <- getInput
	    case readSigned readFloat s of
	    [(n, s')] -> n <$ setInput s'
	    _	      -> empty

bcInt :: Parsec String st BCInt
bcInt = read $ many bcDigit

bcOperator :: Parsec String st BCOperator
bcOperator = getOperator $ oneOf "+-*/"
		where getOperator x | x == '+' = BCAdd
				    | x == '-' = BCSubtract
				    | x == '*' = BCMult
				    | x == '/' = BCDivide

bcExpression :: Parsec String st BCExpr
bcExpression = (between (char '(') (char ')') binaryExpression)
	    <|> BCFinal BCNumber

binaryExpression :: Parsec String st BCExpr
binaryExpression = TNode bcExpression bcOperator bcExpression

funcDecl = try (string "func")
procDecl = try (string "proc")

parseBCIScript :: String -> Either ParseError [[String]]
