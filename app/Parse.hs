module Parse where

import Text.Parsec
import Types


bcProgram :: Parsec String st BCProgram
bcProgram = BCProgram <$> many bcDef

bcDef :: Parsec String st BCDef
bcDef = BCActorDef   <$> bcActor 
     <|>BCOnEventDef <$> bcOnEvent
     <|>BCProcDef    <$> bcProc
     <|>BCFuncDef    <$> bcFunc
     <|>BCEventDef   <$> bcEvent
     <|>BCStateDef   <$> bcState
     <?> "declaration"

bcActor :: Parsec String st BCActor
bcActor = BCActor <$> (string "actor" necessarySpaces) *> (lexeme bcIdentifier) <*> bcActorBody

bcActorBody :: Parsec String st [BCDef]
bcActorBody = (:) <$> ((openBlock *> many bcDef) <* closeBlock) 

bcOnEvent :: Parsec String st BCOnEvent
bcOnEvent = BCOnEvent <$> (string "when") <* necessarySpaces *> bcIdentifier <*> bcBlockSequence

bcProc :: Parsec String st BCProc
bcProc = BCProc <$> (string "proc") <* necessarySpaces *> bcIdentifier <* (lexeme $ char ':') *> bcArgDefs <*> bcBlockSequence

bcFunc :: Parsec String st BCFunc
bcFunc = BCFunc <$> (string "func") <* necessarySpaces *> bcIdentifier <* (lexeme $ char ':') *> bcArgDefs <* (lexeme $ string "->") *> bcArgType <* (lexeme $ char "=") *> bcExpression <* (lexeme $ char ';')

bcBlockSequence :: Parsec String st BCSequence
bcBlockSequence = BCSequence <$> (openBlock *> (many bcStatement)) <* closeBlock


-- A call to a procedure or function
bcStatement :: Parsec String st BCStatement
bcStatement = BCStatement <$> (bcIdentifier <* necessarySpaces) <*> (between (lexeme $ char '(') (lexeme $ char ')') bcArguments) <* (lexeme $ char ';')

bcArgDefs :: Parsec String st BCArgDefs
bcArgDefs = BCArgDefs <$> sepBy bcArgDef (lexeme $ char ',')

bcArgDef :: Parsec String st BCArgDef
bcArgDef = BCArgDef <$> bcIdentifier <* (lexeme $ char ':') *> bcArgType

bcArgType :: Parsec String st BCArgType
bcArgType = 		   BoolType <$ lexeme $ string "bool"
			<|> IntType <$ lexeme $ string "int"
			<|> NumType <$ lexeme $ string "float"
			<|> string  <$ lexeme $ string "string"
			<?> "argument type"

bcArguments :: Parsec String st [BCArg]
bcArguments = (:) <$> (lexeme bcArg) `sepBy` (lexeme $ char ',')

bcArg :: Parsec String st BCArg
bcArg = BCArgExpr <$> bcExpr <|> (BCArgStr <$> bcString)

bcExpression :: Parsec String st BCExpr
bcExpression = BCExprNode <$> bcExpression <*> bcOperator <*> bcExpression
	    <|> BCExprFinal <$> bcLiteral
	    <|> BCExprFinal <$> bcIdentifier
	    <|> BCExprFuncCall <$> bcStatement
	    <?> "expression"

bcValue :: Parsec String st BCValue
bcValue = BCValueLiteral <$> bcLiteral <|> BCValueIdentifier <$> bcIdentifier

bcString :: Parsec String st BCString
bcString = between (char "\"") (char "\"") anyChar

openBlock = lexeme (char '{')
closeBlock = lexeme (char '}')
necessarySpaces = many1 space


bcDigit = oneOf [0 .. 9]


bcLiteral :: Parsec String st BCLiteral
bcLiteral = BCLiteral <$> bcNumber <|> bcBool

bcNumber :: Parsec String st BCNumber
bcNumber = BCNumber <$> try bcFloat <|> bcInt

bcFloat :: Parsec String st BCFloat
bcFloat = BCFloat <$> readFloat $ many bcDigit <*> char '.' <*> many bcDigit

bcInt :: Parsec String st BCInt
bcInt = BCInt <$> read $ many bcDigit

bcBool :: Parsec String st BCBool
bcBool = BCBool <$> read $ (string "true")<|> (string "false")

bcIdentifier :: Parsec String st BCIdentifier
bcIdentifier = BCIdentifier <$> lexeme $ letter <*> many $ letter <|> digit

bcOperator :: Parsec String st BCOperator
bcOperator = getOperator $ oneOf "+-*/"
		where getOperator x | x == '+' = BCAdd
				    | x == '-' = BCSubtract
				    | x == '*' = BCMult
				    | x == '/' = BCDivide


