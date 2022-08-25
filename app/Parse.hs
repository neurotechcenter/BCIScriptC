module Parse where

import Text.Parsec
import Types
import Types (BCIdentifier(BCIdentifier))
import Control.Monad

bcProgram :: Parsec String st BCProgram
bcProgram = BCProgram <$> (whitespace *> many bcDef <* eof)

bcDef :: Parsec String st BCDef
bcDef = BCActorDef   <$> bcActor 
     <|>BCOnEventDef <$> bcOnEvent
     <|>BCProcDef    <$> bcProc
     <|>BCFuncDef    <$> bcFunc
     <|>BCEventDef   <$> bcEvent
     <|>BCStateDef   <$> bcState
     <|>BCVarDef     <$> bcVar
     <?> "declaration"

bcActor :: Parsec String st BCActor
bcActor = BCActor <$> (string "actor" *> necessarySpaces *> bcIdentifier) <*> bcActorBody

bcActorBody :: Parsec String st [BCDef]
bcActorBody = (openBlock *> many bcDef <* closeBlock) 

bcOnEvent :: Parsec String st BCOnEvent
bcOnEvent = BCOnEvent <$> (string "when" *> necessarySpaces *> bcIdentifier) <*> bcBlockSequence

bcProc :: Parsec String st BCProc
bcProc = BCProc <$> (string "proc" *> necessarySpaces *> bcIdentifier) <*> parens bcArgDefs <*> bcBlockSequence

bcFunc :: Parsec String st BCFunc
bcFunc = BCFunc <$> (string "func" *> necessarySpaces *> bcIdentifier) <*> (lexeme $ char ':' *> bcArgDefs) <*> (lexeme (string "->") *> lexeme bcArgType) <*> (lexeme $ char '=' *> bcExpression <* semicolon)

bcState :: Parsec String st BCState
bcState = BCState <$> (string "state" *> necessarySpaces *> bcIdentifier) <*> (bcStateType <* semicolon)

bcEvent :: Parsec String st BCEvent
bcEvent = BCEvent <$> (string "event" *> necessarySpaces *> bcIdentifier <* semicolon)


bcStateType :: Parsec String st StateType
bcStateType = StateBool <$ (str "boolean")
           <|>StateU8   <$ (str "u8")
	   <|>StateI8   <$ (str "i8")
	   <|>StateU32  <$ (str "u32")
	   <|>StateI32  <$ (str "i32")
	   <?> "State type (boolean, u8, i8, u32, i32)"
	   where str = try . lexeme . string

bcVar :: Parsec String st BCVariable
bcVar = BCVariable <$> (string "var" *> necessarySpaces *> bcIdentifier) <*> ((lexeme $ char ':') *> bcVarType <* (lexeme $ char '=')) <*> (bcLiteral <* semicolon) 

bcVarType :: Parsec String st BCDataType
bcVarType = BoolType <$ str "boolean" <|>
            NumType <$ str "float" <|>
	    NumType <$ str "num"  <|>
	    IntType <$ str "int"
	    <?> "variable type (boolean, float, int)"
	    where str = try . lexeme . string

bcBlockSequence :: Parsec String st BCSequence
bcBlockSequence = BCSequence <$> (openBlock *> (many bcStatement) <* closeBlock)

bcStatement :: Parsec String st BCStatement
bcStatement = (BCStatementControl <$> try bcControl <*> getPosition) <|> (BCStatementCall <$> bcCall <*> getPosition) <?> "statement"

bcControl :: Parsec String st BCControl
bcControl = BCControlLoop <$> bcRepeat
	 <|>BCControlWhile <$> bcWhile
	 <|>BCControlIfElse<$> try bcIfElse
	 <|>BCControlIf <$> bcIf
	 <|>BCControlWait <$> bcWait
	 <|>bcWaitForProcess

bcWait :: Parsec String st BCWait
bcWait = BCWait <$> (lexeme (string "wait") *> (char '(' *> bcExpression <* (lexeme $ char ')') <* (lexeme $ char ';'))) 

bcWaitForProcess :: Parsec String st BCControl
bcWaitForProcess = BCControlWaitForProcess <$ (string "process" <* semicolon) 

bcRepeat :: Parsec String st BCRepeat
bcRepeat = BCRepeat <$> (string "repeat" *> necessarySpaces *> bcExpression) <*> bcBlockSequence

bcWhile :: Parsec String st BCWhile
bcWhile = BCWhile <$> (string "while" *> necessarySpaces *> bcExpression) <*> bcBlockSequence

bcIf :: Parsec String st BCIf
bcIf = BCIf <$> ((lexeme $ string "if") *> necessarySpaces *> bcExpression) <*> bcBlockSequence

bcIfElse :: Parsec String st BCIfElse
bcIfElse = BCIfElse <$> ((lexeme $ string "if") *> necessarySpaces *> bcExpression) <*> bcBlockSequence <*> (many bcElseIf) <*> ((lexeme $ string "else") *> necessarySpaces *> bcBlockSequence) 
bcElseIf :: Parsec String st BCElseIf
bcElseIf = BCElseIf <$> (string "else if" *> necessarySpaces *> bcExpression) <*> bcBlockSequence


-- A call to a procedure or function
bcCall :: Parsec String st BCCall
bcCall = BCCall <$> (bcIdentifier) <*> (parens bcArguments <* semicolon)

bcArgDefs :: Parsec String st BCArgDefs
bcArgDefs = BCArgDefs <$> sepBy bcArgDef (lexeme $ char ',')

bcArgDef :: Parsec String st BCArgDef
bcArgDef = BCArgDef <$> (bcIdentifier <* (lexeme $ char ':')) <*> bcArgType

bcArgType :: Parsec String st BCDataType
bcArgType = 	BoolType <$ lexeme (string "bool")
		<|> IntType <$ lexeme (string "int")
		<|> NumType <$ lexeme (string "float")
		<|> StringType <$ lexeme (string "string")
		<?> "argument type"

bcArguments :: Parsec String st [BCArg]
bcArguments = ((lexeme bcArg) `sepBy` (lexeme $ char ','))

bcArg :: Parsec String st BCArg
bcArg = BCArg <$> bcExpression

bcExpression :: Parsec String st BCExpr
bcExpression = try bcBinaryExpr
	    <|> try bcUnaryExpr
	    <|> BCExprFinal <$> BCValueLiteral <$> bcLiteral
	    <|> BCExprFinal <$> BCValueIdentifier <$> bcIdentifier
	    <|> BCExprFuncCall <$> bcCall
	    <?> "expression"

bcBinaryExpr :: Parsec String st BCExpr
bcBinaryExpr = BCExprNode <$> bcExpression <*> bcBinaryOperator <*> bcExpression

bcUnaryExpr :: Parsec String st BCExpr
bcUnaryExpr = BCExprUnaryNode <$> bcUnaryOperator <*> bcExpression

bcValue :: Parsec String st BCValue
bcValue = BCValueLiteral <$> bcLiteral <|> BCValueIdentifier <$> bcIdentifier

bcString :: Parsec String st BCString
bcString = BCString <$> (between (char '\"') (char '\"') (many anyChar))


openBlock = lexeme (char '{')
closeBlock = lexeme (char '}')
necessarySpaces = many1 space


bcDigit = oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']


semicolon = lexeme $ char ';'

bcLiteral :: Parsec String st BCLiteral
bcLiteral = BCLiteralNumber <$> bcNumber <|> BCLiteralBool <$> bcBool <|> BCLiteralString <$> bcString

bcNumber :: Parsec String st BCNumber
bcNumber = BCNumberFloat <$> try bcFloat <|> BCNumberInt <$> bcInt

bcFloat :: Parsec String st BCFloat
bcFloat = fmap rd $ (++) <$> (many1 digit) <*> decimal
    where rd      = read :: String -> BCFloat
          decimal = option "" $ (:) <$> char '.' <*> (many1 digit)


stringify :: [Int] -> String
--stringify = foldl ((:) . show1) "" 
stringify n = foldl (++) "" (map show n)

bcInt :: Parsec String st BCInt
bcInt = BCInt <$> (liftM read (lexeme (many bcDigit)))

bcBool :: Parsec String st BCBool
bcBool = BCBool <$> liftM read (lexeme $ string "true") <|> BCBool <$> liftM read (lexeme $ string "false")

bcIdentifier :: Parsec String st BCIdentifier
bcIdentifier = BCIdentifier <$> (lexeme $ (:) <$> letter <*> (many (letter <|> digit))) <*> getPosition

bcBinaryOperator :: Parsec String st BCOperator
bcBinaryOperator = BCOperatorBinary <$> (liftM getOperator $ lexeme $ oneOf binaryOperators) <*> getPosition
		where getOperator x | x == '+' = BCAdd
				    | x == '-' = BCSubtract
				    | x == '*' = BCMult
				    | x == '/' = BCDiv
				    | x == '&' = BCAnd
				    | x == '|' = BCOr

bcUnaryOperator :: Parsec String st BCOperator
bcUnaryOperator = BCOperatorUnary <$> (liftM getOperator $ lexeme $ oneOf unaryOperators) <*> getPosition
    where getOperator x | x == '!' = BCNot



lexeme p = p <* whitespace

parens p = between (lchar '(') (lchar ')') p

lchar = lexeme . char

whitespace = many (oneOf "\r\n\t\v\f ")

