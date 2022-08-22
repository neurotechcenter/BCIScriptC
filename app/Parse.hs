module Parse where

import Text.Parsec
import Types
import Types (BCIdentifier(BCIdentifier))
import Text.Parsec.Token (GenTokenParser(lexeme))


bcProgram :: Parsec String st BCProgram
bcProgram = BCProgram <$> many bcDef

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
bcFunc = BCFunc <$> (string "func" *> necessarySpaces *> bcIdentifier) <*> (lexeme $ char ':' *> bcArgDefs) <*> (lexeme $ string "->" *> bcArgType) <*> (lexeme $ char "=" *> bcExpression <* semicolon)

bcState :: Parsec String st BCState
bcState = BCState <$> (string "state" <*> necessarySpaces *> bcIdentifier) <*> (bcStateType <* semicolon)

bcEvent :: Parsec String st BCEvent
bcEvent = BCEvent <$> (string "event" <*> necessarySpaces *> bcIdentifier <* semicolon)


bcStateType :: Parsec String st StateType
bcStateType = StateBool <$ str "boolean"
           <|>StateU8   <$ str "u8"
	   <|>StateI8   <$ str "i8"
	   <|>StateU32  <$ str "u32"
	   <|>StateI32  <$ str "i32"
	   <?> "State type (boolean, u8, i8, u32, i32)"
	   where str = try $ lexeme string

bcVar :: Parsec String st BCVariable
bcVar = BCVariable <$> (string "var" <*> necessarySpaces *> bcIdentifier) <*> (lexeme $ char ':' *> bcVarType) <*> bcValue

bcVarType :: Parsec String st BCVariable
bcVarType = BoolType <$ str "boolean" <|>
            NumType <$ str "float" <|>
	    NumType <$ str "num"  <|>
	    IntType <$ str "int"
	    <?> "variable type (boolean, float, int)"
	    where str = try $ lexeme string

bcBlockSequence :: Parsec String st BCSequence
bcBlockSequence = BCSequence <$> (openBlock *> (many bcStatement) <* closeBlock)

bcStatement :: Parsec String st BCStatement
bcStatement = BCStatementControl <$> try bcControl <|> (BCStatementCall <$> bcCall) <?> "statement"

bcControl :: Parsec String st BCControl
bcControl = BCControlLoop <$> bcRepeat
	 <|>BCControlWhile <$> bcWhile
	 <|>BCControlIfElse<$> try bcIfElse
	 <|>BCControlIf <$> bcIf

bcRepeat :: Parsec String st BCControl
bcRepeat = BCLoop <$> (string "repeat" *> necessarySpaces *> bcExpression) <*> bcBlockSequence

bcWhile :: Parsec String st BCWhile
bcWhile = BCWhile <$> (string "while" *> necessarySpaces *> bcExpression) <*> bcBlockSequence

bcIf :: Parsec String st BCIf
bcIf = BCIf <$> ((lexeme $ string "if") *> necessarySpaces *> bcExpression) <*> bcBlockSequence

bcIfElse :: Parsec String st BCIfElse
bcIfElse = BCIfElse <$> ((lexeme $ string "if") *> necessarySpaces *> bcExpression) <*> bcBlockSequence <*> (many bcElseIf) <*> ((lexeme $ string "else") *> necessarySpaces *> bcBlockSequence) 
bcElseIf :: Parsec String st BCElseIf
bcElseIf = BCElseIf <$> (string "else if" *> necessarySpaces *> bcExpression) <*> bcBlockSequence


-- A call to a procedure or function
bcCall :: Parsec String st BCStatement
bcCall = BCStatementCall <$> (bcIdentifier <* necessarySpaces) <*> (parens bcArguments <* semicolon)

bcArgDefs :: Parsec String st BCArgDefs
bcArgDefs = BCArgDefs <$> sepBy bcArgDef (lexeme $ char ',')

bcArgDef :: Parsec String st BCArgDef
bcArgDef = BCArgDef <$> (bcIdentifier <* (lexeme $ char ':')) <*> bcArgType

bcArgType :: Parsec String st BCVarType
bcArgType = 		   BoolType <$ lexeme (string "bool")
			<|> IntType <$ lexeme (string "int")
			<|> NumType <$ lexeme (string "float")
			<?> "argument type"

bcArguments :: Parsec String st [BCArg]
bcArguments = (:) <$> ((lexeme bcArg) `sepBy` (lexeme $ char ','))

bcArg :: Parsec String st BCArg
bcArg = BCArgExpr <$> bcExpr <|> (BCArgStr <$> bcString)

bcExpression :: Parsec String st BCExpr
bcExpression = BCExprNode <$> try (bcExpression <*> bcOperator <*> bcExpression)
	    <|> BCExprUnaryNode <$> try (bcUnaryOperator <*> bcExpression) --Unary operation, only for boolean inverse
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


semicolon = lexeme $ char ';'

bcLiteral :: Parsec String st BCLiteral
bcLiteral = BCLiteralNumber <$> bcNumber <|> BCLiteralBool <$> bcBool

bcNumber :: Parsec String st BCNumber
bcNumber = BCNumberFloat <$> try bcFloat <|> BCNumberInt <$> bcInt

bcFloat :: Parsec String st BCFloat
bcFloat = BCFloat <$> readFloat $ lexeme (many bcDigit <*> char '.' <*> many bcDigit)

bcInt :: Parsec String st BCInt
bcInt = BCInt <$> read $ lexeme $ many bcDigit

bcBool :: Parsec String st BCBool
bcBool = BCBool <$> read (lexeme $ string "true") <|> BCBool <$> read (lexeme $ string "false")

bcIdentifier :: Parsec String st BCIdentifier
bcIdentifier = BCIdentifier <$> (lexeme $ (letter <*> (many $ letter) <|> digit)) <*> getPosition

bcBinaryOperator :: Parsec String st BCOperator
bcBinaryOperator = getOperator $ lexeme $ oneOf binaryOperators
		where getOperator x | x == '+' = getOp BCAdd
				    | x == '-' = getOp BCSubtract
				    | x == '*' = getOp BCMult
				    | x == '/' = getOp BCDivide
				    | x == '&' = getOp BCAnd
				    | x == '|' = getOp BCOr
	      ; getOp ctor = BCOperatorBinary ctor getPosition

bcUnaryOperator :: Parsec String st BCOperator
bcUnaryOperator = getOperator <$> (lexeme (oneOf unaryOperators))
    where getOperator x | x == '!' =  (BCOperatorUnary BCNot getPosition)
