module Types where

import Text.Parsec
import Text.Read
-- Syntax
binaryOperators = "+-*/&|"
unaryOperators = "!"


-- Verification Types:


data Result = Success | Err String | Warn String deriving (Eq, Ord, Show)

--The identifier list on func is for the captured variables within each expression.
data DefType = Actor [Signature] | OnEvent | Event | Proc [BCDataType] | Func [BCDataType] BCDataType [BCIdentifier] | State StateType | Variable BCDataType | BCDataType BCLiteral | Any

data Signature = Signature BCIdentifier DefType
instance Eq Signature where -- Signature is equal when identifier is equal, definition type does not matter.
    (==) (Signature id _) (Signature id2 _) = id == id2

-- AST Types



data BCProgram = BCProgram [BCDef]

data BCDef = BCActorDef BCActor | BCOnEventDef BCOnEvent | BCProcDef BCProc | BCFuncDef BCFunc | BCEventDef BCEvent | BCStateDef BCState | BCVarDef BCVariable

data BCActor = BCActor BCIdentifier [BCDef]

data BCEvent = BCEvent BCIdentifier

data BCState = BCState BCIdentifier StateType

data StateType = StateBool | StateU8 | StateI8 | StateU32 | StateI32

data BCVariable = BCVariable BCIdentifier BCDataType BCLiteral

data BCFunc = BCFunc BCIdentifier BCArgDefs BCDataType BCExpr

data BCProc = BCProc BCIdentifier BCArgDefs BCSequence

data BCOnEvent = BCOnEvent BCIdentifier BCSequence

data BCSequence = BCSequence [BCStatement] deriving Show

data BCStatement = BCStatementCall BCCall SourcePos | BCStatementControl BCControl SourcePos | BCStatementAssign BCAssign SourcePos deriving Show

-- A statement that applies a procedure or builtin function.
data BCCall = BCCall BCIdentifier [BCArg] deriving (Show) 
data BCAssign = BCAssign BCIdentifier BCExpr deriving Show
data BCControl = BCControlLoop BCRepeat | BCControlWhile BCWhile | BCControlIf BCIf | BCControlIfElse BCIfElse | BCControlWaitForProcess | BCControlWait BCWait deriving Show
data BCRepeat = BCRepeat BCExpr BCSequence deriving Show
data BCWait = BCWait BCExpr deriving Show
data BCWhile = BCWhile BCExpr BCSequence deriving Show
data BCIf = BCIf BCExpr BCSequence deriving Show
-- elif(b) {do} is syntactically equal to else { if(b) { do }}
data BCIfElse = BCIfElse BCExpr BCSequence [BCElseIf] BCSequence deriving Show 
data BCElseIf = BCElseIf BCExpr BCSequence deriving Show
 

-- Anything that is expected to have a value immediately, that is, a literal or a variable.
data BCValue = BCValueLiteral BCLiteral | BCValueIdentifier BCIdentifier deriving (Show)

-- Anything that can evaluate to a value in order to be passed as a function argument
data BCArg = BCArg BCExpr deriving (Show)


data TypedExpr = TypedExpr BCExpr BCDataType [Result]

data BCExpr = BCExprNode BCExpr BCOperator BCExpr
	    | BCExprUnaryNode BCOperator BCExpr
	    | BCExprFuncCall BCCall
            | BCExprFinal BCValue  deriving (Show)


data BCOperator = BCOperatorBinary BCBinaryOperator SourcePos | BCOperatorUnary BCUnaryOperator SourcePos
instance Show BCOperator where
    show (BCOperatorBinary (BCAdd) _) = "+"
    show (BCOperatorBinary (BCSubtract) _) = "-"
    show (BCOperatorBinary (BCMult) _) = "*"
    show (BCOperatorBinary (BCDiv) _) = "/"
    show (BCOperatorBinary (BCAnd) _) = "&"
    show (BCOperatorBinary (BCOr) _) = "|"
    show (BCOperatorUnary (BCNot) _) = "!"


data BCBinaryOperator = BCAdd | BCSubtract | BCMult | BCDiv | BCAnd | BCOr 

data BCUnaryOperator = BCNot

data BCArgDefs = BCArgDefs [BCArgDef]
data BCArgDef = BCArgDef BCIdentifier BCDataType 

data BCDataType = BoolType | IntType | NumType | StringType  
instance Show BCDataType where
    show BoolType = "boolean"
    show IntType = "integer"
    show NumType = "number"
    show StringType = "string"
data BCLiteral = BCLiteralNumber BCNumber | BCLiteralBool BCBool | BCLiteralString BCString deriving Show
data BCNumber = BCNumberInt BCInt | BCNumberFloat BCFloat deriving Show

data BCIdentifier = BCIdentifier String SourcePos
instance Eq BCIdentifier where
    (==) (BCIdentifier name _) (BCIdentifier name2 _) = name == name2
instance Show BCIdentifier where
    show (BCIdentifier name _) = name

data BCInt = BCInt Integer deriving (Read, Show)
data BCFloat = BCFloat Double deriving (Read, Show)
data BCBool = BCBool Bool deriving (Read, Show)
data BCString = BCString String deriving (Read, Show)
