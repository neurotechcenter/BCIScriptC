module Types where

import Text.Parsec

-- Syntax
binaryOperators = "+-*/&|"
unaryOperators = "!"


-- Verification Types:


data Result = Success | Err String | Warn String deriving (Eq, Ord, Show)

--The identifier list on func is for the captured variables within each expression.
data DefType = Actor [Signature] | OnEvent | Event | Proc [BCVarType] | Func [BCVarType] BCVarType [BCIdentifier] | State | Variable BCVarType | Any

data Signature = Signature BCIdentifier DefType
instance Eq Signature where -- Signature is equal when identifier is equal, definition type does not matter.
    (==) (Signature id _) (Signature id2 _) = id == id2



-- AST types


data BCProgram = BCProgram [BCDef]

data BCDef = BCActorDef BCActor | BCOnEventDef BCOnEvent | BCProcDef BCProc | BCFuncDef BCFunc | BCEventDef BCEvent | BCStateDef BCState | BCVarDef BCVariable

data BCActor = BCActor BCIdentifier [BCDef]

data BCEvent = BCEvent BCIdentifier

data BCState = BCState BCIdentifier StateType

data StateType = StateBool | StateU8 | StateI8 | StateU32 | StateI32

data BCVariable = BCVariable BCIdentifier BCVarType BCLiteral

data BCFunc = BCFunc BCIdentifier BCArgDefs BCExpr

data BCProc = BCProc BCIdentifier BCArgDefs BCSequence

data BCOnEvent = BCOnEvent BCIdentifier BCSequence

data BCSequence = BCSequence [BCStatement]

data BCStatement = BCStatementCall BCCall | BCStatementControl BCControl | BCStatementAssign


-- A statement that applies a procedure or builtin function.
data BCCall = BCCall BCIdentifier [BCArg] 
data BCAssign = BCAssign BCIdentifier BCExpr
data BCControl = BCControlLoop BCRepeat | BCControlWhile BCWhile | BCControlIf BCIf | BCControlIfElse BCIfElse
data BCRepeat = BCRepeat BCExpr BCSequence
data BCWhile = BCWhile BCExpr BCSequence
data BCIf = BCIf BCExpr BCSequence
-- elif(b) {do} is syntactically equal to else { if(b) { do }}
data BCIfElse = BCIfElse BCExpr BCSequence [BCElseIf] BCSequence 
data BCElseIf = BCElseIf BCExpr BCSequence
 

-- Anything that is expected to have a value immediately, that is, a literal or a variable.
data BCValue = BCValueLiteral BCLiteral | BCValueIdentifier BCIdentifier

-- Anything that can evaluate to a value in order to be passed as a function argument
data BCArg = BCArg BCExpr

data TypedExpr = TypedExpr BCExpr BCVarType [Result]

data BCExpr = BCExprNode BCExpr BCOperator BCExpr
	    | BCExprUnaryNode BCOperator BCExpr
	    | BCExprFuncCall BCFuncCall
            | BCExprFinal BCValue

data BCFuncCall = BCFuncCall BCIdentifier [BCArg]

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
data BCArgDef = BCArgDef BCIdentifier BCVarType 

data BCArgType = BCArgType BCVarType | BCArgStringType
data BCVarType = BoolType | IntType | NumType 
instance Show BCVarType where
    show BoolType = "boolean"
    show IntType = "integer"
    show NumType = "float"
data BCLiteral = BCLiteralNumber BCNumber | BCLiteralBool BCBool
data BCNumber = BCNumberInt BCInt | BCNumberFloat BCFloat

data BCIdentifier = BCIdentifier String SourcePos
instance Eq BCIdentifier where
    (==) (BCIdentifier name _) (BCIdentifier name2 _) = name == name2
    
data BCInt = BCInt Integer
data BCFloat = BCFloat Double
data BCBool = BCBool Bool
data BCString = BCString String
