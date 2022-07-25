module Types where

import Text.Parsec

data BCProgram = BCProgram [BCDef]

data BCDef = BCActorDef BCActor | BCOnEventDef BCOnEvent | BCProcDef BCProc | BCFuncDef BCFunc | BCEventDef BCEvent | BCStateDef BCState | BCVarDef BCVariable

data BCActor = BCActor BCIdentifier [BCDef]

data BCEvent = BCEvent String

data BCState = BCState BCIdentifier StateType

data StateType = StateBool | StateU8 | StateI8 | StateU32 | StateI32

data BCVariable = BCVariable BCIdentifier BCVarType BCExpr

data BCFunc = BCFunc BCIdentifier BCArgDefs BCExpr

data BCProc = BCProc BCIdentifier BCArgDefs BCSequence

data BCOnEvent = BCOnEvent BCIdentifier BCSequence

data BCSequence = BCSequence [BCStatement]

data BCStatement = BCStatementCall BCCall | BCStatementControl BCControl


-- A statement that applies a procedure or builtin function.
data BCCall = BCCall BCIdentifier [BCArg] 

data BCControl = BCControlLoop BCRepeat | BCControlWhile BCWhile | BCControlIf BCIf | BCControlIfElse BCIfElse
data BCRepeat = BCRepeat BCExpr BCSequence
data BCWhile = BCWhile BCExpr BCSequence
data BCIf = BCIf BCExpr BCSequence
-- elif(b) {do} is syntactically equal to else { if(b) { do }}
data BCIfElse = BCIfElse BCExpr BCSequence [BCElseIf] BCSequence 
data BCElseIf = BCExpr BCSequence
 

-- Anything that is expected to have a value immediately, that is, a literal or a variable.
data BCValue = BCValueLiteral BCLiteral | BCValueIdentifier BCIdentifier

-- Anything that can evaluate to a value in order to be passed as a function argument
data BCArg = BCArgExpr BCExpr | BCArgStr BCString

data BCExpr = BCExprNode BCExpr BCOperator BCExpr
            | BCExprFinal BCValue
	    | BCExprFuncCall BCStatement

data BCOperator = BCAdd | BCSubtract | BCMult | BCDiv | BCAnd | BCOr | BCNot

data BCArgDefs = BCArgDefs [BCArgDef]
data BCArgDef = BCArgDef BCIdentifier BCArgType 

data BCArgType = BCArgVarType BCVarType | BCArgStringType
data BCVarType = BoolType | IntType | NumType

data BCLiteral = BCLiteralNumber BCNumber | BCLiteralBool BCBool
data BCNumber = BCNumberInt BCInt | BCNumberFloat BCFloat

data BCIdentifier = BCIdentifier String SourcePos
data BCInt = BCInt Integer
data BCFloat = BCDouble Double
data BCBool = BCBool Bool
data BCString = BCString String
