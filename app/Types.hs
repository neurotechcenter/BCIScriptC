type BCInt = Integer
type BCFloat = Double
type BCBool = Bool
type BCString = StringPos
Text.Parsec.Prim

type BCNumber = BCInt | BCFloat
type BCVar = BCNumber | BCBool


type BCOperator = BCAdd | BCSubtract | BCMult | BCDiv deriving (Eq, Ord, Show)

type BCExpr = BCNode (BCExpr) BCOperator (BCExpr)
	    | BCFinal BCNumber

type BCAnything = BCStatement | BCDirective
