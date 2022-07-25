module Errors where
import Text.Parsec (SourcePos)
import Types

--pos: position t: type n: name m: message
genericNamedError :: SourcePos -> String -> String -> String -> String
genericNamedError pos t n m = "Error at " ++ show pos ++ ", " ++ t ++ " " ++ n ++ ": " ++ m

genericError :: SourcePos -> String -> String
genericError pos m = "Error at " ++ show pos ++ ": " ++ m

actorDefInActor :: SourcePos -> String -> String
actorDefInActor pos name = genericNamedError pos "Actor" name "Cannot define an actor inside another actor"

stateDefInActor :: SourcePos -> String -> String
stateDefInActor pos name = genericNamedError pos "State" name "Cannot define a state within an actor"


errFromDefs :: BCDef -> String
errFromDefs (BCActorDef (BCActor (BCIdentifier name pos) _)) = actorDefInActor pos name 
errFromDefs (BCStateDef (BCState (BCIdentifier name pos) _)) = stateDefInActor pos name 
