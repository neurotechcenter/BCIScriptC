module Errors where
import Text.Parsec (SourcePos)
import Types
import Types (BCIdentifier(BCIdentifier))

--pos: position t: type n: name m: message
genericNamedError :: SourcePos -> String -> String -> String -> String
genericNamedError pos t n m = "Error at " ++ show pos ++ ", " ++ t ++ " " ++ n ++ ": " ++ m

genericError :: SourcePos -> String -> String
genericError pos m = "Error at " ++ show pos ++ ": " ++ m

alreadyDefined :: BCIdentifier -> BCIdentifier -> String
alreadyDefined def prev = genericNamedError (getsourcepos def) "Identifier" (getidname def) ("\"" ++ (getidname prev) ++ "\" already defined here: " ++ (show $ getsourcepos prev))   

notDefined :: BCIdentifier -> String
notDefined def = genericNamedError (getsourcepos def) "Identifier" (getidname def) "Not defined."

actorDefInActor :: SourcePos -> String -> String
actorDefInActor pos name = genericNamedError pos "Actor" name "Cannot define an actor inside another actor"

stateDefInActor :: SourcePos -> String -> String
stateDefInActor pos name = genericNamedError pos "State" name "Cannot define a state within an actor"

onEventDefOutsideActor :: SourcePos -> String -> String
onEventDefOutsideActor pos name = genericNamedError pos "OnEvent" name "Event listeners must be defined inside an actor"

eventDefOutsideActor :: SourcePos -> String -> String
onEventDefOutsideActor pos name = genericNamedError pos "Event" name "Events must be defined inside an actor"

illegalDef :: BCDef -> String
illegalDef (BCActorDef (BCActor (BCIdentifier name pos) _)) = actorDefInActor pos name 
illegalDef (BCStateDef (BCState (BCIdentifier name pos) _)) = stateDefInActor pos name 
illegalDef (BCOnEventDef (BCOnEvent (BCIdentifier name pos) _)) = onEventDefOutsideActor pos name
illegalDef (BCEventDef (BCEvent (BCIdentifier name pos))) = eventDefOutsideActor pos name

getsourcepos (BCIdentifier _ pos) = pos
getidname (BCIdentifier name _) = name
