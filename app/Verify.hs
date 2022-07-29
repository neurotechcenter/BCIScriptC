module Verify where

import Types
import Errors
import Control.Monad.State.Lazy
import GHC.Utils.Monad (concatMapM)
import Control.Applicative (liftA, liftA2)
import GHC.Read (list)

data Result = Success | Err String deriving (Eq, Ord, Show)

data DefType = Actor | OnEvent | Event | Proc [BCVarType] | Func [BCVarType] BCVarType | State | Variable BCVarType | Any
type WithSigs a = State [Signature] a


data Signature = Signature BCIdentifier DefType
instance Eq Signature where -- Signature is equal when identifier is equal, definition type does not matter.
    (==) (Signature id _) (Signature id2 _) = id == id2


idunwrap (BCIdentifier name _) = name
sigunwrap (Signature id _) = id

getPrevDef :: BCIdentifier -> [Signature] -> BCIdentifier
getPrevDef id sigs = sigunwrap $ filter ((==) $ Signature id Any) sigs !! 0  

getSign :: BCDef -> Signature
getSign(BCActorDef actor) = Signature (getname actor) Actor where getname(BCActor name _) = name


--Verifies a program, and separates out top-level on event declarations because they are not allowed outside of actors
verify :: BCProgram -> ([Result], BCProgram)
verify (BCProgram defs) = (errsFromOnEvent defs ++ (fst $ verDefs (filter notOnEventOrEvent defs, [])), BCProgram defs)
    where errsFromOnEvent defs = map (Err <$> illegalDef) (filter isOnEventOrEvent defs); isOnEventOrEvent = (liftA2 (||) isOnEvent isEvent);notOnEventOrEvent = liftA not isOnEventOrEvent; isOnEvent (BCOnEventDef _) = True; isOnEvent (_) = False; isEvent (BCEventDef _) = True; isEvent (_) = False;



-- Verifies a set of definitions
verDefs :: ([BCDef], [Signature]) ->  ([Result], [Signature])
verDefs a = tupleApplyResult a verDef

-- verifies a single definition, returns new Results and new signatures, does not return results or signatures passed in.
-- Only verifies name declarations, does not verify the bodies of any definitions
verDef :: (BCDef, [Signature]) -> ([Result], Signature)
verDef ((BCActorDef (BCActor name defs)), sigs) =
    assertNotDefined name sigs $ --don't continue if actor name has already been defined
    tupleAddFirst (map Err (map illegalDef (filter isActorOrState defs))) -- Cannot define actors or states within an actor 
    ((fst $ verDefs (filter (liftA not isActorOrState) defs, sigs)), Signature name Actor) -- Discard returned signatures from verDefs, as they are local within the actor's namespace.
    where isActorOrState = liftA2 (||) isActorDef isStateDef; isActorDef(BCActorDef _) = True; isActorDef(_) = False; isStateDef(BCStateDef _) = True; isStateDef(_) = False

verDef ((BCOnEventDef (BCOnEvent name _)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name OnEvent)

verDef ((BCEventDef (BCEvent name)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name Event)

verDef ((BCProcDef (BCProc name (BCArgDefs args) _)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name (Proc (map getArgType args)))
    where getArgType(BCArgDef n t) = t

verDef (BCFuncDef (BCFunc name args ret)) = 
    assertNotDefined name sigs $
    ([Success], Signature name Func (map getArgType args) (getArgType ret))
    where getArgType(BCArgDef n t) = t

verSequence :: BCSequence -> [Signature] -> [Result]
verSequence (BCSequence statements) sigs =
    concatMap (flip verStatement $ sigs) statements

verStatement :: BCStatement -> [Signature] -> [Result]
verStatement statement sigs =
    [Success]



--Asserts that a name is not already defined, evals to the results and signature if not defined and an error if defined 
assertNotDefined :: BCIdentifier -> [Signature] -> ([Result], Signature) -> ([Result], Signature)
assertNotDefined name sigs val = ifElse 
    (isAlreadyDefined name sigs)
    ([Err $ alreadyDefined name (getPrevDef name sigs)], Signature name Any)
    val

assertDefined :: BCIdentifier -> [Signature] -> ([Result])
assertDefined name sigs = ifElse (isAlreadyDefined name sigs) ([Success]) ([Err $ notDefined name])

--Checks if a name already exists in the list of signatures.
isAlreadyDefined :: BCIdentifier -> [Signature] -> Bool
isAlreadyDefined name [] = False -- First Actor definition, empty Signatures list
isAlreadyDefined name s = elem (Signature name Any) s 

-- Conditional function
ifElse :: Bool -> a -> a -> a
ifElse True x _ = x
ifElse False _ y = y

-- Concatenates lists in two tuples
tupleAdd :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
tupleAdd t1 t2 = (fst t1 ++ fst t2, snd t1 ++ snd t2)

-- Adds a list to the first value of a tuple
tupleAddFirst :: [a] -> ([a], b) -> ([a], b)
tupleAddFirst a b = (a ++ fst b, snd b)

--Applies a function across a tuple
tupleApply (f, g) (a, b) = (f a, g b)

--Applies two binary functions across two tuples
tupleDoubleApply :: (a -> a -> a, b -> b -> b) -> (a, b) -> (a, b) -> (a, b)
tupleDoubleApply (f, g) (a1, b1) (a2, b2) = (f a1 a2, g b1 b2)


--Applies function to a tuple of lists, essentially folding over the first element of the tuple, while returning the first result (Results) while passing the second result ([Signature) and list tail (the rest of the definitions) to itself.
--This took like 3 days to figure out and i dont know how to explain it better.
tupleApplyResult :: ([a], [b]) -> ((a, [b]) -> ([c], b)) -> ([c], [b])
tupleApplyResult ([], b) f = ([], b)
tupleApplyResult ([x], b) f = tupleApply (id, (b ++)) (listify2nd (f (x, b)))
tupleApplyResult (x:xs, b) f = applyAndAdd xs (tupleApply (id, (b ++)) (listify2nd (f (x, b)))) (flip tupleApplyResult f)

--Applies function f to a tuple of a and b, then adds it to a tuple of c and b, for adding the results of a tuple function to a previously-computed tuple result
applyAndAdd ::  [a] -> ([c], [b]) -> (([a], [b]) -> ([c], [b])) -> ([c], [b])
applyAndAdd a (c, b) f = tupleAdd (c, b) (f (a, b)) 

-- makes the second element of a tuple into a list
listify2nd (a, b) = (a, [b])
