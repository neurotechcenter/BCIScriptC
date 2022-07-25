module Verify where

import Types
import Errors
import Control.Monad.State.Lazy
import GHC.Utils.Monad (concatMapM)
import Control.Applicative (liftA, liftA2)


data Result = Success | Err String deriving (Eq, Ord, Show)

data DefType = Actor | OnEvent | Event | Proc | Func | State | Variable
type WithSigs a = State [Signature] a

data Signature = Signature String DefType

type Signatures = ([Signature],[Signature],Bool) --All definition signatures, global and local, as well as a boolean decising whether currently in local or global scope
initSigs = ([],[], False)

thd (a,b,c) = c

idunwrap (BCIdentifier name _) = name

getSign :: BCDef -> Signature
getSign(BCActorDef actor) = Signature (idunwrap $ getname actor) Actor where getname(BCActor name _) = name


verify :: BCProgram -> ([Result], BCProgram)
verify (BCProgram defs) = (evalState (verDefs defs) initSigs, BCProgram defs)

verDefs :: [BCDef] -> State Signatures [Result]
verDefs defs = concatMapM verDef defs


verDef :: BCDef -> State Signatures [Result]
verDef (BCActorDef (BCActor (BCIdentifier name _) defs)) = do
    sigs <- get
    ; put (Signature name Actor : fst sigs, snd sigs, True) --set signatures to local
    ; let isActorDef (BCActorDef _) = True; isActorDef (_) = False; isStateDef (BCStateDef _) = True; isStateDef(_) = False;
    ; let errDefs = map Err $ map errFromDefs (filter (liftA2 (||) isActorDef isStateDef) defs)
    ; let validDefs = filter (liftA not $ liftA2 (||) isActorDef isStateDef) defs 
    ; let results = foldr (:) errDefs $ verDefs validDefs 
    ; put (fst sigs, [], False) -- returning to global scope, clear local scope
    ; return results

--verDef (BCOnEventDef onEvent) = [Success]
