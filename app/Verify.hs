module Verify where

import Types
import Errors
import Builtins (builtins)
import Control.Monad.State.Lazy
import GHC.Utils.Monad (concatMapM)
import Control.Applicative (liftA, liftA2)
import GHC.Read (list)
import Data.Function ((&))
import GHC.Cmm.CLabel (ConInfoTableLocation(DefinitionSite))
import GHC.Cmm (CmmNode(args))
import GHC.CmmToAsm.AArch64.Instr (x0)
import GHC.Data.BooleanFormula (BooleanFormula)
import GHC.Plugins (nameUnique)
import GHC.Tc.Types (TcIdSigInst(sig_inst_sig))
import GHC.Driver.Main (batchMsg)

--This is a large set of definitions, so i'm going to break it up into sections

--  Section: Verification functions 

verify program = program & verifyAllDeclarations & validateDefinitions


--  Section: Verification of declarations
--Verifies a program, and separates out top-level on event declarations because they are not allowed outside of actors
verifyAllDeclarations :: BCProgram -> ([Result], BCProgram)
verifyAllDeclarations (BCProgram defs) = (errsFromOnEvent defs ++ (fst $ verDecs (filter notOnEventOrEvent defs, [])), BCProgram defs)
    where errsFromOnEvent defs = map (Err <$> illegalDef) (filter isOnEventOrEvent defs); isOnEventOrEvent = (liftA2 (||) isOnEvent isEvent);notOnEventOrEvent = liftA not isOnEventOrEvent; isOnEvent (BCOnEventDef _) = True; isOnEvent (_) = False; isEvent (BCEventDef _) = True; isEvent (_) = False;


-- Verifies a set of definitions
verDecs :: ([BCDef], [Signature]) ->  ([Result], [Signature])
verDecs a = tupleApplyResult a verDec

-- Verifies a single definition, returns new Results and new signatures, does not return results or signatures passed in.
-- Also verifies expressions, because variables must not be used before 
verDec :: (BCDef, [Signature]) -> ([Result], Signature)
verDec ((BCActorDef (BCActor name defs)), sigs) =
    assertNotDefined name sigs $ --don't continue if actor name has already been defined
    tupleAddFirst (map Err (map illegalDef (filter isActorOrState defs))) -- Cannot define actors or states within an actor, ensured here so that we don't have to pass the context to verDec
    (wrapActorSigs (verDecs (filter (liftA not isActorOrState) defs, sigs))) -- Return results and the single signature of the actor, with local variables stored as part of its signature
    where isActorOrState = liftA2 (||) isActorDef isStateDef; isActorDef(BCActorDef _) = True; isActorDef(_) = False; isStateDef(BCStateDef _) = True; isStateDef(_) = False;

-- Takes the result of verifying an actor and changes the returned local definitions into part of the actor's signature, to be used later.
wrapActorSigs :: BCIdentifier -> ([Result], [Signature]) -> ([Result], Signature)
wrapActorSigs res = (fst res, Actor name snd res)

verDec ((BCOnEventDef (BCOnEvent name _)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name OnEvent)

verDec ((BCEventDef (BCEvent name)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name Event)

verDec ((BCProcDef (BCProc name (BCArgDefs args) _)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name (Proc (map getArgType args)))
    where getArgType(BCArgDef n t) = t

verDec (BCFuncDef (BCFunc name args ret expr)) = 
    assertNotDefined name sigs $
    ([Success], Signature name Func (map getArgType args) (getArgType ret) (getCapturedNames expr))
    where getArgType(BCArgDef n t) = t

verDec ((BCVarDef (BCVariable name vtype expr)), sigs) = 
    assertNotDefined name sigs $
    ([Success], Signature name (Variable vtype (getCapturedNames expr)))

verDec ((BCStateDef (BCState name stype)), sigs) = 
    assertNotDefined name sigs $
    ([Success], Signature name (Variable stype))

-- Section: Validate Definitions

validateDefinitions :: BCProgram -> [Signature] -> ([Result], BCProgram)
validateDefinitions (BCProgram defs) sigs =
    valDefs (filter isValidatable defs) sigs
    where isValidatable (BCVarDef _) = True; isValidatable (BCOnEventDef _) = True; isValidatable (BCProcDef _) = True; isValidatable (BCFuncDef _) = True; isValidatable (BCActorDef _) = True; isValidatable (_) = False;

valDefs :: [BCDef] -> [Signature] -> [Result]
valDefs defs sigs = concatMap (flip valDef sigs) defs -- Can map here because no more signatures are added when validating definitions

valDef :: BCDef -> [Signature] -> [Result]

valDef (BCActorDef (BCActor name defs)) sigs = 
    valDefs defs (sigs ++ (getActorSigs actor sigs)) -- Validate definitions including actor's local definitions
    where getActorSigs (BCActor name _) s = getSigs $ filter (eqName name) (filter isActor sigs) !! 0; eqName name (Actor aName _) = name == aName; getSigs (Actor name sigs) = sigs 

valDef (BCVarDef (BCVariable name vtype expr)) sigs =
    tupleApplyConcat (flip assertDefined sigs, flip (assertNoCircularDef name) sigs) (getCapturedNames expr)
    ++ valExpr expr

valDef (BCFuncDef (BCFunc name args ret expr)) sigs =
    concatMap valExpr (map argunwrap args) 
    ++ tupleApplyConcat (flip assertDefined sigs, flip (assertNoCircularDef name) sigs) (getCapturedNames expr)
    ++ valExpr expr

valDef (BCOnEventDef (BCOnEvent eventName sequence)) sigs = 
    assertDefined eventName
    ++ valSequence sequence

valDef (BCProcDef (BCProc name (BCArgDefs args) sequence)) sigs =
    concatMap valExpr (map argunwrap args)
    ++ valSequence sequence (sigs + )

valSequence :: BCSequence -> [Signature] -> [Result]
valSequence (BCSequence statements) sigs =
    concatMap (flip verStatement sigs) statements

valStatement :: BCStatement -> [Signature] -> [Result]
valStatement (BCStatementCall callname args) sigs = 
    assertDefined callname sigs ++
    valCall args sigs

valStatement (BCStatementControl control) sigs = valControl control sigs

valStatement (BCStatementAssign assign) sigs = valAssign assign sigs

valAssign :: BCAssign -> [Signature] -> [Result]
valAssign (BCAssign id expr) sigs = --if id is bound to a variable, ensure that the righthand expression matches its type, otherwise return error message
    runIfVar id sigs (flip valAndMatchExpr expr <*> getVarType) -- Inside the parenthesis is an expression that composes a function which takes a signature and returns [Result]
    where getVarType (Signature name (Variable var)) = var

valControl :: BCControl -> [Signature] -> [Result]
valControl (BCRepeat expr) sigs = 
    valAndMatchExpr IntType expr

valAndMatchExpr :: VarType -> BCExpr -> [Result] -- Make sure expression is valid and matches a type, otherwise return error messages.
valAndMatchExpr mtype expr = runRight (matchExprTypes mtype) (valExpr expr)

matchExprTypes :: BCIdentifier -> VarType -> TypedExpr -> [Result]
matchExprTypes id vtype (TypedExpr _ etype) = 
    if matchTypes vtype etype then [Success] else [Err $ genericError (getsourcepos id) " expected " ++ (getTypename vtype) ++ " but given " ++ (getTypename etype)]
    where getTypename (NumType) = "Number"; getTypename (BoolType) = "Boolean"; getTypename (IntType) = "Integer"

matchTypes a a = True
matchTypes NumType IntType = True
matchTypes a b = False


valExpr :: BCExpr -> Either [Result] TypedExpr

-- Validates a procedure call.
valCall :: BCIdentifier -> [BCArg] -> [Signature] -> [Result]


-- Section: Utility functions 

argsToSigs :: [BCArgDef] -> [Signature]
argsToSigs defs = map getArgSig defs where getArgSig (BCArgDef name atype) = Signature name (Variable atype)  

getCapturedNames :: BCExpr -> [BCIdentifier]
getCapturedNames (BCExprNode expr _ expr2) = getCapturedNames expr ++ getCapturedNames expr2
getCapturedNames (BCExprUnaryNode _ expr) = getCapturedNames expr
getCapturedNames (BCExprFinal (BCValueLiteral _)) = []
getCapturedNames (BCExprFinal (BCValueIdentifier name)) = name
getCapturedNames (BCExprFuncCall (BCFuncCall name args)) = name ++ (concatMap $ getCapturedArgs (map argunwrap args)) where argunwrap (BCArg expr) = expr 



argunwrap (BCArg expr) = expr
idunwrap (BCIdentifier name _) = name
sigunwrap (Signature id _) = id

getPrevDef :: BCIdentifier -> [Signature] -> BCIdentifier
getPrevDef id sigs = sigunwrap $ filter ((==) $ Signature id Any) sigs !! 0  

getSign :: BCDef -> Signature
getSign(BCActorDef actor) = Signature (getname actor) Actor where getname(BCActor name _) = name

--Asserts that a name is not already defined, evals to the results and signature if not defined and an error if defined 
assertNotDefined :: BCIdentifier -> [Signature] -> ([Result], Signature) -> ([Result], Signature)
assertNotDefined name sigs val = ifIfElse
    (isAlreadyDefined name builtins)
    ([Err $ alreadyDefinedBuiltin name], Signature name Any)
    (isAlreadyDefined name sigs)
    ([Err $ alreadyDefined name (getPrevDef name sigs)], Signature name Any)
    val

assertDefined :: BCIdentifier -> [Signature] -> [Result]
assertDefined name sigs = ifElse (isAlreadyDefined name sigs) [Success] [Err $ notDefined name]

--Applies a function to a signature if it is defined, otherwise returns an error message.
runIfDef :: BCIdentifier -> [Signature] -> (Signature -> [Result]) -> [Result]
runIfDef id sigs f = case getSignature id sigs of
    Just s -> f s
    Nothing -> Err notDefined id

--Use runIfVar instead.
assertIsVar :: BCIdentifier -> [Signature] -> [Result]
assertIsVar id sigs = if getSignature id sigs == (Signature id (Variable _)) then [Success] else [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not a variable")]

--Applies a function to a signature if it is a variable, otherwise returns an error message
runIfVar :: BCIdentifier -> [Signature] -> (Signature -> [Result]) -> [Result]
runIfVar id sigs f = case getSignature id sigs of 
    Just (Signature id (Variable vtype)) -> f $ Signature id (Variable vtype) 
    _ -> [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not a variable")]



-- Returns whether or not an identifier is called from another identifier which itself calls.
assertNoCircularDef :: BCIdentifier -> [BCIdentifier] -> [Signature] -> [Result]
assertNoCiruclarDef id1 ids sigs = map circularDef (filter (isCircular id sigs) ids)
    where isCircular id sigs id2 = id `elem` (getSignatureCapturedNames id2 sigs)

assertNoRecursion :: BCIdentifier -> BCExpr -> [Result]
assertNoRecursion id expr sigs = if id `elem` (getCapturedNames expr) then [Err $ recursion id] else [Success] 

getSignatureCapturedNames :: BCIdentifier -> [Signature] -> [BCIdentifier]
getSignatureCapturedNames id sigs = case (getSignature id sigs) of
    Just (Signature _ (Func _ _ caps)) -> caps
    _ -> []

getSignature :: BCIdentifier -> [Signature] -> Maybe Signature
getSignature id sigs = find (eqSig id) sigs


eqSig :: BCIdentifier -> Signature -> Boolean
eqSig id (Signature name _) = id == name



--Checks if a name already exists in the list of signatures.
isAlreadyDefined :: BCIdentifier -> [Signature] -> Bool
isAlreadyDefined name [] = False -- First Actor definition, empty Signatures list
isAlreadyDefined name s = elem (Signature name Any) s 

-- Conditional function
ifElse :: Bool -> a -> a -> a
ifElse True x _ = x
ifElse False _ y = y

ifIfElse :: Bool -> a -> Bool -> a -> a
ifIfElse True x _ _ = x
ifIfElse False _ True y _ = y
ifIfElse False _ False _ z = z

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

tupleApplyConcat :: (([a] -> [b]), ([a] -> [b])) -> [a] -> [b]
tupleApplyConcat (f, g) a = f a ++ g a

--Applies function to a tuple of lists, essentially folding over the first element of the tuple, while returning the first result (Results) while passing the second result ([Signature) and list tail (the rest of the definitions) to itself.
--This took like 3 days to figure out and i dont know how to explain it better.
tupleApplyResult :: ([a], [b]) -> ((a, [b]) -> ([c], b)) -> ([c], [b])
tupleApplyResult ([], b) f = ([], b)
tupleApplyResult ([x], b) f = tupleApply (id, (b ++)) (listify2nd (f (x, b)))
tupleApplyResult (x:xs, b) f = applyAndAdd xs (tupleApply (id, (b ++)) (listify2nd (f (x, b)))) (flip tupleApplyResult f)

--Applies function f to a tuple of a and b, then adds it to a tuple of c and b, for adding the results of a tuple function to a previously-computed tuple result
applyAndAdd ::  [a] -> ([c], [b]) -> (([a], [b]) -> ([c], [b])) -> ([c], [b])
applyAndAdd a (c, b) f = tupleAdd (c, b) (f (a, b)) 

--Applies a function to the right side of an either value, or returns the left side.
runRight :: (b -> a) -> Either a b -> a
runRight f (Right b) = f b
runRight f (Left a) = a

-- makes the second element of a tuple into a list
listify2nd (a, b) = (a, [b])
