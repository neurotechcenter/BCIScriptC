module Verify where

import Types
import Errors
import Builtins (builtins)
import Control.Monad.State.Lazy
import GHC.Utils.Monad (concatMapM)
import Control.Applicative (liftA, liftA2)
import GHC.Read (list)
import Data.Function ((&))
import Data.List (find)
import GHC.Cmm.CLabel (ConInfoTableLocation(DefinitionSite))
import GHC.Cmm (CmmNode(args, res))
import GHC.CmmToAsm.AArch64.Instr (x0)
import GHC.Data.BooleanFormula (BooleanFormula)
import GHC.Plugins (nameUnique)
import GHC.Tc.Types (TcIdSigInst(sig_inst_sig))
import GHC.Driver.Main (batchMsg)
import Text.Parsec (SourcePos)
import Text.Parsec.Pos (newPos)

--This is a large set of definitions, so i'm going to break it up into sections

--  Section: Verification functions 




--  Section: Verification of declarations
--Verifies a program, and separates out top-level on event declarations because they are not allowed outside of actors
verifyAllDeclarations :: BCProgram -> ([Result], BCProgram, [Signature])
verifyAllDeclarations (BCProgram defs) = tupleComb (errsFromOnEvent defs ++) (verDecs (filter notOnEventOrEvent defs, [])) (BCProgram defs)
    where errsFromOnEvent defs = map (Err <$> illegalDef) (filter isOnEventOrEvent defs); isOnEventOrEvent = (liftA2 (||) isOnEvent isEvent);notOnEventOrEvent = liftA not isOnEventOrEvent; isOnEvent (BCOnEventDef _) = True; isOnEvent (_) = False; isEvent (BCEventDef _) = True; isEvent (_) = False;

tupleComb f (x, y) z = (f x, z, y)

-- Verifies a set of definitions
verDecs :: ([BCDef], [Signature]) ->  ([Result], [Signature])
verDecs a = tupleApplyResult a verDec

-- Verifies a single definition, returns new Results and new signatures, does not return results or signatures passed in.
-- Also verifies expressions, because variables must not be used before 
verDec :: (BCDef, [Signature]) -> ([Result], Signature)
verDec ((BCActorDef (BCActor name defs)), sigs) =
    assertNotDefined name sigs $ --don't continue if actor name has already been defined
    tupleAddFirst (map Err (map illegalDef (filter isActorOrState defs))) -- Cannot define actors or states within an actor, ensured here so that we don't have to pass the context to verDec
    (wrapActorSigs name (verDecs (filter (liftA not isActorOrState) defs, sigs))) -- Return results and the single signature of the actor, with local variables stored as part of its signature
    where isActorOrState = liftA2 (||) isActorDef isStateDef; isActorDef(BCActorDef _) = True; isActorDef(_) = False; isStateDef(BCStateDef _) = True; isStateDef(_) = False;


verDec ((BCOnEventDef (BCOnEvent name _)), sigs) =
    (runIfEvent name sigs (\x -> [Success]), Signature (BCIdentifier "nothing" (newPos "nowhere" 1 1)) Any)

verDec ((BCEventDef (BCEvent name)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name Event)

verDec ((BCProcDef (BCProc name (BCArgDefs args) _)), sigs) =
    assertNotDefined name sigs $
    ([Success], Signature name (Proc (map getArgType args)))
    where getArgType(BCArgDef n t) = t

verDec ((BCFuncDef (BCFunc name (BCArgDefs args) ret expr)), sigs) = 
    assertNotDefined name sigs $
    ([Success], Signature name (Func (map getArgType args) ret (getCapturedNames expr)))
    where getArgType(BCArgDef n t) = t

verDec ((BCVarDef (BCVariable name vtype expr)), sigs) = 
    assertNotDefined name sigs $
    ([Success], Signature name (Variable vtype ))

verDec ((BCStateDef (BCState name stype)), sigs) = 
    assertNotDefined name sigs $
    ([Success], Signature name (State stype))

-- Takes the result of verifying an actor and changes the returned local definitions into part of the actor's signature, to be used later.
wrapActorSigs :: BCIdentifier -> ([Result], [Signature]) -> ([Result], Signature)
wrapActorSigs id res = (fst res, Signature id (Actor $ snd res))

-- Section: Validate Definitions

validateDefinitions :: BCProgram -> [Signature] -> ([Result], BCProgram)
validateDefinitions (BCProgram defs) sigs =
    (valDefs (filter isValidatable defs) sigs, BCProgram defs)
    where isValidatable (BCVarDef _) = True; isValidatable (BCOnEventDef _) = True; isValidatable (BCProcDef _) = True; isValidatable (BCFuncDef _) = True; isValidatable (BCActorDef _) = True; isValidatable (_) = False;

valDefs :: [BCDef] -> [Signature] -> [Result]
valDefs defs sigs = concatMap (flip valDef sigs) defs -- Can map here because no more signatures are added when validating definitions

valDef :: BCDef -> [Signature] -> [Result]

valDef (BCActorDef (BCActor name defs)) sigs = 
    valDefs defs (sigs ++ (getActorSigs (BCActor name defs) sigs)) -- Validate definitions including actor's local definitions
    where getActorSigs (BCActor name _) s = getSigs $ filter (eqName name) (filter isActor sigs) !! 0; eqName aName (Signature name _) = name == aName; getSigs (Signature _ (Actor sigs)) = sigs; isActor(Signature _ (Actor _)) = True; isActor _ = False; 

valDef (BCVarDef (BCVariable name vtype val)) sigs =
    matchLiteralType name vtype val

valDef (BCFuncDef (BCFunc name args ret expr)) sigs =
    tupleApplyConcat (concatMap (flip assertDefined sigs), flip (assertNoCircularDef name) sigs) (getCapturedNames expr)
    ++ valAndMatchExpr (getsourcepos name) ret expr sigs

valDef (BCOnEventDef (BCOnEvent eventName sequence)) sigs = 
    assertDefined eventName sigs
    ++ valSequence sequence sigs

valDef (BCProcDef (BCProc name (BCArgDefs args) sequence)) sigs =
    valSequence sequence (sigs ++ (argsToSigs args))

valSequence :: BCSequence -> [Signature] -> [Result]
valSequence (BCSequence statements) sigs =
    concatMap (flip valStatement sigs) statements

valStatement :: BCStatement -> [Signature] -> [Result]
valStatement (BCStatementCall (BCCall callname args) pos) sigs = 
    runIfDef callname sigs (flip (valProcCall callname args) sigs)

valStatement (BCStatementControl control pos) sigs = valControl control pos sigs

valStatement (BCStatementAssign assign pos) sigs = valAssign assign pos sigs

valAssign :: BCAssign -> SourcePos -> [Signature] -> [Result]
valAssign (BCAssign id expr) pos sigs = --if id is bound to a variable, ensure that the righthand expression matches its type, otherwise return error message
    runIfVar id sigs (vmat pos expr sigs . getVarType) -- Inside the parenthesis is an expression that composes a function which takes a signature and returns [Result]
    where getVarType (Signature name (Variable vtype)) = vtype; vmat pos expr sigs ltype = valAndMatchExpr pos ltype expr sigs

valControl :: BCControl -> SourcePos -> [Signature] -> [Result]
valControl (BCControlLoop (BCRepeat expr seq)) pos sigs = 
    valAndMatchExpr pos IntType expr sigs ++ (valSequence seq sigs)

valControl (BCControlWaitForProcess) _ _ = [Success]

valControl (BCControlWait (BCWait expr)) pos sigs = 
    valAndMatchExpr pos NumType expr sigs;

-- Validates a procedure call. The identifier is the name of the called procedure within the statement it is being called from, and the signature is the signature of the target procedure
valProcCall :: BCIdentifier -> [BCArg] -> Signature -> [Signature] -> [Result]
valProcCall id args (Signature name (Proc argtypes)) sigs = if length args /= length argtypes then
    [Err $ differentNumberOfArguments (getsourcepos id) (idunwrap name) argtypes args]
    else zipWith' (\t e -> valAndMatchExpr (idpos id) t e sigs)  argtypes (map argunwrap args) -- make sure args match expected types
    where idpos (BCIdentifier _ pos) = pos

zipWith' :: (a -> b -> [c]) -> [a] -> [b] -> [c]
zipWith' f = go
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys) = f x y ++ go xs ys

valAndMatchExpr :: SourcePos -> BCDataType -> BCExpr -> [Signature] -> [Result] -- Make sure expression is valid and matches a type, otherwise return error messages.
valAndMatchExpr pos mtype expr sigs = runRight (matchExprTypes pos mtype) (valExpr expr sigs)

matchExprTypes :: SourcePos -> BCDataType -> (BCDataType, [Result]) -> [Result]
matchExprTypes pos vtype (etype, results) = 
    if matchTypes vtype etype then results ++ [Success] else results ++ [Err $ genericError pos " expected " ++ (getTypename vtype) ++ " but given " ++ (getTypename etype)]

matchLiteralType :: BCIdentifier -> BCDataType -> BCLiteral -> [Result]
matchLiteralType id ltype rtype = if matchTypes ltype (literalType rtype) then [Success] else [Err $ genericError (getsourcepos id) " expected " ++ (getTypename ltype) ++ " but given " ++ (getTypename $ literalType rtype)]

literalType (BCLiteralBool _) = BoolType
literalType (BCLiteralNumber (BCNumberFloat _)) = NumType
literalType (BCLiteralNumber (BCNumberInt _)) = IntType
literalType (BCLiteralString _) = StringType

getTypename (NumType) = "Number"
getTypename (BoolType) = "Boolean"
getTypename (IntType) = "Integer";
getTypename (StringType) = "String"

matchTypes IntType IntType = True
matchTypes NumType NumType = True
matchTypes BoolType BoolType = True
matchTypes StringType StringType = True
matchTypes NumType IntType = True
matchTypes a b = False

data OpType = NumOp | BoolOp

valExpr :: BCExpr -> [Signature] -> Either [Result] (BCDataType, [Result])
valExpr (BCExprNode e1 op e2) sigs = case opType op of
    NumOp -> totalNumType op (valExpr e1 sigs) (valExpr e2 sigs)
    BoolOp -> valBoolTypes op (valExpr e1 sigs) (valExpr e2 sigs) 

valExpr (BCExprUnaryNode op e1) sigs = Right $ (BoolType, valAndMatchExpr (getopsourcepos op) BoolType e1 sigs)
    where getopsourcepos (BCOperatorUnary op pos) = pos

valExpr (BCExprFuncCall call) sigs = valFuncCall call sigs 

valExpr (BCExprFinal value) sigs = valValue value sigs

opType :: BCOperator -> OpType
opType op = case op of
	   (BCOperatorBinary BCAdd _) -> NumOp
	   (BCOperatorBinary BCSubtract _) -> NumOp
	   (BCOperatorBinary BCMult _) -> NumOp
	   (BCOperatorBinary BCDiv _) -> NumOp
	   (BCOperatorBinary BCAnd _) -> BoolOp
	   (BCOperatorBinary BCOr _) -> BoolOp

type ValExpResult = Either [Result] (BCDataType, [Result]) --validates that types in an integer expression make sense
totalNumType :: BCOperator -> ValExpResult -> ValExpResult -> ValExpResult --Mixing num and int results in num
totalNumType p (Right (IntType, results1)) (Right (IntType, results2)) = Right (IntType, results1 ++ results2)
totalNumType p (Right (NumType, results1)) (Right (NumType, results2)) = Right (NumType, results1 ++ results2)
totalNumType p (Right (NumType, results1)) (Right (IntType, results2)) = Right (NumType, results1 ++ results2)
totalNumType p (Right (IntType, results1)) (Right (NumType, results2)) = Right (NumType, results1 ++ results2)
totalNumType op (Right (type1, results)) (Right (type2, results2)) = Left $ results ++ results2 ++ [Err $ genericError (getOpSourcePos op) ("Cannot apply operator " ++ (show op) ++ " to types " ++ (show type1) ++ " and " ++ (show type2) ++ ".") ]-- fails if either is a bool type
    where getOpSourcePos (BCOperatorBinary _ pos) = pos
totalNumType p (Left results) (Left results2) = Left $ results ++ results2 -- fails if any subexpression fails
totalNumType p _ (Left results) = Left results
totalNumType p (Left results) _ = Left results

valBoolTypes :: BCOperator -> ValExpResult -> ValExpResult -> ValExpResult -- validates that types in a boolean expression match
valBoolTypes op (Right (BoolType, res1)) (Right (BoolType, res2)) = Right (BoolType, res1 ++ res2)
valBoolTypes op (Right (type1, res1)) (Right (type2, res2)) = Left $ res1 ++ res2 ++ [Err $ genericError (getOpSourcePos op) ("Cannot apply operator " ++ (show op) ++ " to types " ++ (show type1) ++ " and " ++ (show type2) ++ ".")]
    where getOpSourcePos (BCOperatorBinary _ pos) = pos
valBoolTypes op (Left res1) (Left res2) = Left $ res1 ++ res2
valBoolTypes op _ (Left res) = Left res
valBoolTypes op (Left res) _ = Left res


valFuncCall :: BCCall -> [Signature] -> ValExpResult
valFuncCall (BCCall id args) sigs = 
    case getSignature id sigs of 
	Just (Signature fid (Func fargs ret _)) -> if length args /= length fargs then Left [Err $ differentNumberOfArguments (getsourcepos fid) (show id) fargs args]
	    else Right (ret, zipWith' (\l r -> valAndMatchExpr (getsourcepos id) l r sigs) fargs (map argunwrap args))
	_ -> Left [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not defined, or it is not a function")]


valValue :: BCValue -> [Signature] -> ValExpResult
valValue (BCValueLiteral (BCLiteralNumber (BCNumberInt num))) sigs = Right (IntType, [Success])
valValue (BCValueLiteral (BCLiteralNumber (BCNumberFloat num))) sigs = Right (NumType, [Success])
valValue (BCValueLiteral (BCLiteralBool b)) sigs = Right (BoolType, [Success])
valValue (BCValueLiteral (BCLiteralString s)) sigs = Right (StringType, [Success])
valValue (BCValueIdentifier id) sigs = case getSignature id sigs of
    Just (Signature vid (Variable vtype)) -> Right (vtype, [Success])
    _ -> Left [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not defined, or it is not a variable")]

-- Section: Utility functions 

argsToSigs :: [BCArgDef] -> [Signature]
argsToSigs defs = map getArgSig defs where getArgSig (BCArgDef name atype) = Signature name (Variable atype)  

getCapturedNames :: BCExpr -> [BCIdentifier]
getCapturedNames (BCExprNode expr _ expr2) = getCapturedNames expr ++ getCapturedNames expr2
getCapturedNames (BCExprUnaryNode _ expr) = getCapturedNames expr
getCapturedNames (BCExprFinal (BCValueLiteral _)) = []
getCapturedNames (BCExprFinal (BCValueIdentifier name)) = [name]
getCapturedNames (BCExprFuncCall (BCCall name args)) = [name] ++ (concatMap getCapturedNames (map argunwrap args)) where argunwrap (BCArg expr) = expr 



argunwrap (BCArg expr) = expr
idunwrap (BCIdentifier name _) = name
sigunwrap (Signature id _) = id

getPrevDef :: BCIdentifier -> [Signature] -> BCIdentifier
getPrevDef id sigs = sigunwrap $ filter ((==) $ Signature id Any) sigs !! 0  

getSign :: BCDef -> Signature
getSign(BCActorDef actor) = Signature (getname actor) (Actor []) where getname(BCActor name _) = name

--Asserts that a name is not already defined, evals to the results and signature if not defined and an error if defined 
assertNotDefined :: BCIdentifier -> [Signature] -> ([Result], Signature) -> ([Result], Signature)
assertNotDefined name sigs val = ifIfElse
    (isAlreadyDefined name builtins)
    ([Err $ alreadyDefinedBuiltin name], Signature name Any)
    (isAlreadyDefined name sigs)
    ([Err $ alreadyDefined name (getPrevDef name sigs)], Signature name Any)
    val

assertDefined :: BCIdentifier -> [Signature] -> [Result]
assertDefined name sigs = ifElse (isAlreadyDefined name (sigs ++ builtins)) [Success] [Err $ notDefined name]

--Applies a function to a signature if it is defined, otherwise returns an error message.
runIfDef :: BCIdentifier -> [Signature] -> (Signature -> [Result]) -> [Result]
runIfDef id sigs f = case getSignature id sigs of
    Just s -> f s
    Nothing -> [Err $ notDefined id]

--Use runIfVar instead.
assertIsVar :: BCIdentifier -> [Signature] -> [Result]
assertIsVar id sigs = if isVariable (getSignature id sigs) then [Success] else [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not a variable")] 
    where isVariable (Just (Signature _ (Variable _))) = True; isVariable _ = False;

--Applies a function to a signature if it is a variable, otherwise returns an error message
runIfVar :: BCIdentifier -> [Signature] -> (Signature -> [Result]) -> [Result]
runIfVar id sigs f = case getSignature id sigs of 
    Just (Signature id (Variable vtype)) -> f $ Signature id (Variable vtype) 
    _ -> [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not defined, or it is not a variable")]

runIfEvent :: BCIdentifier -> [Signature] -> (Signature -> [Result]) -> [Result]
runIfEvent id sigs f = case getSignature id sigs of 
    Just (Signature id Event) -> f $ Signature id Event 
    _ -> [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not defined, or it is not an event")]

runIfProc :: BCIdentifier -> [Signature] -> (Signature -> [Result]) -> [Result]
runIfProc id sigs f = case getSignature id sigs of
    Just (Signature id (Proc args)) -> f $ Signature id (Proc args)
    _ -> [Err $ genericError (getsourcepos id) ((idunwrap id) ++ " is not defined, or it is not a procedure")]



-- Returns whether or not an identifier is called from another identifier which itself calls.
assertNoCircularDef :: BCIdentifier -> [BCIdentifier] -> [Signature] -> [Result]
assertNoCircularDef id1 ids sigs = map (Err . (circularDef id1)) (filter (isCircular id1 sigs) ids)
    where isCircular id sigs id2 = id `elem` (getSignatureCapturedNames id2 sigs)

assertNoRecursion :: BCIdentifier -> BCExpr -> [Result]
assertNoRecursion id expr = if id `elem` (getCapturedNames expr) then [Err $ recursiveExpression id] else [Success] 

getSignatureCapturedNames :: BCIdentifier -> [Signature] -> [BCIdentifier]
getSignatureCapturedNames id sigs = case (getSignature id sigs) of
    Just (Signature _ (Func _ _ caps)) -> caps
    _ -> []

getSignature :: BCIdentifier -> [Signature] -> Maybe Signature
getSignature id sigs = find (eqSig id) $ sigs ++ builtins


eqSig :: BCIdentifier -> Signature -> Bool
eqSig id (Signature name _) = id == name



--Checks if a name already exists in the list of signatures.
isAlreadyDefined :: BCIdentifier -> [Signature] -> Bool
isAlreadyDefined name [] = False -- First Actor definition, empty Signatures list
isAlreadyDefined name s = elem (Signature name Any) s 

-- Conditional function
ifElse :: Bool -> a -> a -> a
ifElse True x _ = x
ifElse False _ y = y

ifIfElse :: Bool -> a -> Bool -> a -> a -> a
ifIfElse True x _ _ _ = x
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
