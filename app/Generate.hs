module Generate where
import Types
import Text.Parsec
import Builtins (builtinSub)
import Data.List
import Control.Monad
import Data.Char (toLower)
import Types (BCStatement(BCStatementControl), BCControl (BCControlIf, BCControlLoop), BCIfElse (BCIfElse))

generateProgram :: BCProgram -> String
generateProgram (BCProgram defs) = 
    "#include \"BCIEvent.hpp\"" +>
    "\n\n" ++
    "void BCIEvent::BCIEventApplication::InitBCIEvent(){" +>
    (foldr (+>) "" $ map indent $ map ((++ "\n") . genDef) defs) +> --the folds add newline characters to the end of each definition
    "}"

genDef :: BCDef -> String 
genDef (BCActorDef (BCActor name defs)) =
    "Actor *" ++ (show name) ++ " = makeActor()" +>
    (foldr (+>) "" $ map indent (actorify (map genDef defs))) +>
    "addActor(std::unique_ptr<Actor>(" ++ (show name) ++ "))"

genDef (BCOnEventDef (BCOnEvent id (BCSequence sequence))) =
    "addEventListener(SequenceBuilder())" +>
    (foldr (+>) "" (map genStatement sequence)) +>
    "getSequence());"

genDef (BCEventDef (BCEvent id)) = 
    "addEvent(\"" ++ (show id) ++ "\"" 

genDef (BCStateDef (BCState id stype)) = 
    "addState(\"" ++ (show id) ++ "\", " ++ (getSType stype) ++ ")"

genDef (BCVarDef (BCVariable id vtype val)) = 
    "addVariable(std::move(std::make_unique<" ++ (getVType vtype) ++ ">(\"" ++ (show id) ++ "\", " ++ (show $ evalLit val) ++ ")))"

genDef _ = "" --Not implemented yet

genStatement :: BCStatement -> String
genStatement (BCStatementCall (BCCall id args) _) = 
    case liftMFirstArg subBuiltIn (liftM snd (find (eqFst $ show id) builtinSub)) args of --Only checks builtins for now, because procedures aren't implemented yet
	Just x -> x
	Nothing -> ""

genStatement (BCStatementControl (BCControlIf (BCIf expr seq)) _) = 
    ".addIfBlock(" ++ (exprLambda expr) ++ ")" +>
    (genSequence seq) +>
    ".closeStatement()"

genStatement (BCStatementControl (BCControlWaitForProcess) _ )=
    ".addWaitForProcessBlock()"

genStatement (BCStatementControl (BCControlWait (BCWait expr)) _ ) = 
    ".addTimerBlock(" ++ (exprLambda expr) ++ ")"

genStatement (BCStatementControl (BCControlIfElse (BCIfElse expr seq elifs elseq)) _) = 
    ".addIfElseBlock(" ++ (exprLambda expr) ++ ")" +>
    (genSequence seq) +>
    ".closeStatement()" +>
    (foldl' (+>) "" (map genIfElse elifs)) +>
    (genSequence elseq) +>
    ".closeStatement()"
    where genIfElse (BCElseIf ex seq) = genSequence seq +> "closeStatement()"  

genStatement (BCStatementControl (BCControlLoop (BCRepeat expr seq)) _) = 
    ".addLoopBlock(" ++ (exprLambda expr) ++ ")" +>
    (genSequence seq) +>
    ".closeStatement()"

genStatement (BCStatementControl (BCControlWhile (BCWhile expr seq)) _) =
    ".addWhileLoopBlock(" ++ (exprLambda expr) ++ ")" +>
    (genSequence seq) +>
    ".closeStatement()"


exprLambda :: BCExpr -> String
exprLambda e = "[](Actor& callingActor){ return " ++ (genExpr e) ++ ";}"  

genSequence :: BCSequence -> String
genSequence (BCSequence states) = (foldl' (+>) "" (map genStatement states))

liftMFirstArg :: Monad m => (a -> b -> c) -> m a -> b -> m c --for when only the first argument of a function is monadic, lifts the function so it takes one monadic arg 
liftMFirstArg f x y = liftM2 f x (pure y) --wack monad logic

genExpr :: BCExpr -> String
genExpr (BCExprNode ex1 op ex2) = "(" ++ (genExpr ex1) ++ (show op) ++ (genExpr ex2) ++ ")"
genExpr (BCExprUnaryNode op expr) = "(" ++ (show op) ++ (genExpr expr) ++ ")"
genExpr (BCExprFuncCall (BCCall id args)) = "(" ++ "UNIMPLEMENTED" ++ ")"
genExpr (BCExprFinal val) = "(" ++ (getVal val) ++ ")"

getVal (BCValueLiteral (BCLiteralBool (BCBool b))) = map toLower $ show b
getVal (BCValueLiteral (BCLiteralNumber (BCNumberFloat (BCFloat f)))) = show f
getVal (BCValueLiteral (BCLiteralNumber (BCNumberInt (BCInt i)))) = show i
getVal (BCValueIdentifier (BCIdentifier name _)) = "UNIMPLEMENTED"



subBuiltIn :: String -> [BCArg] -> String
subBuiltIn def args = concatMap (subArgs args) (getStrSubs def)

getStrSubs def = 
    case parse strSubs "" def of
	Right x -> x
	_ -> []

subArgs :: [BCArg] -> StrSub -> String
subArgs args (StrSub n) = genExpr $ getArgExpr $ args !! n
subArgs args (StrNoSub s) = s

getArgExpr (BCArg ex) = ex

strSubs :: Parsec String st [StrSub]
strSubs = many ((StrSub <$> try strSubN) <|> (StrNoSub <$> many (noneOf "$")))

strSubN :: Parsec String st Int
strSubN = char '$' *> (read <$> many1 digit)

data StrSub = StrNoSub String | StrSub Int

eqFst :: Eq a => a -> (a, b) -> Bool
eqFst x (y, _) = x == y


evalLit :: BCLiteral -> String
evalLit (BCLiteralBool (BCBool b)) = show b
evalLit (BCLiteralNumber (BCNumberInt (BCInt i))) = show i
evalLit (BCLiteralNumber (BCNumberFloat (BCFloat f))) = show f


getSType :: StateType -> String
getSType StateBool = "BCIState::Boolean"
getSType StateU8 = "BCIState::u8"
getSType StateI8 = "BCIState::i8"
getSType StateU32 = "BCIState::u32"
getSType StateI32 = "BCIState::i32"

getVType :: BCDataType -> String
getVType BoolType = "bool"
getVType IntType = "int"
getVType NumType = "double"
getVType StringType = "string"

actorify :: [String] -> [String] --In an actor, function calls are to the actor itself, so we need to add dots. 
actorify [] = []
actorify [d] = ["->" ++ d] --Since the actor is a pointer, we also need to add an arrow at the start.
actorify (d:ds) =  ["->" ++ d] ++ (map ("." ++) ds) 

indent :: String -> String
indent str = "\t" ++ str

(+>) :: String -> String -> String
l +> r = l ++ "\n" ++ r
