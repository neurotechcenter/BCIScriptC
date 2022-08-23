module Builtins where

import Types
import Text.Parsec.Pos


builtins :: [Signature]
builtins = [
    Signature (BCIdentifier "start" (newPos "builtins" 1 1)) Event,
    mkprocsig "move" [NumType, NumType],
    mkprocsig "moveTo" [NumType, NumType],
    mkprocsig "changeCostume" [IntType],
    mkprocsig "nextCostume" [],
    mkfuncsig "truncate" [NumType] IntType,
    mkprocsig "displayStr" [StringType]
    ]


builtinSub :: [(String, String)]
builtinSub = [
    ("moveTo", "callingActor.setPositionX($0); callingActor.setPositionY($1)"),
    ("truncate", "((int) $0)"),
    ("displayStr", "callingActor.displayString(\"$0\")")
    ]


mkprocsig :: String -> [BCDataType] -> Signature
mkprocsig name args = Signature (BCIdentifier name (newPos "builtins" 1 1)) (Proc args)

mkfuncsig :: String -> [BCDataType] -> BCDataType -> Signature
mkfuncsig name args ret = Signature (BCIdentifier name (newPos "builtins" 1 1)) (Func args ret [])
