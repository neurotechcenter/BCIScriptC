module Builtins where

import Types (BCVarType)



builtins :: [Signature]
builtins = [
    mkprocsig "move" [NumType, NumType],
    mkprocsig "moveTo" [NumType, NumType],
    mkprocsig "changeCostume" [IntType],
    mkprocsig "nextCostume" []
    ]

mkprocsig name args = Signature (BCIdentifier name) Proc args
