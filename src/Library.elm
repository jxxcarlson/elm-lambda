module Library exposing (..)

import Dict
import Lambda exposing (..)


stdEnv =
    Dict.fromList (List.map (\s -> ( s, Str s )) (String.split "" "abcdefghijklmnopqrstuvwxyz"))


true =
    Lambda "x" (Lambda "y" (Var "x"))


false =
    Lambda "x" (Lambda "y" (Var "y"))


zero s z =
    Lambda s (Lambda z (Var z))


one s z =
    Lambda s (Lambda z (Apply (Var s) (Var z)))


isZero =
    Lambda "n" (Apply (Apply (Var "n") (Lambda "x" false)) true)


two s z =
    Lambda s (Lambda z (Apply (Var s) (Apply (Var s) (Var z))))


a =
    Apply (Var "s") (Var "z")


ex1 =
    Apply (Apply true (Var "e")) (Var "f")


ex2 =
    Apply (Apply false (Var "e")) (Var "f")


id =
    Lambda "x" (Var "x")


id_ x =
    Lambda x (Var x)


k =
    Lambda "x" (Lambda "y" (Var "x"))
