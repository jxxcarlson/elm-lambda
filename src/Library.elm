module Library exposing (..)

import Dict
import Lambda exposing (..)


stdEnv =
    Dict.fromList (List.map (\s -> ( s, Str s )) (String.split "" "abcdefghijklmnopqrstuvwxyz"))


true =
    Lambda "x" (Lambda "y" (Var "x"))


false =
    Lambda "x" (Lambda "y" (Var "y"))


ex1 =
    App (App true (Var "e")) (Var "f")


ex2 =
    App (App false (Var "e")) (Var "f")


id =
    Lambda "x" (Var "x")


k =
    Lambda "x" (Lambda "y" (Var "x"))
