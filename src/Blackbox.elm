module Blackbox exposing (applySubstitutions, helpText, transform)

import Dict exposing (Dict)
import Lambda
import LambdaParser exposing (parse)


applySubstitutions : List ( String, String ) -> String -> String
applySubstitutions substitutions str =
    List.foldl (\( a, b ) s -> String.replace a b s) str substitutions


subs =
    [ ( "first", "\\x.\\y.x" )
    , ( "second", "\\x.\\y.y" )
    ]


transform : String -> String
transform str =
    case str |> parse |> Result.map (Lambda.beta >> Lambda.toString) of
        Ok output ->
            output

        Err _ ->
            "Error"


helpText : String
helpText =
    """----------------------------------------------------
Beta reduce lambda expressions

For example,

> (\\x.x)(\\y.y)(\\z.z)
\\z.z

----------------------------------------------------
Commands

:let a b       # store b in the variable a


----------------------------------------------------
"""
