module Blackbox exposing (applySubstitutions, transform, helpText)

import LambdaParser  exposing(parse)
import Lambda
import Dict exposing(Dict)


applySubstitutions : List (String, String) -> String -> String
applySubstitutions substitutions str =
    List.foldl (\(a, b) s -> String.replace a b s) str substitutions

subs = [
    ("first", "\\x.\\y.x")
  , ("second", "\\x.\\y.y")
  ]


transform : String -> String
transform str =
    case str |> parse |> Result.map (Lambda.beta >> Lambda.toString) of
        Ok output -> output
        Err _ -> "Error"

helpText : String
helpText = """------------------------------
Beta reduce lambda expressions

For example,

> (\\x.x)(\\y.y)(\\z.z)
\\z.z
------------------------------
"""

