module Blackbox exposing (transform, helpText)

import LambdaParser  exposing(parse)
import Lambda

transform : String -> String
transform str =
    case str |> parse |> Result.map (Lambda.beta >> Lambda.toString) of
        Ok output -> output
        Err _ -> "Error"

helpText : String
helpText = """Beta reduce lambda expressions

For example,

> (\\x.x)(\\y.y)(\\z.z)
\\z.z
"""

