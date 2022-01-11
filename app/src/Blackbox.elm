module Blackbox exposing (helpText, transform)

import Lambda.Lambda as Lambda
import Lambda.LambdaParser exposing (parse)


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

:load defs.txt   # load definitions file
:reset           # reset definitions
:show            # display file contents
:raw             # raw output, e.g., \\x.x
:pretty          # pretty output, e.g., λx.x

:let a b         # store b in the variable a
:parse STR       # parse STR to Expr

----------------------------------------------------
Examples
-------
> :let first \\x.\\y.x
> first
  \\x.\\y.x
> :pretty
> :let second \\x.\\y.y
> (first)(second)
  \\y.\\x0.\\y0.y0
> (second)(first)
  \\y.y

> :parse \\s.\\z.s s z
Ok (Lambda "s" (Lambda "z" (Apply (Apply (Var "s") (Var "s")) (Var "z"))))

Note: Use parentheses to ensure that first is applied
to second and vice versa.
----------------------------------------------------
"""