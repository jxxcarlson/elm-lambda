module Blackbox exposing (examplesText, helpText, transform)

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
λz.z

----------------------------------------------------
Commands

STR              # beta reduce STR
:let a b         # store b in the variable a
:parse STR       # parse STR to Expr
:normal STR      # is STR in normal form?

:examples        # display examples
:load defs.txt   # load definitions file

:reset           # reset definitions
:show            # display file contents
:defs            # display current definitions
:raw             # set app for raw output, e.g., \\x.x
:pretty          # set app for pretty output, e.g., λx.x

To quit, type ^D.

NOTE: \\x.\\y.x \\u.u and (\\x.\\y.x)\\u.u are different.
The first is an abstraction in normal form: λx.λy.x(λu.u).
The second — (λx.λy.x)λu.u — is an application that beta
reduces to λy.λu.u.

"""


examplesText =
    """
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

> :normal  λx.λy.x λu.u
true

> :normal (λx.λy.x)λu.u
false

Note: Use parentheses to ensure that first is applied
to second and vice versa.
----------------------------------------------------

"""
