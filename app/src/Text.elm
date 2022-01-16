module Text exposing (examples, help)


help : String
help =
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
:beta a b        # is a beta-equivalent to b?

:examples        # display examples
:load defs.txt   # load definitions file

:reset           # reset environment
:show            # display environment file
:env             # display current environment

:raw             # set app for raw output, e.g., \\x.x
:pretty          # set app for pretty output, e.g., λx.x
:named           # set app for named output, e.g., 'true' instead of λx.λy.x

To quit, type ^D.

NOTE: \\x.\\y.x \\u.u and (\\x.\\y.x)\\u.u are different.
The first is an abstraction in normal form: λx.λy.x(λu.u).
The second — (λx.λy.x)λu.u — is an application that beta
reduces to λy.λu.u.

"""


examples : String
examples =
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
Ok (LambdaTest "s" (LambdaTest "z" (Apply (Apply (Var "s") (Var "s")) (Var "z"))))

> :normal  λx.λy.x λu.u
true

> :normal (λx.λy.x)λu.u
false

Note: Use parentheses to ensure that first is applied
to second and vice versa.
----------------------------------------------------

"""
