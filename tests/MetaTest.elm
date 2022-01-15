module MetaTest exposing (suite)

import Expect
import Meta.Expression
import Meta.Lang
import Test exposing (..)


env =
    Meta.Expression.setupEnviroment data


eval =
    Meta.Lang.eval env


testEval input expected =
    test input <|
        \_ ->
            eval input
                |> Expect.equal expected


data =
    """
id        \\x.x

# Booleans
true      \\x.\\y.x
false     \\x.\\y.y
and       \\p.\\q.p q p
or        \\p.\\q.p p q
not       \\p.p (false) true

# Church numerals
zero      \\s.\\z.z
one       \\s.\\z.s z
two       \\s.\\z.s s z
three     \\s.\\z.s s s z
isZero    \\n.n (\\x.zero) true
succ      \\n.\\f.\\x.f(n f x)

# Tests
test0     (isZero) zero
test1     (isZero) one
test2     (isZero) two
test3     (isZero) three

"""


suite : Test
suite =
    describe "Meta.Lang.eval"
        [ testEval "not true" "λx.λy.y"
        , testEval "not false" "λx.λy.x"
        , testEval "and true true" "λx.λy.x"
        , testEval "and true false" "λx.λy.y"
        , testEval "and false true" "λx.λy.y"
        , testEval "and false false" "λx.λy.y"
        , testEval "succ zero" "λf.λx.f(x)"
        , testEval "isZero zero" "λx.λy.x"
        , testEval "isZero one" "λs.λz.z"
        , testEval "a b c" "a(b)(c)"
        , testEval "(a b c)" "a(b)(c)"
        , testEval "λx.(a b)" "λx.a(b)"
        ]
