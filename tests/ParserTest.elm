module ParserTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Lambda exposing (Env, Expr(..), Value(..), beta, eval)
import LambdaParser exposing (..)
import Library exposing (id_)
import Parser.Advanced exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "Lambda Parser"
        [ describe "parser"
            [ test "parse variable" <|
                \_ ->
                    run exprParser1 "a b c"
                        |> Expect.equal (Ok (Var "a"))
            , test "parse abstraction" <|
                \_ ->
                    run exprParser1 "\\x.x"
                        |> Expect.equal (Ok (Lambda "x" (Var "x")))
            , test "parse two abstractions" <|
                \_ ->
                    run exprParser1 "\\x.\\y.z"
                        |> Expect.equal (Ok (Lambda "x" (Lambda "y" (Var "z"))))
            ]
        ]
