module ParserToolsTest exposing (..)

import Expect exposing (Expectation)
import Parser.Advanced exposing (run)
import ParserToolsAdvanced
import Test exposing (..)


suite : Test
suite =
    describe "Lambda Parser"
        [ describe "parser"
            [ test "parse variable" <|
                \_ ->
                    run expressionParser "a b c"
                        |> Expect.equal (Ok (Var "a"))
            , test "parse abstraction" <|
                \_ ->
                    run expressionParser "\\x.x"
                        |> Expect.equal (Ok (Lambda "x" (Var "x")))
            , test "parse two abstractions" <|
                \_ ->
                    run expressionParser "\\x.\\y.z"
                        |> Expect.equal (Ok (Lambda "x" (Lambda "y" (Var "z"))))
            ]
        ]
