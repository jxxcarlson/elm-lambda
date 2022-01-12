module ParserTest exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression exposing (Expr(..), beta)
import Lambda.Parser exposing (..)
import Parser.Advanced exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "Lambda LambdaParser"
        [ describe "parser"
            [ test "parse variable" <|
                \_ ->
                    run exprParser "a"
                        |> Expect.equal (Ok (Var "a"))
            , test "parse abstraction" <|
                \_ ->
                    run exprParser "\\x.x"
                        |> Expect.equal (Ok (Lambda "x" (Var "x")))
            , test "parse two abstractions" <|
                \_ ->
                    run exprParser "\\x.\\y.z"
                        |> Expect.equal (Ok (Lambda "x" (Lambda "y" (Var "z"))))
            , test "parse application" <|
                \_ ->
                    run exprParser "x y"
                        |> Expect.equal (Ok (Apply (Var "x") (Var "y")))
            , test "parse abstraction with application" <|
                \_ ->
                    run exprParser "\\x.(x)(y)"
                        |> Expect.equal (Ok (Lambda "x" (Apply (Var "x") (Var "y"))))
            , test "parse abstraction with application (2)" <|
                \_ ->
                    run exprParser "\\x.x y"
                        |> Expect.equal (Ok (Lambda "x" (Apply (Var "x") (Var "y"))))
            , test "parse abstraction with application (3)" <|
                \_ ->
                    run exprParser "\\x.x y"
                        |> Expect.equal (Ok (Lambda "x" (Apply (Var "x") (Var "y"))))
            ]
        ]
