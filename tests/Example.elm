module Example exposing (..)

import Dict
import Eval exposing (Env, Value(..), eval)
import Expect exposing (Expectation)
import Lambda exposing (Expr(..), beta)
import Library exposing (id_)
import Test exposing (..)


suite : Test
suite =
    describe "Lambda"
        [ describe "eval"
            [ test "eval variable, empty environment" <|
                \_ ->
                    eval (Var "x") Dict.empty
                        |> Expect.equal (Eval.Err "Variable not defined")
            , test "eval variable, x = 17 in environment" <|
                \_ ->
                    eval (Var "x") (Dict.singleton "x" (Int 17))
                        |> Expect.equal (Int 17)
            , test "eval application (identity function)" <|
                \_ ->
                    eval (Apply (Lambda "x" (Var "x")) (Var "x"))
                        (Dict.singleton "x" (Int 17))
                        |> Expect.equal (Int 17)
            , test "eval application (identity function (2))" <|
                \_ ->
                    eval (Apply (Lambda "x" (Var "x")) (Var "y")) (Dict.singleton "y" (Str "y"))
                        |> Expect.equal (Str "y")
            ]
        , describe "beta"
            [ test
                "true e f -> e"
              <|
                \_ ->
                    beta (Apply (Apply (Lambda "x" (Lambda "y" (Var "x"))) (Var "e")) (Var "f"))
                        |> Expect.equal (Var "e")
            , test
                "false e f -> f"
              <|
                \_ ->
                    beta (Apply (Apply (Lambda "x" (Lambda "y" (Var "y"))) (Var "e")) (Var "f"))
                        |> Expect.equal (Var "f")
            , test "id id -> id" <|
                \_ -> beta (Apply (id_ "x") (id_ "y")) |> Expect.equal (id_ "y")
            , test
                "zero (Var 'u')"
              <|
                \_ ->
                    beta (Apply (Library.zero "s" "z") (Var "u"))
                        |> Expect.equal (Lambda "z" (Var "z"))
            , test
                "one zero"
              <|
                \_ ->
                    beta (Apply (Library.one "s" "z") (Library.zero "s'" "z'"))
                        |> Expect.equal (Lambda "z" (Lambda "z'" (Var "z'")))
            , test
                "zero one"
              <|
                \_ ->
                    beta (Apply (Library.zero "s" "z") (Library.one "s'" "z'"))
                        |> Expect.equal (Lambda "z" (Var "z"))
            , test
                "isZero zero"
              <|
                \_ ->
                    beta (Apply Library.isZero (Library.zero "u" "v"))
                        |> Expect.equal Library.true
            , test
                "isZero one"
              <|
                \_ ->
                    beta (Apply Library.isZero (Library.one "u" "v"))
                        |> Expect.equal Library.false
            ]
        ]



-- beta (App isZero (zero "u" "v")
{-

   > App id id
   App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))
       : Expr
   > eval (App id (Var "y")) (Dict.singleton "y" (Str "y"))
   Str "y" : Value
   > eval (App id id) (Dict.singleton "y" (Str "y"))
   Closure "x" (Var "x") (Dict.fromList [("y",Str "y")])
       : Value
   > eval (App id id) Dict.empty)
   |
   > eval (App id id) Dict.empty
   Closure "x" (Var "x") (Dict.fromList [])

-}
{-


   > e1 = Lambda "a" (Var "a")
   Lambda "a" (Var "a") : Expr

   > e2 = Lambda "b" (Var "x")
   Lambda "b" (Var "x") : Expr

   > substitute e1 "x" e2
   Lambda "b" (Lambda "a" (Var "a"))

   > substitute e1 "x" (App e2 e2)
   App (Lambda "b" (Lambda "a" (Var "a"))) (Lambda "b" (Lambda "a" (Var "a")))
-}
