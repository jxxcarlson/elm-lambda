module LambdaTest exposing (suite)

import Dict
import Expect
import Lambda.Expression exposing (Expr(..), beta)
import Test exposing (..)


suite : Test
suite =
    describe "Lambda"
        [ describe "beta"
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
            ]
        ]



-- beta (App isZero (zero "u" "v")
{-

   > App id id
   App (LambdaTest "x" (Var "x")) (LambdaTest "x" (Var "x"))
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


   > e1 = LambdaTest "a" (Var "a")
   LambdaTest "a" (Var "a") : Expr

   > e2 = LambdaTest "b" (Var "x")
   LambdaTest "b" (Var "x") : Expr

   > substitute e1 "x" e2
   LambdaTest "b" (LambdaTest "a" (Var "a"))

   > substitute e1 "x" (App e2 e2)
   App (LambdaTest "b" (LambdaTest "a" (Var "a"))) (LambdaTest "b" (LambdaTest "a" (Var "a")))
-}
