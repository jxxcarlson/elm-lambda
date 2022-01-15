module Lambda.EvalAndrey exposing (Env, Value(..), env1, eval)

import Dict exposing (Dict)
import Lambda.Expression exposing (Expr(..))


type Value
    = Int Int
    | Str String
    | Err String
    | Closure String Expr Env


type alias Env =
    Dict String Value


env1 =
    Dict.fromList
        [ ( "z", Int 0 )
        , ( "1", Int 1 )
        , ( "2", Int 2 )
        ]


eval : Expr -> Env -> Value
eval expr env =
    case expr of
        Var name ->
            Dict.get name env |> Maybe.withDefault (Err "Variable not defined")

        Lambda name body ->
            Closure name body env

        Apply operator operand ->
            call (eval operator env) (eval operand env)


call : Value -> Value -> Value
call f x =
    case f of
        Closure name expr env ->
            eval expr (Dict.insert name x env)

        _ ->
            Err "I can only call functions"


 describe "eval"
            [ test "eval variable, empty environment" <|
                \_ ->
                    eval (Var "x") Dict.empty
                        |> Expect.equal (Lambda.EvalAndrey.Err "Variable not defined")
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