module Lambda exposing
    ( Env
    , Expr(..)
    , Value(..)
    , beta
    , call
    , eval
    , freeVariables
    , substitute
    )

-- https://lambdacalc.io/

import Dict exposing (Dict)
import Set exposing (Set)


type Expr
    = Var String
    | Lambda String Expr
    | Apply Expr Expr


type Value
    = Int Int
    | Str String
    | Err String
    | Closure String Expr Env


type alias Env =
    Dict String Value


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



-- AUXILIARY


freeVariables : Expr -> Set String
freeVariables expr =
    case expr of
        Var str ->
            Set.singleton str

        Lambda name body ->
            Set.diff (freeVariables body) (Set.singleton name)

        Apply e1 e2 ->
            Set.union (freeVariables e1) (freeVariables e2)


substitute : Expr -> String -> Expr -> Expr
substitute expr1 x expr2 =
    case expr2 of
        Var y ->
            if x == y then
                expr1

            else
                Var y

        Lambda y expr ->
            if x /= y && not (Set.member y (freeVariables expr2)) then
                Lambda y (substitute expr1 x expr)

            else
                expr2

        Apply e1 e2 ->
            Apply (substitute expr1 x e1) (substitute expr1 x e2)


beta : Expr -> Expr
beta expr =
    case expr of
        Apply (Lambda x e1) e2 ->
            beta (substitute e2 x e1)

        Lambda x e ->
            Lambda x (beta e)

        Apply e f ->
            beta (Apply (beta e) f)

        _ ->
            expr
