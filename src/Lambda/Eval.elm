module Lambda.Eval exposing (eval)

import Dict exposing (Dict)
import Lambda.Expression exposing (Expr(..))
import Lambda.Parser


eval : Dict String String -> String -> String
eval dict str =
    case Lambda.Parser.parse str of
        Err err ->
            "Parse error: " ++ Debug.toString err

        Ok expr ->
            rewrite dict expr
                |> Lambda.Expression.beta
                |> Lambda.Expression.reduceSubscripts
                -- |> Lambda.Expression.compressNameSpace
                |> Lambda.Expression.toString


rewrite : Dict String String -> Expr -> Expr
rewrite definitions expr =
    case expr of
        Var s ->
            case Dict.get s definitions of
                Just t ->
                    -- Var (parenthesize t)
                    case Lambda.Parser.parse t of
                        Ok u ->
                            u

                        Err _ ->
                            Var "ERROR"

                Nothing ->
                    Var s

        Lambda binder body ->
            Lambda binder (rewrite definitions body)

        Apply e1 e2 ->
            Apply (rewrite definitions e1) (rewrite definitions e2)
