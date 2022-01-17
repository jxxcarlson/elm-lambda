module Lambda.Eval exposing
    ( eval
    , equivalent
    )

{-| There is a single exposed function,

    eval : Dict String String -> String -> String

The first argument is a dictionary which defined rewrite
rules, e.g.,

    Dict.fromList
        [
          ("true", "\\x.\\y.x")
          "false",  "\\x.\\y.y")
          "and", "\\p.\\q.p q p")
          "or", "\\p.\\q.p p q"
          "not", "\\p.p (\\x.\\y.y) (\\x.\\y.x)"
        ]

Strings on the left are to be rewritten as strings on the right.
Those on the right should be lambda tersm.

The eval function parses the given string, rewrites it as needed,
applies beta reduction, and then turns this final expression
back into a string.

@docs eval

-}

import Dict exposing (Dict)
import Lambda.Expression exposing (Expr(..), ViewStyle(..))
import Lambda.Parser


equivalent : Dict String String -> String -> String
equivalent dict str =
    case Lambda.Parser.parse str of
        Err err ->
            "Parse error: " ++ Debug.toString err

        Ok expr ->
            case rewrite dict expr of
                Apply a b ->
                    case ( Lambda.Expression.beta a, Lambda.Expression.beta b ) of
                        ( Var s, Var t ) ->
                            if String.left 22 s == "TOO MANY SUBSTITUTIONS" then
                                "LHS may be divergent"

                            else if String.left 22 t == "TOO MANY SUBSTITUTIONS" then
                                "RHS may be divergent"

                            else if s == t then
                                "true"

                            else
                                "false"

                        _ ->
                            case Lambda.Expression.equivalent a b of
                                True ->
                                    "true"

                                False ->
                                    "false"

                _ ->
                    "Bad args"


{-| -}
eval : ViewStyle -> Dict String String -> String -> String
eval viewStyle dict str =
    case Lambda.Parser.parse str of
        Err err ->
            "Parse error: " ++ Debug.toString err

        Ok expr ->
            expr
                |> rewrite dict
                |> Lambda.Expression.beta
                |> Lambda.Expression.reduceSubscripts
                -- |> Lambda.Expression.compressNameSpace
                |> Lambda.Expression.toString viewStyle
                |> reverseRewrite viewStyle dict


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


reverseRewrite : ViewStyle -> Dict String String -> String -> String
reverseRewrite outputStyle definitions str =
    case outputStyle of
        Raw ->
            str

        Pretty ->
            str

        Named ->
            let
                defPairs =
                    Dict.toList definitions |> List.map (\( name, lambda ) -> ( name, String.replace "\\" "λ" lambda ))
            in
            List.foldl (\( name, lambda ) acc -> String.replace lambda name acc) str defPairs
