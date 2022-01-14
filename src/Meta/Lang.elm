module Meta.Lang exposing (eval, lookup, parse)

import Dict
import Lambda.Expression
import Lambda.Parser
import Meta.Expression
import Parser.Advanced as PA
import Set
import Tools.Advanced.Parser as PT
import Tools.Problem exposing (Context, Problem(..))


eval : Meta.Expression.Environment -> String -> String
eval env str =
    case parse str of
        Err _ ->
            "Parse error"

        Ok strs ->
            strs
                |> List.map String.trim
                |> List.map (lookup env)
                |> List.map (expand env)
                |> apply
                |> Lambda.Expression.beta
                |> Lambda.Expression.reduceSubscripts
                |> Lambda.Expression.toString


apply : List Lambda.Expression.Expr -> Lambda.Expression.Expr
apply exprs =
    case exprs of
        [] ->
            Lambda.Expression.Var "ERROR (3)"

        expr :: [] ->
            Lambda.Expression.beta expr

        _ ->
            Lambda.Expression.beta (Lambda.Expression.apply exprs)


parse : String -> Result (List (PA.DeadEnd Context Problem)) (List String)
parse str =
    PA.run (PT.many itemParser) str


lookup : Meta.Expression.Environment -> String -> Lambda.Expression.Expr
lookup env str =
    case Dict.get str env of
        Just meta ->
            case Meta.Expression.eval meta of
                Just expr ->
                    expr

                Nothing ->
                    Lambda.Expression.Var "ERROR (1)"

        Nothing ->
            case Lambda.Parser.parse str of
                Ok expr ->
                    expr

                Err _ ->
                    Lambda.Expression.Var "ERROR (2)"


expand a b =
    b



--
--expand : MetaTest.Expression.Environment -> LambdaTest.Expression.Expr -> LambdaTest.Expression.Expr
--expand env expr =
--    case expr of
--        LambdaTest.Expression.Var str ->
--            case Dict.get str env of
--                Nothing ->
--                    expr
--
--                Just (Var ) ->
--                    LambdaTest.Expression.Var val


itemParser =
    PA.oneOf [ PT.first simpleItemParser PA.spaces, PT.first (PT.parenthesized simpleItemParser) PA.spaces ]


simpleItemParser : PA.Parser Context Problem String
simpleItemParser =
    PA.variable { start = \c -> Char.isAlpha c || c == '\\', inner = \c -> c /= ' ', reserved = Set.empty, expecting = ExpectingVar }
