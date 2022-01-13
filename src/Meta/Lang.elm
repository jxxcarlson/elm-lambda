module Meta.Lang exposing (eval, lookup, parse)

import Dict
import Lambda.Expression
import Lambda.Parser
import Meta.Expression exposing (Meta(..))
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
            List.map (lookup env) strs
                |> Meta.Expression.apply
                |> Maybe.map Lambda.Expression.toString
                |> Maybe.withDefault "error"


parse : String -> Result (List (PA.DeadEnd Context Problem)) (List String)
parse str =
    PA.run (PT.many itemParser) str


lookup : Meta.Expression.Environment -> String -> Meta.Expression.Meta
lookup env str =
    case Dict.get str env of
        Just meta ->
            meta

        Nothing ->
            case Lambda.Parser.parse str of
                Ok expr ->
                    L expr

                Err _ ->
                    MetaErr "Parse error"


itemParser =
    PA.oneOf [ PT.first simpleItemParser PA.spaces, PT.first (PT.parenthesized simpleItemParser) PA.spaces ]


simpleItemParser : PA.Parser Context Problem String
simpleItemParser =
    PA.variable { start = \c -> Char.isAlpha c || c == '\\', inner = \c -> c /= ' ', reserved = Set.empty, expecting = ExpectingVar }
