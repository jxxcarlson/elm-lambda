module Meta.Expression exposing
    ( Environment
    , Meta(..)
    , addVar
    , define
    , eval
    , showEnvironment
    )

import Dict exposing (Dict)
import Lambda.Expression as Lambda
import Lambda.Parser
import Maybe.Extra


type Meta
    = V String Lambda.Expr
    | L Lambda.Expr
    | MetaApply (List Meta)
    | MetaAbstract String (List Meta)
    | MetaErr String


type alias Environment =
    Dict String Meta


showMeta : Meta -> String
showMeta meta =
    case meta of
        V name expr ->
            name ++ ": " ++ Lambda.toString expr

        L expr ->
            "lambda: " ++ Lambda.toString expr

        MetaApply metas ->
            "metaApply: " ++ (List.map showMeta metas |> String.join " ")

        MetaAbstract binder metas ->
            "metaAbstract: " ++ binder ++ "." ++ (List.map showMeta metas |> String.join " ")

        MetaErr str ->
            "Error: " ++ str


{-|

    > idv = define "id" "\\x.x"
    V "id" (Lambda "x" (Var "x"))

    > eval (MetaApply [idv, idv])
    Just (Lambda "x0" (Var "x0"))

    > eval (MetaApply [idv, idv, idv])
    Just (Lambda "x00" (Var "x00"))

-}
eval : Meta -> Maybe Lambda.Expr
eval meta =
    case meta of
        V _ expr ->
            Just <| Lambda.beta expr

        L expr ->
            Just <| Lambda.beta expr

        MetaApply metas ->
            List.map eval metas |> Maybe.Extra.values |> applyList |> Maybe.map Lambda.beta

        MetaAbstract binder metas ->
            case applyList (List.map eval metas |> Maybe.Extra.values) of
                Nothing ->
                    Nothing

                Just expr ->
                    Just <| Lambda.Lambda binder (Lambda.beta expr)

        MetaErr _ ->
            Nothing


applyList : List Lambda.Expr -> Maybe Lambda.Expr
applyList exprs =
    case exprs of
        a :: b :: rest ->
            Just (loop { acc = Lambda.Apply a b, terms = rest } nextStep)

        _ ->
            Nothing


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b


type alias State =
    { acc : Lambda.Expr, terms : List Lambda.Expr }


nextStep : State -> Step State Lambda.Expr
nextStep { acc, terms } =
    case List.head terms of
        Nothing ->
            Done acc

        Just term ->
            Loop { acc = Lambda.Apply term acc, terms = List.drop 1 terms }


showEnvironment : Environment -> String
showEnvironment env =
    env |> Dict.values |> List.map showMeta |> String.join "\n"


addVar : String -> String -> Environment -> Environment
addVar name str env =
    case define name str of
        (V name_ expr_) as expr ->
            Dict.insert name expr env

        _ ->
            env


define : String -> String -> Meta
define name str =
    case Lambda.Parser.parse str of
        Ok expr ->
            V name expr

        Err _ ->
            MetaErr ("Could not parse: " ++ str)
