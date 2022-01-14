module Meta.Expression exposing
    ( Environment
    , Meta(..)
    , addVar
    , apply
    , define
    , eval
    , load
    , setupEnviroment
    , showEnvironment
    , testEnvironment
    )

import Dict exposing (Dict)
import Lambda.Defs
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


apply : List Meta -> Maybe Lambda.Expr
apply metas =
    eval <| MetaApply metas


{-|

    > idv = define "id" "\\x.x"
    V "id" (LambdaTest "x" (Var "x"))

    > eval (MetaApply [idv, idv])
    Just (LambdaTest "x0" (Var "x0"))

    > eval (MetaApply [idv, idv, idv])
    Just (LambdaTest "x00" (Var "x00"))

-}
eval : Meta -> Maybe Lambda.Expr
eval meta =
    case meta of
        V _ expr ->
            Just <| Lambda.beta expr

        L expr ->
            Just <| Lambda.beta expr

        MetaApply metas ->
            List.map eval metas
                |> Maybe.Extra.values
                |> Lambda.apply
                |> Lambda.beta
                |> Just

        MetaAbstract binder metas ->
            let
                expr =
                    Lambda.apply (List.map eval metas |> Maybe.Extra.values)
            in
            Just <| Lambda.Lambda binder (Lambda.beta expr)

        MetaErr _ ->
            Nothing


showEnvironment : Environment -> String
showEnvironment env =
    env |> Dict.values |> List.map showMeta |> String.join "\n"


addVar : String -> String -> Environment -> Environment
addVar name str env =
    case define name str of
        (V _ _) as expr ->
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


load : List ( String, String ) -> Environment
load defs =
    List.foldl (\( name, definition ) acc -> addVar name definition acc) Dict.empty defs


setupEnviroment : String -> Environment
setupEnviroment data =
    let
        rawDefinitions =
            Lambda.Defs.install (Lambda.Defs.removeComments data) []

        definitions =
            Lambda.Defs.expand rawDefinitions |> Lambda.Defs.expand
    in
    load definitions


testEnvironment =
    load
        [ ( "id", "\\x.x" )
        , ( "true", "\\x.\\y.x" )
        , ( "false", "\\x.\\y.y" )
        , ( "zero", "\\s.\\z.z" )
        , ( "one", "\\s.\\z.s z" )
        , ( "two", "\\s.\\z.s s z" )
        ]
