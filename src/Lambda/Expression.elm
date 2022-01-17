module Lambda.Expression exposing
    ( Expr(..), beta, compressNameSpace, isNormal, reduceSubscripts, toString
    , ViewStyle(..), alphaConvertWithExpr, applyVariableMap, compare, depth, equivalent, size, size2, substitute, variableMap
    )

{-| In this module we define the type Expr used to represent the lambda calculus.
The main function is beta: Expr -> Expr which carries out beta reductions.

@docs Expr, beta, compressNameSpace, isNormal, reduceSubscripts, toString

For checking things: <https://lambdacalc.io/>

-}

import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


{-| -}
type Expr
    = Var String
    | Lambda String Expr
    | Apply Expr Expr


{-| Run alpha conversions on e2 using the map e2 -> e1.
If e1 and e2 do not have the same structure, then e2 is
returned unchanged
-}
alphaConvertWithExpr : Expr -> Expr -> Expr
alphaConvertWithExpr e1 e2 =
    let
        vMap =
            variableMap e2 e1 |> Maybe.withDefault Dict.empty
    in
    applyVariableMap vMap e2


applyVariableMap : Dict String String -> Expr -> Expr
applyVariableMap vMap expr =
    case expr of
        Var x ->
            case Dict.get x vMap of
                Nothing ->
                    Var x

                Just y ->
                    Var y

        Lambda x e ->
            case Dict.get x vMap of
                Nothing ->
                    Lambda x (applyVariableMap vMap e)

                Just y ->
                    Lambda y (applyVariableMap vMap e)

        Apply e1 e2 ->
            Apply (applyVariableMap vMap e1) (applyVariableMap vMap e2)


variableMap : Expr -> Expr -> Maybe (Dict String String)
variableMap a b =
    let
        map_ =
            compare a b |> List.sort |> List.Extra.unique

        va =
            variables a

        vb =
            variables b
    in
    if Set.size va == Set.size vb && Set.size va == List.length map_ then
        Just (Dict.fromList map_)

    else
        Nothing


compare : Expr -> Expr -> List ( String, String )
compare e1 e2 =
    case ( e1, e2 ) of
        ( Var x, Var y ) ->
            [ ( x, y ) ]

        ( Lambda x1 b1, Lambda x2 b2 ) ->
            ( x1, x2 ) :: compare b1 b2

        ( Apply a1 b1, Apply a2 b2 ) ->
            compare a1 a2 ++ compare b1 b2

        _ ->
            [ ( "Error", "Error" ) ]


size : Expr -> Int
size expr =
    case expr of
        Var _ ->
            1

        Lambda _ e ->
            1 + size e

        Apply e1 e2 ->
            1 + size e1 + size e2


size2 : Expr -> Int
size2 expr =
    case expr of
        Var str ->
            String.length str

        Lambda _ e ->
            1 + size2 e

        Apply e1 e2 ->
            1 + size2 e1 + size2 e2


depth : Expr -> Int
depth expr =
    case expr of
        Var _ ->
            1

        Lambda _ e ->
            1 + size e

        Apply e1 e2 ->
            1 + max (size e1) (size e2)


type ViewStyle
    = Raw
    | Pretty
    | Named


toString : ViewStyle -> Expr -> String
toString viewStyle expr =
    case viewStyle of
        Raw ->
            toRawString expr

        Pretty ->
            toPrettyString expr

        Named ->
            toPrettyString expr


toRawString : Expr -> String
toRawString expr =
    case expr of
        Var str ->
            str

        Lambda binder expr_ ->
            "\\" ++ binder ++ "." ++ toRawString expr_

        Apply e1 e2 ->
            toRawString e1 ++ " " ++ toRawString e2


{-| String representation of expression
-}
toPrettyString : Expr -> String
toPrettyString expr =
    case expr of
        Var str ->
            str

        Lambda binder expr_ ->
            String.fromChar 'λ' ++ binder ++ "." ++ toPrettyString expr_

        Apply e1 e2 ->
            toPrettyString e1 ++ "(" ++ toPrettyString e2 ++ ")"


apply : List Expr -> Expr
apply exprs =
    case exprs of
        [] ->
            Var "ERROR: List cannot be empty"

        expr :: [] ->
            expr

        a :: b :: [] ->
            Apply a b

        a :: b :: rest ->
            apply (Apply a b :: rest)



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


boundVariables : Expr -> Set String
boundVariables expr =
    Set.diff (variables expr) (freeVariables expr)


variables : Expr -> Set String
variables expr =
    case expr of
        Var str ->
            Set.singleton str

        Lambda name body ->
            Set.union (variables body) (Set.singleton name)

        Apply e1 e2 ->
            Set.union (variables e1) (variables e2)


freshenVariables : Expr -> Expr -> Expr
freshenVariables expr1 expr2 =
    freshenVariablesAux (variables expr2 |> Set.toList) expr1


freshenVariablesAux : List String -> Expr -> Expr
freshenVariablesAux avoid expr =
    case List.head avoid of
        Nothing ->
            expr

        Just x ->
            let
                xx =
                    fresh x avoid

                newExpr =
                    renameVariable x xx expr
            in
            freshenVariablesAux (List.drop 1 avoid) newExpr


renameVariable : String -> String -> Expr -> Expr
renameVariable a b expr =
    case expr of
        Var x ->
            if x == a then
                Var b

            else
                expr

        Lambda x body ->
            if x == a then
                Lambda b (renameVariable a b body)

            else
                Lambda x (renameVariable a b body)

        Apply e1 e2 ->
            Apply (renameVariable a b e1) (renameVariable a b e2)


fresh : String -> List String -> String
fresh str avoid =
    if List.member str avoid then
        freshAux 0 str avoid

    else
        str


freshAux : Int -> String -> List String -> String
freshAux count str avoid =
    let
        newStr =
            str ++ String.fromInt count
    in
    if List.member newStr avoid then
        freshAux (count + 1) str avoid

    else
        newStr


{-|

    substitute expr1 for (Var "x") in expr2

-}
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


maxSize2 =
    100000


{-| beta reduce expression
-}
beta : Expr -> Expr
beta expr =
    if betaAux expr == expr then
        expr

    else
        beta (betaAux expr)


betaAux : Expr -> Expr
betaAux expr =
    case expr of
        Apply (Lambda x e1) e2 ->
            let
                e2Fresh =
                    freshenVariables e2 e1

                a =
                    substitute e2Fresh x e1
            in
            if size2 a > maxSize2 then
                Var ("TOO MANY SUBSTITUTIONS (size2 > " ++ String.fromInt maxSize2 ++ "). Term may be divergent")

            else
                a

        Lambda x e ->
            Lambda x (beta e)

        Apply e f ->
            Apply (beta e) (beta f)

        _ ->
            expr


{-| is the expression in normal form?
-}
isNormal : Expr -> Bool
isNormal expr =
    beta expr == expr


numerals =
    String.split "" "0123456789"


hasNumericEnding : String -> Bool
hasNumericEnding str =
    List.member (String.right 1 str) numerals


{-| remove numeric subscripts in variable names to the extent possible
-}
reduceSubscripts : Expr -> Expr
reduceSubscripts expr =
    let
        vars =
            variables expr |> Set.toList

        varsWithNumericEndings =
            List.filter (\s -> hasNumericEnding s) vars

        reducibleVariables =
            List.filter (\s -> not (List.member (String.dropRight 1 s) vars)) varsWithNumericEndings
    in
    List.foldl (\var acc -> renameVariable var (String.dropRight 1 var) acc) expr reducibleVariables


{-| Map variable names to "a", "b", ..., "z" to the extent possible
-}
compressNameSpace : Expr -> Expr
compressNameSpace expr =
    let
        vars =
            variables expr |> Set.toList |> List.sort |> List.take 26

        alphabet =
            String.split "" "abcdefghijklmnopqrstuvwxyz" |> List.take (List.length vars)

        -- List.range 0 25 |> List.map (\n -> "X" ++ String.fromInt n)
        pairs =
            List.map2 (\a b -> ( a, b )) vars alphabet
    in
    List.foldl (\pair acc -> renameVariable (Tuple.first pair) (Tuple.second pair) acc) expr pairs


equivalent : Expr -> Expr -> Bool
equivalent e1 e2 =
    let
        f1 =
            beta e1

        f2 =
            alphaConvertWithExpr f1 (beta e2)
    in
    f1 == f2
