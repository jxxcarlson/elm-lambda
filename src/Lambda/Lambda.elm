module Lambda.Lambda exposing
    ( Expr(..)
    , beta
    , boundVariables
    , freeVariables
    , freshenVariables
    , isNormal
    , renameVariable
    , substitute
    , toRawString
    , toString
    , variables
    )

-- https://lambdacalc.io/

import Set exposing (Set)


type Expr
    = Var String
    | Lambda String Expr
    | Apply Expr Expr


toRawString : Expr -> String
toRawString expr =
    case expr of
        Var str ->
            str

        Lambda binder expr_ ->
            "\\" ++ binder ++ "." ++ toRawString expr_

        Apply e1 e2 ->
            toRawString e1 ++ " " ++ toRawString e2


toString : Expr -> String
toString expr =
    case expr of
        Var str ->
            str

        Lambda binder expr_ ->
            String.fromChar 'λ' ++ binder ++ "." ++ toString expr_

        Apply e1 e2 ->
            -- toString e1 ++ " " ++ toString e2
            toString e1 ++ "(" ++ toString e2 ++ ")"



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


freshenVariable : String -> Expr -> Expr -> Expr
freshenVariable x expr1 expr2 =
    let
        xx =
            fresh x (variables expr2 |> Set.toList)
    in
    renameVariable x xx expr1


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
            in
            substitute e2Fresh x e1

        Lambda x e ->
            Lambda x (beta e)

        Apply e f ->
            Apply (beta e) (beta f)

        _ ->
            expr


isNormal : Expr -> Bool
isNormal expr =
    beta expr == expr
