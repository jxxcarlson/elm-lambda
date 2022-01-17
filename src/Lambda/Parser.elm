module Lambda.Parser exposing
    ( parse, exprParser
    , parseUnsafe
    )

{-| The primary function exported is

    parse : String -> Result Error Expr

If application of parse to a string is successful it
returns an Expr.

@docs parse, exprParser

-}

import Lambda.Expression exposing (Expr(..))
import Parser.Advanced as PA exposing ((|.), (|=))
import Set
import Tools.Advanced.Parser as PT
import Tools.Problem exposing (Context, Problem(..))


type alias Parser a =
    PA.Parser Context Problem a


{-| -}
parse : String -> Result (List (PA.DeadEnd Context Problem)) Expr
parse str =
    PA.run exprParser str


parseUnsafe : String -> Expr
parseUnsafe str =
    case parse str of
        Ok e ->
            e

        Err _ ->
            Var "Parse failed"


{-|

    > run exprParser "\\x.x(\\y.y)(\\z.z)"
    Ok (Apply (Apply (LambdaTest "x" (Var "x")) (LambdaTest "y" (Var "y"))) (LambdaTest "z" (Var "z")))

    > run exprParser "\\x.x(\\y.y)(\\z.z)" |> Result.map beta
    Ok (LambdaTest "z" (Var "z"))

-}
exprParser : PA.Parser Context Problem Expr
exprParser =
    PA.inContext Tools.Problem.Expression
        (simpleExprParser |> PA.andThen applicationParser)


applicationParser aInitial =
    PA.inContext Tools.Problem.Application
        (PT.foldWithInitialValue (\a b -> Apply b a) simpleExprParser aInitial)


simpleExprParser =
    PA.inContext Tools.Problem.SimpleExpression
        (PT.first
            (PA.oneOf
                [ PA.lazy (\_ -> PT.parenthesized exprParser)
                , PA.lazy (\_ -> abstractionParser)
                , variableParser
                ]
            )
            PA.spaces
        )


abstractionParser =
    PA.inContext Tools.Problem.Abstraction <|
        (PA.succeed (\var expr -> Lambda var expr)
            |. PA.oneOf [ PA.symbol (PA.Token (String.fromChar 'λ') ExpectingLambdaCharacter), PA.symbol (PA.Token "\\" ExpectingBackslash) ]
            |= rawVariableParser
            |. PA.symbol (PA.Token "." ExpectingPeriod)
            |= PA.lazy (\_ -> exprParser)
            |. PA.spaces
        )



--variableParser : PA.LambdaParser Context Problem Expr
--variableParser =
--    text Char.isAlpha (\c -> not <| List.member c [ '.', ' ', '\n' ]) |> PA.map Var
--variableParser : PA.LambdaParser Context Problem Expr
--variableParser =
--    PT.textPS Char.isAlpha [ '.', ' ', '\n' ] |> PA.map ((\val -> val.content) >> Var)


variableParser : PA.Parser Context Problem Expr
variableParser =
    PA.inContext Tools.Problem.Variable <|
        (PA.variable { start = Char.isAlpha, inner = Char.isAlpha, reserved = Set.empty, expecting = ExpectingVar }
            |> PA.map Var
        )


rawVariableParser : PA.Parser Context Problem String
rawVariableParser =
    text Char.isAlpha (\c -> not <| List.member c [ '.', ' ', '\n' ])


text : (Char -> Bool) -> (Char -> Bool) -> Parser String
text prefix continue =
    PA.succeed (\start finish content -> String.slice start finish content)
        |= PA.getOffset
        |. PA.chompIf (\c -> prefix c) ExpectingPrefix
        |. PA.chompWhile (\c -> continue c)
        |= PA.getOffset
        |= PA.getSource
