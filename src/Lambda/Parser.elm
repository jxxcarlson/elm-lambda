module Lambda.Parser exposing
    ( Parser
    , abstractionParser
    , applicationParser
    , applicationParser1
    , exprParser
    , exprParser1
    , id
    , parse
    , rawVariableParser
    , text
    , unsafeParse
    , variableParser
    )

import Lambda.Expression exposing (Expr(..))
import Parser.Advanced as PA exposing ((|.), (|=))
import Set
import Tools.Advanced.Parser as PT
import Tools.Problem exposing (Context, Problem(..))


type alias Parser a =
    PA.Parser Context Problem a


parse : String -> Result (List (PA.DeadEnd Context Problem)) Expr
parse str =
    PA.run exprParser str


unsafeParse : String -> Expr
unsafeParse str =
    case parse str of
        Ok expr ->
            expr

        Err _ ->
            id


id : Expr
id =
    Lambda "x" (Var "x")


{-|

    > run exprParser "\\x.x(\\y.y)(\\z.z)"
    Ok (Apply (Apply (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))) (Lambda "z" (Var "z")))

    > run exprParser "\\x.x(\\y.y)(\\z.z)" |> Result.map beta
    Ok (Lambda "z" (Var "z"))

-}
exprParser : PA.Parser Context Problem Expr
exprParser =
    exprParser1 |> PA.andThen applicationParser


applicationParser aInitial =
    PT.foldWithInitialValue (\a b -> Apply b a) exprParser1 aInitial



--  exprParser = PT.first exprParser1_ PA.spaces


exprParser1 =
    PT.first
        (PA.oneOf
            [ PT.parenthesized (PA.lazy (\_ -> exprParser1))
            , PA.lazy (\_ -> abstractionParser)
            , variableParser
            ]
        )
        PA.spaces


applicationParser1 : PA.Parser Context Problem Expr
applicationParser1 =
    PA.succeed (\e1 e2 -> Apply e1 e2)
        |= abstractionParser
        |. PA.spaces
        |= exprParser1
        |. PA.spaces


abstractionParser =
    PA.succeed (\var expr -> Lambda var expr)
        |. PA.oneOf [ PA.symbol (PA.Token (String.fromChar 'λ') ExpectingLambdaCharacter), PA.symbol (PA.Token "\\" ExpectingBackslash) ]
        |= rawVariableParser
        |. PA.symbol (PA.Token "." ExpectingPeriod)
        -- |= exprParser1
        |= PA.lazy (\_ -> exprParser)
        |. PA.spaces



--variableParser : PA.LambdaParser Context Problem Expr
--variableParser =
--    text Char.isAlpha (\c -> not <| List.member c [ '.', ' ', '\n' ]) |> PA.map Var
--variableParser : PA.LambdaParser Context Problem Expr
--variableParser =
--    PT.textPS Char.isAlpha [ '.', ' ', '\n' ] |> PA.map ((\val -> val.content) >> Var)


variableParser : PA.Parser Context Problem Expr
variableParser =
    PA.variable { start = Char.isAlpha, inner = Char.isAlpha, reserved = Set.empty, expecting = ExpectingVar } |> PA.map Var


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
