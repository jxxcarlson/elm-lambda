module LambdaParser exposing (..)

-- https://mattwetmore.me/posts/parsing-combinators-with-parser-combinators
-- https://lambdacalc.io/

import Lambda exposing (Expr(..))
import Parser.Advanced as PA exposing ((|.), (|=))
import ParserToolsAdvanced as PT
import Problem exposing (Context, Problem(..))
import Set


type alias Parser a =
    PA.Parser Context Problem a


{-|

    > PA.run applicationParser "\\x.x(\\y.y)"
    Ok (Just (Apply (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))))

-}



--applicationParser =
--    PT.fold (\a b -> Apply b a) expressionParser


exprParser =
    expressionParser |> PA.andThen applicationParser2


applicationParser2 aInitial =
    PT.foldWithInitialValue (\a b -> Apply b a) expressionParser aInitial


expressionParser =
    PA.oneOf
        [ parenthesized (PA.lazy (\_ -> expressionParser))
        , PA.lazy (\_ -> abstractionParser)
        , variableParser
        ]


leftParen =
    PA.symbol (PA.Token "(" ExpectingLParen)


rightParen =
    PA.symbol (PA.Token ")" ExpectingRParen)


parenthesized p =
    PT.middle leftParen p rightParen


applicationParser1 : PA.Parser Context Problem Expr
applicationParser1 =
    PA.succeed (\e1 e2 -> Apply e1 e2)
        |= abstractionParser
        |. PA.spaces
        |= expressionParser
        |. PA.spaces


abstractionParser =
    PA.succeed (\var expr -> Lambda var expr)
        |. PA.symbol (PA.Token "\\" ExpectingBackslash)
        |= rawVariableParser
        |. PA.symbol (PA.Token "." ExpectingPeriod)
        |= expressionParser
        |. PA.spaces



--variableParser : PA.Parser Context Problem Expr
--variableParser =
--    text Char.isAlpha (\c -> not <| List.member c [ '.', ' ', '\n' ]) |> PA.map Var
--variableParser : PA.Parser Context Problem Expr
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



--type alias ErrorData =
--    List (LambdaParser.Advanced.DeadEnd Context Problem)
--type Step state a
--    = Loop state
