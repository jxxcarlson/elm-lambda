module Problem exposing (..)


type Problem
    = ExpectingPrefix
    | ExpectingBackslash
    | ExpectingPeriod
    | ExpectingSymbol String
    | EndOfInput
    | UnHandledError
    | ExpectingLParen
    | ExpectingRParen
    | ExpectingVar


type Context
    = TextExpression
