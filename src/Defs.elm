module Defs exposing (..)

import Parser exposing ((|.), (|=), Parser)
import ParserTools as PT


type alias Definition =
    ( String, String )


install : String -> List Definition -> List Definition
install str defs =
    List.foldl installOne defs (String.lines str)


installOne : String -> List Definition -> List Definition
installOne str defs =
    case parse str of
        Just def ->
            def :: defs

        Nothing ->
            defs


parse : String -> Maybe Definition
parse str =
    case Parser.run definitionParser str of
        Ok def ->
            Just def

        Err _ ->
            Nothing


definitionParser : Parser Definition
definitionParser =
    Parser.succeed (\a b -> ( a, b ))
        |= PT.word
        |. Parser.spaces
        |= lambda


lambda : Parser String
lambda =
    PT.textPS (\c -> Char.isAlpha c || c == '\\') [ '\n' ] |> Parser.map (\value -> value.content)
