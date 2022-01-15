module Lambda.Defs exposing (dictionary, show)

{-| This module provides the means (via function dictionary) of constructing
a dictionary of terms to be used by the rewriting functions.

@docs dictionary, show

-}

import Dict exposing (Dict)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Tools.Parser as PT


type alias Definition =
    ( String, String )


definitions =
    """
id        \\x.x
pair      \\x.\\y.\\f.f x y
first     \\p.p(\\x.\\y.x)
second    \\p.p(\\x.\\y.y)

# Booleans
true      \\x.\\y.x
false     \\x.\\y.y
and       \\p.\\q.p q p
or        \\p.\\q.p p q
not       \\p.p (false) true

# Church numerals
zero      \\s.\\z.z
one       \\s.\\z.s z
two       \\s.\\z.s s z
three     \\s.\\z.s s s z
isZero    \\n.n (\\x.\\s.\\z.z) \\x.\\y.x
succ      \\n.\\f.\\x.f(n f x)
"""


{-| Construct a string representation of the given dictionary.
-}
show : Dict String String -> String
show dict =
    dict |> Dict.toList |> List.foldl (\( a, b ) acc -> acc ++ "\n" ++ a ++ ": " ++ b) ""


removeComments : String -> String
removeComments str =
    str
        |> String.lines
        |> List.map (String.split "#")
        |> List.map List.head
        |> Maybe.Extra.values
        |> String.join "\n"


expand1 : List Definition -> List Definition
expand1 defs =
    let
        dict =
            Dict.fromList defs
    in
    List.map (resolve (unresolved_ defs) dict) defs


expand : List Definition -> List Definition
expand defs =
    case unresolved_ defs of
        [] ->
            defs

        unresolved ->
            expand (expandAux unresolved defs)


expandAux : List String -> List Definition -> List Definition
expandAux unresolved defs =
    let
        dict =
            Dict.fromList defs
    in
    List.map (resolve unresolved dict) defs


unresolved_ defs =
    let
        terms =
            List.map Tuple.first defs

        values =
            List.map Tuple.second defs
    in
    List.foldl (\term acc -> addTerm term values acc) [] terms


resolve : List String -> Dict String String -> Definition -> Definition
resolve unresolved dict (( name, value ) as def) =
    let
        replace word target =
            case Dict.get word dict of
                Just replacement ->
                    String.replace word replacement target

                Nothing ->
                    target

        newValue =
            List.foldl (\word acc -> replace word acc) value unresolved
    in
    ( name, newValue )


addTerm : String -> List String -> List String -> List String
addTerm term terms additions =
    if List.any (\item -> String.contains term item) terms then
        if not (List.member term additions) then
            term :: additions

        else
            additions

    else
        additions


install : String -> List Definition -> List Definition
install str defs =
    List.foldl installOne defs (String.lines (removeComments str)) |> expand


{-| Construct a dictionary of pairs for rewrite rules from an input string.
-}
dictionary : String -> Dict String String
dictionary str =
    Dict.fromList (install str [])


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
    PT.textPS (\c -> Char.isAlpha c || c == '\\' || c == '(') [ '\n' ] |> Parser.map (\value -> value.content)
