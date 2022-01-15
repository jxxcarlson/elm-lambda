module Tools.Parser exposing
    ( first
    , fold
    , many
    , manyNonEmpty
    , manySeparatedBy
    , middle
    , optional
    , parenthesized
    , second
    , sequence
    , string
    , textPS
    , word
    )

import Parser exposing ((|.), (|=), Parser)


word : Parser String
word =
    textPS Char.isAlpha [ ' ' ] |> Parser.map (\value -> value.content)


string : String -> Parser String
string str =
    Parser.symbol str |> Parser.map (\_ -> str)


{-| Apply a parser zero or more times and return a list of the results.

    > run Parser.int "7"
    Ok 7 : Result (List Parser.DeadEnd) Int

    > run (first Parser.int Parser.spaces) "7 "
    Ok 7 : Result (List Parser.DeadEnd) Int

    > int_ = first Parser.int Parser.spaces
    Parser <function> : Parser.Parser Int

    > run (many int_) "1 2 3"
    Ok [1,2,3]

-}
many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


{-|

    > run (manySeparatedBy Parser.spaces Parser.int) "1 2 3"
    Ok [1,2,3]

    > comma = first (Parser.symbol ",") Parser.spaces
    Parser <function> : Parser.Parser ()

    > run (manySeparatedBy comma Parser.int) "1, 2,    3"
    Ok [1,2,3]

-}
manySeparatedBy : Parser () -> Parser a -> Parser (List a)
manySeparatedBy sep p =
    manyNonEmpty_ p (second sep p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ Parser.end |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        , Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]


manyNonEmpty : Parser a -> Parser (List a)
manyNonEmpty p =
    p
        |> Parser.andThen (\x -> manyWithInitialList [ x ] p)


manyNonEmpty_ : Parser a -> Parser a -> Parser (List a)
manyNonEmpty_ p q =
    p
        |> Parser.andThen (\x -> manyWithInitialList [ x ] q)


manyWithInitialList : List a -> Parser a -> Parser (List a)
manyWithInitialList initialList p =
    Parser.loop initialList (manyHelp p)


{-| Running `optional p` means run p, but if it fails, succeed anyway
-}
optional : Parser () -> Parser ()
optional p =
    Parser.oneOf [ p, Parser.succeed () ]


{-| running `first p q` means run p, then run q
and return the result of running p.
-}
first : Parser a -> Parser b -> Parser a
first p q =
    p |> Parser.andThen (\x -> q |> Parser.map (\_ -> x))


{-| running `second p q` means run p, then run q
and return the result of running q.
-}
second : Parser a -> Parser b -> Parser b
second p q =
    p |> Parser.andThen (\_ -> q)



{-

   > parenthesized p = middle (Parser.symbol "(") p (Parser.symbol ")")
   <function> : Parser.Parser b -> Parser.Parser b

   > pint = parenthesized Parser.int
   Parser <function> : Parser.Parser Int

   > run pint "(2)"
   Ok 2 : Result (List Parser.DeadEnd) In

-}


middle : Parser a -> Parser b -> Parser c -> Parser b
middle p q r =
    first (second p q) r


{-| textPS = "text prefixText stopCharacters": Get the longest string
whose first character satisfies the prefixTest and whose remaining
characters are not in the list of stop characters. LambdaTest:

    line =
        textPS (\c -> Char.isAlpha) [ '\n' ]

recognizes lines that start with an alphabetic character.

-}
textPS : (Char -> Bool) -> List Char -> Parser { start : Int, finish : Int, content : String }
textPS prefixTest stopChars =
    Parser.succeed (\start finish content -> { start = start, finish = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefixTest c)
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


{-|

    > run (sequence [string "foo", string " ", string "bar"]) "foo bar"
    Ok ["foo"," ","bar"]

    > type Expr = String String | Int Int

    > stringExpr str = (first (string str) Parser.spaces) |> Parser.map String
    <function> : String -> Parser.Parser Expr

    > run (stringExpr "a") "a b c"
    Ok (String "a") : Result (List Parser.DeadEnd) Expr

    > intExpr = first Parser.int Parser.spaces |> Parser.map Int
    Parser <function> : Parser.Parser Int

    > intExpr = (first Parser.int Parser.spaces) |> Parser.map Int
    Parser <function> : Parser.Parser Expr

    > xOrY = Parser.oneOf [stringExpr "x", stringExpr "y"]
    Parser <function> : Parser.Parser Expr

    > run (sequence [xOrY, intExpr]) "x 2"
    Ok

    > run (many (sequence [xOrY, intExpr])) "x 1 y 2 x 3 x 4 y 5"
    Ok [[String "x",Int 1],[String "y",Int 2],[String "x",Int 3],[String "x",Int 4],[String "y",Int 5]]
        : Result (List Parser.DeadEnd) (List (List Expr))

-}
sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    Parser.loop { parsers = parsers, results = [] } sequenceAux


type alias State a =
    { parsers : List (Parser a), results : List a }


sequenceAux : State a -> Parser (Parser.Step (State a) (List a))
sequenceAux state =
    case List.head state.parsers of
        Nothing ->
            Parser.succeed () |> Parser.map (\_ -> Parser.Done (List.reverse state.results))

        Just parser ->
            parser |> Parser.map (\a -> Parser.Loop { state | results = a :: state.results, parsers = List.drop 1 state.parsers })


type alias FoldState a =
    { init : Maybe a, acc : Maybe a }


fold : (a -> a -> a) -> Parser a -> Parser (Maybe a)
fold f p =
    Parser.loop { init = Nothing, acc = Nothing } (foldAux f p)


foldAux : (a -> a -> a) -> Parser a -> FoldState a -> Parser (Parser.Step (FoldState a) (Maybe a))
foldAux f p state =
    Parser.oneOf
        [ Parser.succeed (\a -> Parser.Loop (update f a state))
            |= p
        , Parser.succeed () |> Parser.map (\_ -> Parser.Done state.acc)
        ]


update : (a -> a -> a) -> a -> FoldState a -> FoldState a
update f a state =
    case ( state.init, state.acc ) of
        ( Nothing, _ ) ->
            { state | init = Just a }

        ( Just firstA, Nothing ) ->
            { state | acc = Just (f a firstA) }

        ( Just _, Just b ) ->
            { state | acc = Just (f a b) }


leftParen =
    Parser.symbol "("


rightParen =
    Parser.symbol ")"


parenthesized p =
    middle leftParen p rightParen
