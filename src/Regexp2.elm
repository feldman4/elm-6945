module Regexp2 exposing (..)

import Html exposing (text, div, ul, br, ol, li)
import Regex


-- TESTS


testText : List String
testText =
    """
abc
aac
acc
zzzaxcqqq
abdabec
foo
barbar
foo bar baz quux
anything containing them
catdogcat
catcatdogdog
acatdogdogcats
[][[^]]]
^][].[][^
???
abc{
""" |> String.split "\n" |> List.filter ((/=) "")


tests : List Expr
tests =
    [ Alt
        [ Seq [ Atom 'a', Dot, Atom 'c', Atom '{' ]
        , Repeat (RepeatInfo (Atom '?') 3 Nothing)
        ]
    , exactly "dog" |> atLeast 2
    , Seq [ Bol, exactly "cat", exactly "dog" |> atLeast 1 ]
    , Seq [ NotFrom "aeiou" |> repeat 3, From "aeiou" ]
    , Seq [ Atom '[', From "][^-" |> atLeast 4, Atom ']' ]
    , Seq [ exactly "[", NotFrom "^" |> between 1 4, exactly "]" ]
    , Seq [ Group (Dot |> star), BackRef 1, Eol ]
    ]



-- TYPES


{-|
Seq and Alt are combinators.
-}
type Expr
    = Atom Char
    | From String
    | NotFrom String
    | Seq (List Expr)
    | Alt (List Expr)
    | Group Expr
    | Repeat RepeatInfo
    | BackRef Int
    | Dot
    | Eol
    | Bol


type RE
    = BRE
    | ERE


type alias RepeatInfo =
    { expr : Expr
    , start : Int
    , end : Maybe Int
    }



-- HELPERS


exactly : String -> Expr
exactly string =
    string |> String.toList |> List.map Atom |> Seq


interval : Int -> Maybe Int -> Expr -> Expr
interval start end expr =
    Repeat (RepeatInfo expr start end)


between : Int -> Int -> Expr -> Expr
between start end expr =
    interval start (Just end) expr


atLeast : Int -> Expr -> Expr
atLeast start expr =
    interval start Nothing expr


repeat : Int -> Expr -> Expr
repeat n expr =
    interval n (Just n) expr


plus : Expr -> Expr
plus expr =
    expr |> interval 1 Nothing


star : Expr -> Expr
star expr =
    expr |> interval 0 Nothing



-- PRINTING


{-| For quoting outside brackets.
-}
charNeedsQuoting : RE -> Char -> Bool
charNeedsQuoting re char =
    let
        special =
            case re of
                BRE ->
                    ".[\\" ++ ""

                ERE ->
                    "?\\.^$[]*{}-"

        isSpecial =
            (flip String.contains) special
    in
        char |> String.fromChar |> isSpecial


quote : RE -> Char -> String
quote re char =
    if charNeedsQuoting re char then
        "\\" ++ (String.fromChar char)
    else
        String.fromChar char


group : RE -> String -> String
group re string =
    case re of
        BRE ->
            "\\(" ++ string ++ "\\)"

        ERE ->
            "(" ++ string ++ ")"


needsGrouping : Expr -> Bool
needsGrouping expr =
    case expr of
        Seq _ ->
            True

        Alt _ ->
            True

        Repeat _ ->
            True

        _ ->
            False


bracket : RE -> String -> String -> String
bracket re prefix string =
    let
        contents =
            String.toList string |> List.map (quote re) |> String.concat
    in
        "[" ++ prefix ++ contents ++ "]"


repeatToInterval : RE -> RepeatInfo -> String
repeatToInterval re { expr, start, end } =
    let
        endString =
            case end of
                Just x ->
                    toString x

                Nothing ->
                    ""

        ( leftBrace, rightBrace ) =
            case re of
                BRE ->
                    ( "\\{", "\\}" )

                ERE ->
                    ( "{", "}" )
    in
        [ leftBrace, (toString start), ",", endString, rightBrace ] |> String.concat


{-| Special repeat characters are different in ERE and BRE.
-}
specialRepeat : RE -> RepeatInfo -> String
specialRepeat re repeatInfo =
    let
        ( start, end ) =
            ( repeatInfo.start, repeatInfo.end )

        finite start_ end_ =
            if start_ == end_ then
                "{" ++ (toString start_) ++ "}"
            else
                repeatToInterval re repeatInfo
    in
        case ( start, end ) of
            ( 0, Nothing ) ->
                "*"

            ( 1, Nothing ) ->
                "+"

            ( 0, Just 1 ) ->
                case re of
                    BRE ->
                        finite 0 1

                    ERE ->
                        "?"

            ( start, Just end ) ->
                finite start end

            _ ->
                repeatToInterval re repeatInfo


unescapeStar : String -> String
unescapeStar expr =
    case expr |> String.toList of
        '^' :: '\\' :: '*' :: rest ->
            "^*" ++ (rest |> String.fromList)

        '\\' :: '*' :: rest ->
            "*" ++ (rest |> String.fromList)

        _ ->
            expr


{-| Print the expression tree, then apply any final processing.
-}
printer : RE -> Expr -> String
printer re expr =
    let
        result =
            print re expr

        finalFilter =
            case re of
                BRE ->
                    unescapeStar

                ERE ->
                    identity
    in
        result |> finalFilter


print : RE -> Expr -> String
print re expr =
    case expr of
        Atom char ->
            quote re char

        From string ->
            bracket re "" string

        NotFrom string ->
            bracket re "^" string

        Seq seq ->
            seq |> List.map (print re) |> String.concat

        Alt seq ->
            let
                pipe =
                    case re of
                        BRE ->
                            "\\|"

                        ERE ->
                            "|"
            in
                seq |> List.map (print re) |> String.join pipe

        Repeat repeatInfo ->
            let
                repeatString =
                    specialRepeat re repeatInfo

                exprString =
                    print re repeatInfo.expr
                        |> if needsGrouping repeatInfo.expr then
                            (group re)
                           else
                            identity
            in
                exprString ++ repeatString

        BackRef n ->
            case re of
                BRE ->
                    "\\" ++ toString n

                ERE ->
                    "ILLEGAL BACKREFERENCE IN ERE"

        Group expr ->
            print re expr |> group re

        Dot ->
            "."

        Bol ->
            "^"

        Eol ->
            "$"



-- VIEW


main : Html.Html msg
main =
    let
        testRegex re r =
            testText
                |> List.filter (Regex.contains (Regex.regex (print re r)))

        viewList xs =
            xs |> List.map (\x -> li [] [ x |> text ])

        viewRegex r =
            let
                a =
                    Debug.log "regex (BRE)" (r |> print BRE)
                        |> (\_ -> Debug.log "regex (ERE)" (r |> print ERE))
                        |> (\_ -> Debug.log "internal representation" r)
            in
                div []
                    [ div [] [ "regex (BRE): " ++ (r |> print BRE) |> text ]
                    , div [] [ "regex (ERE): " ++ (r |> print ERE) |> text ]
                    , div [] [ "matches (ERE): " |> text, div [] (r |> testRegex ERE |> viewList) ]
                    , br [] []
                    ]

        divTests =
            (tests |> List.map viewRegex) |> div []
    in
        div []
            ([ div [] [ "Check the console to see the internal representation of each regex." |> text ]
             , br [] []
             , divTests
             , div [] [ "tested on:" |> text ]
             , ol [] (testText |> viewList)
             ]
            )
