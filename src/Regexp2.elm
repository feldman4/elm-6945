module Regexp2 exposing (..)

import Html exposing (text, div, ul, br, ol, li)
import Regex


-- TESTS


testText : List String
testText =
    """abc
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
dogdogcatdogdog
catcatcatdogdogdog
acatdogdogcats
ifacatdogdogs
acatdogdogsme
[][[^]]]
^][].[][^""" |> String.split "\n"


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


dummy : number
dummy =
    3



-- PRINTING


charNeedsQuoting : Char -> Bool
charNeedsQuoting char =
    let
        isSpecial =
            (flip String.contains) "?\\.^$[]*{}-"
    in
        char |> String.fromChar |> isSpecial


quote : Char -> String
quote char =
    if charNeedsQuoting char then
        "\\" ++ (String.fromChar char)
    else
        String.fromChar char


group : String -> String
group string =
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


bracket : String -> String -> String
bracket prefix string =
    let
        contents =
            String.toList string |> List.map quote |> String.concat
    in
        "[" ++ prefix ++ contents ++ "]"


repeatToInterval : RepeatInfo -> String
repeatToInterval { expr, start, end } =
    let
        endString =
            case end of
                Just x ->
                    toString x

                Nothing ->
                    ""
    in
        [ "{", (toString start), ",", endString, "}" ] |> String.concat


print : Expr -> String
print expr =
    case expr of
        Atom char ->
            quote char

        From string ->
            bracket "" string

        NotFrom string ->
            bracket "^" string

        Seq seq ->
            seq |> List.map print |> String.concat

        Alt seq ->
            seq |> List.map print |> String.join "|"

        Repeat repeatInfo ->
            let
                repeatString =
                    case ( repeatInfo.start, repeatInfo.end ) of
                        ( 0, Nothing ) ->
                            "*"

                        ( 1, Nothing ) ->
                            "+"

                        ( start, Just end ) ->
                            if start == end then
                                "{" ++ (toString start) ++ "}"
                            else
                                repeatToInterval repeatInfo

                        _ ->
                            repeatToInterval repeatInfo

                exprString =
                    print repeatInfo.expr
                        |> if needsGrouping repeatInfo.expr then
                            group
                           else
                            identity
            in
                exprString ++ repeatString

        BackRef n ->
            "\\" ++ toString n

        Group expr ->
            print expr |> group

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
        testRegex r =
            testText
                |> List.filter (Regex.contains (Regex.regex (print r)))

        viewList xs =
            xs |> List.map (\x -> li [] [ x |> text ])

        viewRegex r =
            let
                a =
                    Debug.log "regex" (r |> print)
                        |> (\_ -> Debug.log "internal representation" r)
            in
                div []
                    [ div [] [ "regex: " ++ (r |> print) |> text ]
                    , div [] [ "matches: " |> text, div [] (r |> testRegex |> viewList) ]
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
