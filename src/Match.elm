module Match exposing (..)

import Dict exposing (Dict)
import Html exposing (br, div, li, text, ul)
import Html.Attributes exposing (size, style, type_, value)
import Html.Events exposing (onClick, onInput)
import String exposing (endsWith, split, startsWith)


-- why are we using functions as combinators, rather than representing them with
-- symbols and compiling to function afterwards? it might save the final
-- conversion step, but makes the combination uninspectable. is adding a function
-- that respects the combinator spec easier than adding a symbol and procedure?
--
--
-- also, how is pattern matching used for type-based dispatch? what is special
-- about cons (::)? I think you can pattern match on your own cons-like functions
-- but not on constructors stored in variables - is this true in Haskell?


{-| it would be nice to use the same function to compose the data and
the pattern, as in Haskell/Elm case pattern matching
-}
examples : List ( String, String )
examples =
    [ ( "( ( a ( 1 2 3 ) ) )", "(( ? a ) ( 1 2 3 ) )" )
    , ( "( ( a ( 1 2 3 ) ) )", "(( ? a ) ( 1 2 ( ?:choice 3 ) ) )" )
    , ( "( ( a ( 1 2 3 ) c ) )", "(a ( ( ? b ) 2 3 ) c)" )
    , ( "( ( a ( 1 2 3 ) c ) )", "(a ( 1 ( ?? b )  ) c)" )
      -- CFG can't handle this
    , ( "( ( a b b b b b a ) )", "( ( ? A ) ( ?? x ) ( ?? y ) ( ?? x ) ( ? A ))" )
    ]


schemeString : String -> Tree String
schemeString s =
    let
        invalid =
            Branch [ Node "invalid" ]
    in
        s
            |> schemeToTree (\x -> Just x)
            |> Maybe.withDefault invalid



-- SUCCESS


printSuccess : Succeed a String
printSuccess a b =
    Just ("first match bound: " ++ (toString a))


failToDebug : Succeed a String
failToDebug a b =
    let
        _ =
            Debug.log "got to" a
    in
        Nothing



--TYPES


type alias Symbol =
    String


type alias Eaten =
    Int


type alias Data a =
    Tree a


type Tree a
    = Branch (List (Tree a))
    | Node a


type alias Matcher a b =
    Dict Symbol (Data a) -> Succeed a b -> Data a -> Maybe b


type alias Succeed a b =
    Dict Symbol (Data a) -> Eaten -> Maybe b



--MATCHERS


listMatcher : List (Matcher comparable String) -> Matcher comparable String
listMatcher matchers dict succeed data =
    case ( matchers, data ) of
        -- we matched the whole list! pass on 1 list eaten
        ( [], Branch ((Branch []) :: _) ) ->
            succeed dict 1

        ( _, Branch [] ) ->
            Nothing

        ( [], _ ) ->
            Nothing

        -- not OK I guess
        ( _, Node x ) ->
            Nothing

        ( _, Branch ((Node x) :: _) ) ->
            Nothing

        ( matcher :: restOfMatchers, Branch ((Branch xs) :: outerRest) ) ->
            let
                -- here is where signaling the number of items consumed is useful
                -- could we have a combinator that matches segments without this?
                succeed2 dict2 n =
                    let
                        trueRestOfData =
                            List.drop n xs |> Branch

                        anotherRest =
                            Branch (trueRestOfData :: outerRest)
                    in
                        listMatcher restOfMatchers dict2 succeed anotherRest
            in
                matcher dict succeed2 (Branch xs)


segmentMatcher : Symbol -> Matcher comparable String
segmentMatcher s dict succeed data =
    let
        -- new symbol encountered. make the shortest match and succeed. if that
        -- fails, succeed on longer matches until one works or we run out of data.
        eat xs =
            let
                munchOn n =
                    let
                        dict2 =
                            Dict.insert s (Branch (List.take n xs)) dict
                    in
                        case succeed dict2 n of
                            Just whatever ->
                                Just whatever

                            Nothing ->
                                if n == List.length xs then
                                    Nothing
                                else
                                    munchOn (n + 1)
            in
                munchOn 0
    in
        case data of
            Node x ->
                Nothing

            Branch xs ->
                case Dict.get s dict of
                    -- we've already matched this
                    Just (Branch ys) ->
                        if List.take (List.length ys) xs == ys then
                            succeed dict (List.length ys)
                        else
                            Nothing

                    Just (Node _) ->
                        Nothing

                    Nothing ->
                        eat xs


elementMatcher : Symbol -> Matcher comparable String
elementMatcher s dict succeed data =
    case data of
        Node _ ->
            Nothing

        Branch [] ->
            Nothing

        Branch (y :: _) ->
            case Dict.get s dict of
                Nothing ->
                    succeed (Dict.insert s y dict) 1

                Just x ->
                    if x == y then
                        succeed dict 1
                    else
                        Nothing


{-| Node tags the data itself.
-}
exactMatcher : Data a -> Matcher a b
exactMatcher x dict succeed data =
    case data of
        Branch (y :: _) ->
            if x == y then
                succeed dict 1
            else
                Nothing

        _ ->
            Nothing


{-| Alternative to list matcher.
-}
choiceMatcher : List (Matcher a b) -> Matcher a b
choiceMatcher choices dict succeed data =
    case ( choices, data ) of
        ( choice :: rest, Branch (y :: _) ) ->
            case choice dict succeed (Branch [ y ]) of
                Just x ->
                    Just x

                Nothing ->
                    choiceMatcher rest dict succeed data

        _ ->
            Nothing



-- CONVERSION


{-| (a) => Branch [Node a]
-}
schemeToTree : (String -> Maybe a) -> String -> Maybe (Tree a)
schemeToTree convert input =
    let
        ( openParen, closeParen ) =
            ( "(", ")" )

        isList s =
            startsWith openParen s && endsWith closeParen s

        breakList s =
            s
                |> String.dropLeft 1
                |> String.dropRight 1
                |> String.trim
                |> splitParen
                |> List.filter ((/=) "")

        cleanup xs =
            List.map (schemeToTree convert) xs |> allJust
    in
        -- either we have a primitive or a list
        if input |> isList then
            input
                |> breakList
                |> List.map (schemeToTree convert)
                |> allJust
                |> Maybe.map Branch
        else
            convert input |> Maybe.map Node


{-| Parses "a ( b c ) d" into ["a", "( b c )", "d"]. Seems too complicated.
And it still doesn't deal with "a (b c) d".
-}
splitParen : String -> List String
splitParen string =
    let
        tokens =
            String.split " " string

        ( openParen, closeParen ) =
            ( "(", ")" )

        f parenDepth n soFar toGo =
            case toGo of
                token :: restToGo ->
                    if (parenDepth == 0) && (token /= openParen) then
                        ( String.join " " (soFar ++ [ token ]), n + 1 )
                    else if token == openParen then
                        if parenDepth == 0 then
                            f (parenDepth + 1) (n + 1) soFar restToGo
                        else
                            f (parenDepth + 1) (n + 1) (soFar ++ [ token ]) restToGo
                    else if token == closeParen then
                        if parenDepth == 1 then
                            ( String.join " " ("(" :: soFar ++ [ ")" ]), n + 1 )
                        else
                            f (parenDepth - 1) (n + 1) (soFar ++ [ token ]) restToGo
                    else
                        f parenDepth (n + 1) (soFar ++ [ token ]) restToGo

                [] ->
                    ( String.join " " soFar, n )

        runner soFar toGo =
            let
                ( parsed, n ) =
                    f 0 0 [] toGo

                restToGo =
                    List.drop n toGo
            in
                if List.isEmpty restToGo then
                    (soFar ++ [ parsed ])
                else
                    runner (soFar ++ [ parsed ]) restToGo
    in
        runner [] tokens


{-| -}
treeToMatcher : Tree String -> Matcher String String
treeToMatcher parseTree =
    case parseTree of
        Node x ->
            exactMatcher parseTree

        Branch ((Node "?") :: (Node s) :: []) ->
            elementMatcher s

        Branch ((Node "??") :: (Node s) :: []) ->
            segmentMatcher s

        Branch ((Node "?:choice") :: choices) ->
            choices |> List.map treeToMatcher |> choiceMatcher

        Branch xs ->
            xs |> List.map treeToMatcher |> listMatcher


allJust : List (Maybe b) -> Maybe (List b)
allJust xs =
    let
        xs_ =
            List.filterMap identity xs
    in
        if (List.length xs) == (List.length xs_) then
            Just xs_
        else
            Nothing



-- APP


main : Program Never (Model String) Action
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.batch [])
        }


init : Model String
init =
    { inputString = "( ( a ( 1 2 3 ) ) )"
    , convert = schemeString
    , matcherString = "(( ? a ) ( 1 2 3 ) )"
    , consoleSucceed = False
    }


type Action
    = Input String
    | InputM String
    | Example ( String, String )
    | ConsoleSucceed


update : Action -> Model a -> ( Model a, Cmd msg )
update action model =
    case action of
        Input s ->
            { model | inputString = s } ! []

        InputM s ->
            { model | matcherString = s } ! []

        Example ( a, b ) ->
            { model | inputString = a, matcherString = b } ! []

        ConsoleSucceed ->
            { model | consoleSucceed = not model.consoleSucceed } ! []


view : Model String -> Html.Html Action
view model =
    let
        input =
            Html.input [ onInput Input, value model.inputString, size 40 ] []
                |> (\x -> div [] [ x ])

        match =
            Html.input [ onInput InputM, value model.matcherString, size 40 ] []
                |> (\x -> div [] [ x ])

        matcherParse =
            Html.div []
                [ model.matcherString
                    |> schemeString
                    |> toString
                    |> (++) "using : "
                    |> text
                ]

        matcher =
            model.matcherString |> schemeString |> treeToMatcher

        output =
            model.convert model.inputString
                |> viewMatch matcher

        success =
            if model.consoleSucceed then
                failToDebug
            else
                printSuccess

        viewMatch matcher data =
            matcher Dict.empty success data
                |> Maybe.withDefault ("no match for " ++ (data |> toString))
                |> (\s -> div [] [ text s ])

        exampleDivs =
            examples
                |> List.map (\x -> li [ onClick (Example x) ] [ x |> toString |> text ])
                |> ul []
                |> (\x -> div [] [ br [] [], "click on example to load:" |> text, x ])

        consoleSucceed =
            div [] [ text "always fail, printing matches to browser console", Html.input [ type_ "checkbox", onClick ConsoleSucceed ] [] ]
    in
        div []
            [ input
            , match
            , br [] []
            , output
            , matcherParse
            , exampleDivs
            , consoleSucceed
            ]


type alias Model a =
    { inputString : String
    , matcherString : String
    , convert : a -> Tree a
    , consoleSucceed : Bool
    }
