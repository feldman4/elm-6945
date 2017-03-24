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
-- symbols + naming = ???
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
    , ( "( ( a ( 1 2 3 ) c ) )", "(a ( ( ? b ) 2 3 ) c)" )
    , ( "( ( a ( 1 2 3 ) c ) )", "(a ( 1 ( ?? b )  ) c)" )
      -- CFG can't handle this
    , ( "( ( a b b b b b a ) )", "( ( ? A ) ( ?? x ) ( ?? y ) ( ?? x ) ( ? A ))" )
      -- choice
    , ( "( ( 2 ( 1 2 3 ) ) )", "(( ? a ) ( 1 ( ?:choice 1 ( ? a ) ) 3 ) )" )
    , ( "( ( z ) )", "( ( ?:choice a b ( ? x ) c ) )" )
    , ( "( ( 1 ( 1 2 3 ) ) )", "(( ?:positive a ) ( ?? x ) )" )
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


{-| Initially this type was just Dict Symbol (Data a). I extended it for
pattern naming. The patternDict field refers to Matcher, so an Env type was
needed to break the type alias recursion. This extra layer is hidden by accessor
and updater functions like envGet and envInsert.
-}
type Env a b
    = Env
        { dict : Dict Symbol (Data a)
        , patternDict : Dict Symbol (Matcher a b)
        }


type Tree a
    = Branch (List (Tree a))
    | Node a


type alias Matcher a b =
    Env a b -> Succeed a b -> Data a -> Maybe b


type alias Succeed a b =
    Env a b -> Eaten -> Maybe b



--MATCHERS


listMatcher : List (Matcher a b) -> Matcher a b
listMatcher matchers env succeed data =
    case ( matchers, data ) of
        -- we matched the whole list! pass on 1 list eaten
        ( [], Branch ((Branch []) :: _) ) ->
            succeed env 1

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
                matcher env succeed2 (Branch xs)


envGet : String -> Env a b -> Maybe (Data a)
envGet key env =
    case env of
        Env env_ ->
            Dict.get key env_.dict


envInsert : String -> Data a -> Env a b -> Env a b
envInsert key value env =
    case env of
        Env env_ ->
            Env { env_ | dict = Dict.insert key value env_.dict }


segmentMatcher : Symbol -> Matcher comparable String
segmentMatcher s env succeed data =
    let
        -- new symbol encountered. make the shortest match and succeed. if that
        -- fails, succeed on longer matches until one works or we run out of data.
        eat xs =
            let
                munchOn n =
                    let
                        env2 =
                            envInsert s (Branch (List.take n xs)) env
                    in
                        case succeed env2 n of
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
                case envGet s env of
                    -- we've already matched this
                    Just (Branch ys) ->
                        if List.take (List.length ys) xs == ys then
                            succeed env (List.length ys)
                        else
                            Nothing

                    Just (Node _) ->
                        Nothing

                    Nothing ->
                        eat xs


elementMatcher : Symbol -> Matcher a b
elementMatcher =
    restrictedElementMatcher (\_ -> True)


{-| Node tags the data itself.
-}
exactMatcher : Data a -> Matcher a b
exactMatcher x env succeed data =
    case data of
        Branch (y :: _) ->
            if x == y then
                succeed env 1
            else
                Nothing

        _ ->
            Nothing



-- {-| Alternative to list matcher.
-- -}
-- choiceMatcher : List (Matcher_ a b) -> Matcher_ a b


choiceMatcher : List (Matcher a b) -> Matcher a b
choiceMatcher choices env succeed data =
    case ( choices, data ) of
        ( choice :: rest, Branch (y :: _) ) ->
            case choice env succeed (Branch [ y ]) of
                Just x ->
                    Just x

                Nothing ->
                    choiceMatcher rest env succeed data

        _ ->
            Nothing


restrictedElementMatcher : (Data a -> Bool) -> Symbol -> Matcher a b
restrictedElementMatcher restriction s env succeed data =
    case data of
        Node _ ->
            Nothing

        Branch [] ->
            Nothing

        Branch (y :: _) ->
            if restriction y then
                case envGet s env of
                    Nothing ->
                        let
                            env2 =
                                envInsert s y env
                        in
                            succeed env2 1

                    Just x ->
                        if x == y then
                            succeed env 1
                        else
                            Nothing
            else
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



-- treeToMatcher : Tree String -> Matcher_ String String
-- treeToMatcher : Tree String -> Matcher_T String b


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

        Branch ((Node "?:int") :: (Node s) :: []) ->
            restrictedElementMatcher isInt s

        Branch ((Node "?:positive") :: (Node s) :: []) ->
            restrictedElementMatcher isPositive s

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


isPositive : Tree String -> Bool
isPositive data =
    case data of
        Node x ->
            case String.toFloat x of
                Ok val ->
                    val > 0

                _ ->
                    False

        _ ->
            False


isInt : Tree String -> Bool
isInt data =
    case data of
        Node x ->
            x |> String.toInt |> isOk

        _ ->
            False


isOk : Result error value -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False



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
            model.matcherString
                |> schemeString
                |> treeToMatcher

        output =
            model.convert model.inputString
                |> viewMatch matcher

        success =
            if model.consoleSucceed then
                failToDebug
            else
                printSuccess

        emptyEnv =
            Env { dict = Dict.empty, patternDict = Dict.empty }

        viewMatch matcher data =
            matcher emptyEnv success data
                |> Maybe.withDefault ("no match for " ++ (data |> toString))
                |> (\s -> div [] [ text s ])

        exampleString ( a, b ) =
            [ "input: ", a, " pattern: ", b ] |> String.join ""

        exampleDivs =
            examples
                |> List.map (\x -> li [ onClick (Example x) ] [ x |> exampleString |> text ])
                |> ul []
                |> (\x -> div [] [ x ])

        consoleSucceed =
            div []
                [ Html.input [ type_ "checkbox", onClick ConsoleSucceed ] []
                , text "always fail, printing all matches to browser console"
                ]
    in
        div []
            [ intro
            , br [] []
            , input
            , match
            , br [] []
            , output
            , matcherParse
            , exampleDivs
            , consoleSucceed
            ]


intro : Html.Html msg
intro =
    """Enter input text and pattern.
    You have to write in scheme style, with space around inner parentheses.
    Click on an example from the list to load it. Available operators are:
      ? ?? ?:choice ?:int ?:positive.
  """ |> (\x -> div [] [ x |> text ])


type alias Model a =
    { inputString : String
    , matcherString : String
    , convert : a -> Tree a
    , consoleSucceed : Bool
    }
