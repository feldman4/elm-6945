module Match exposing (..)

import Html exposing (text, div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Dict exposing (Dict)
import String exposing (startsWith, endsWith, split)


-- why are we using functions as combinators, rather than representing them with
-- symbols and compiling to function afterwards? it might save the final
-- conversion step, but makes the combination uninspectable. is adding a function
-- that respects the combinator spec easier than adding a symbol and procedure?
--
--
-- also, how is pattern matching used for type-based dispatch? what is special
-- about cons (::)? I think you can pattern match on your own cons-like functions
-- but not on constructors stored in variables - is this true in Haskell?


main2 : Html.Html msg
main2 =
    let
        viewMatch matcher data =
            matcher Dict.empty failToDebug data
                |> Maybe.withDefault ("no match for " ++ (data |> toString))
                |> (\s -> div [] [ text s ])
    in
        [ ( exampleMatcher, stringToData "abc" )
          -- , ( exampleMatcher2, stringToData "abbbbca" )
          -- , ( exampleMatcher2, schemeString "(a (c d) a)" )
          -- ,
        ]
            |> List.map (\( a, b ) -> viewMatch a b)
            |> div []


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
    { inputString = "(a b b c)"
    , convert = schemeString
    , matcherString = "(a ( ?? b ) c)"
    }


type Action
    = Input String
    | InputM String


update : Action -> Model a -> ( Model a, Cmd msg )
update action model =
    case action of
        Input s ->
            { model | inputString = s } ! []

        InputM s ->
            { model | matcherString = s } ! []


view : Model String -> Html.Html Action
view model =
    let
        input =
            Html.input [ onInput Input, value model.inputString ] []

        match =
            Html.input [ onInput InputM, value model.matcherString ] []

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

        viewMatch matcher data =
            matcher Dict.empty printSuccess data
                |> Maybe.withDefault ("no match for " ++ (data |> toString))
                |> (\s -> div [] [ text s ])
    in
        div [] [ input, match, output, matcherParse ]


type alias Model a =
    { inputString : String
    , matcherString : String
    , convert : a -> Tree a
    }


schemeString : String -> Tree String
schemeString s =
    let
        invalid =
            Branch [ Node "invalid" ]
    in
        s
            |> schemeToTree (\x -> Just x)
            |> Maybe.withDefault invalid


exampleData : Tree number
exampleData =
    Branch [ Node 3, Node 4, Node 7 ]


stringToData : String -> Tree Symbol
stringToData s =
    s
        |> String.split ""
        |> List.map Node
        |> Branch



-- sort of nice when you can use the same function to compose the data and
-- the pattern


exampleMatcher : Matcher String String
exampleMatcher =
    listMatcher [ exactMatcher "a", segmentMatcher "b", exactMatcher "c" ]


{-| CFG can't handle this
-}
exampleMatcher2 : Matcher Symbol String
exampleMatcher2 =
    listMatcher
        [ elementMatcher "A"
        , segmentMatcher "x"
        , segmentMatcher "y"
        , segmentMatcher "x"
        , elementMatcher "A"
        ]


exampleMatcher3 : Matcher String String
exampleMatcher3 =
    listMatcher [ segmentMatcher "x" ]


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


{-| Node tags data, Branch tags a list of Nodes or Branches
-}
type Tree a
    = Branch (List (Tree a))
    | Node a


type alias Matcher a b =
    Dict Symbol (Data a) -> Succeed a b -> Data a -> Maybe b


type alias Succeed a b =
    Dict Symbol (Data a) -> Eaten -> Maybe b


listMatcher : List (Matcher comparable String) -> Matcher comparable String
listMatcher matchers dict succeed data =
    case ( matchers, data ) of
        -- this is OK, pass on 0?
        ( [], Branch [] ) ->
            succeed dict 0

        ( _, Branch [] ) ->
            Nothing

        ( [], _ ) ->
            Nothing

        -- not OK I guess
        ( _, Node x ) ->
            Nothing

        ( matcher :: restOfMatchers, Branch xs ) ->
            let
                -- here is where signaling the number of items consumed is useful
                -- could we have a combinator that matches segments without this?
                succeed2 dict2 n =
                    let
                        trueRestOfData =
                            List.drop n xs
                    in
                        listMatcher restOfMatchers dict2 succeed (Branch trueRestOfData)
            in
                matcher dict succeed2 data


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
exactMatcher : a -> Matcher a b
exactMatcher x dict succeed data =
    case data of
        Branch (y :: _) ->
            case y of
                Node z ->
                    if x == z then
                        succeed dict 1
                    else
                        Nothing

                Branch _ ->
                    Nothing

        _ ->
            Nothing


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


test : List String
test =
    splitParen "a ( b ) c" |> Debug.log "testparen"


{-| Parses "a ( b c ) d" into ["a", "( b c )", "d"]. Seems too complicated.
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
                        f (parenDepth + 1) (n + 1) soFar restToGo
                    else if token == closeParen then
                        if parenDepth == 1 then
                            ( String.join " " ("(" :: soFar ++ [ ")" ]), n + 1 )
                        else
                            f (parenDepth - 1) (n + 1) soFar restToGo
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
            exactMatcher x

        Branch ((Node "?") :: (Node s) :: []) ->
            elementMatcher s

        Branch ((Node "??") :: (Node s) :: []) ->
            segmentMatcher (s |> Debug.log "segmentMatcher")

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


insertNoReplace : comparable -> v -> Dict comparable v -> Maybe (Dict comparable v)
insertNoReplace k v dict =
    case Dict.get k dict of
        Just x ->
            Nothing

        Nothing ->
            Just (Dict.insert k v dict)
