module Witch exposing (..)

import Html exposing (text, div, ol, li)
import Html.Attributes exposing (class, style)
import List.Extra


main : Html.Html msg
main =
    let
        ball =
            [ ( [ One ], [ MarkedPoint One ] )
            , ( [ Two ], [ MarkedPoint Two ] )
            , ( [ Three ], [ MarkedPoint Three ] )
            ]

        ballTwo_0 =
            [ ( [ One ], [] )
            , ( [ Two ], [ MarkedPoint Two ] )
            , ( [ Three ], [] )
            ]

        ballTwo_1 =
            [ ( [ One ], [ MarkedPoint One ] )
            , ( [ Two ], [] )
            , ( [ Three ], [ MarkedPoint Three ] )
            ]

        ballTwo =
            [ ( [ One, Two, Three ]
              , [ Attachment ballTwo_0, Attachment ballTwo_1 ]
              )
            ]

        ballTwoTwo =
            [ ( [ One ], [ MarkedPoint One, MarkedPoint Two ] )
            , ( [ Two ], [ MarkedPoint Three, MarkedPoint Four ] )
            ]

        count =
            findAllMarkedPoints ballTwo |> Debug.log "count"

        -- list seams that can collide
        collide =
            findCollisions ballTwoTwo
                |> List.sortBy toNumberString
                |> Debug.log "collide"

        w22 =
            degenerate ballTwoTwo
                |> Debug.log "W22"
                |> List.map (\x -> li [] [ x |> drawBall ])
    in
        div []
            [ ol []
                [ li [] [ ball |> drawBall ]
                , li [] [ ballTwo |> drawBall ]
                ]
            , ol []
                w22
            ]


collide : List Collision
collide =
    [ -- twelve outcomes
      [ ( [ One ], [ MarkedPoint One ] ), ( [ Two ], [ MarkedPoint Two ] ), ( [ Three ], [ MarkedPoint Three ] ) ]
      -- three outcomes
    , [ ( [ One ], [ MarkedPoint One ] ), ( [ Two ], [ MarkedPoint Two ] ) ]
      -- no outcomes
    , [ ( [ One ], [ MarkedPoint One ] ) ]
      -- three outcomes
    , [ ( [ Two ], [ MarkedPoint Two ] ), ( [ Three ], [ MarkedPoint Three ] ) ]
      -- no outcomes
    , [ ( [ Two ], [ MarkedPoint Two ] ) ]
      -- no outcomes
    , [ ( [ Three ], [ MarkedPoint Three ] ) ]
    ]


base22 : List (List String)
base22 =
    [ [ "a", "b" ] ]


test : List (List (List String))
test =
    base22
        |> allOrderings (toString)
        |> List.Extra.unique
        |> Debug.log "test"


testOutput2 : List (List (List String))
testOutput2 =
    -- single seam
    [ -- not allowed? trivial bracketing
      [ [ "a", "b", "c", "d" ] ]
      -- octagon
    , [ [ "a", "b", "c" ], [ "d" ] ]
      -- diamond
    , [ [ "a", "b" ], [ "c", "d" ] ]
      -- square
    , [ [ "a", "b" ], [ "c" ], [ "d" ] ]
      -- octagon
    , [ [ "a" ], [ "b", "c", "d" ] ]
      -- square
    , [ [ "a" ], [ "b", "c" ], [ "d" ] ]
      -- square
    , [ [ "a" ], [ "b" ], [ "c", "d" ] ]
      -- pentagon
    , [ [ "a" ], [ "b" ], [ "c" ], [ "d" ] ]
    , [ [ "a", "c" ], [ "b", "d" ] ]
    , [ [ "a", "c" ], [ "b" ], [ "d" ] ]
    , [ [ "a" ], [ "c" ], [ "b", "d" ] ]
    , [ [ "a" ], [ "c" ], [ "b" ], [ "d" ] ]
    , [ [ "a", "c", "d" ], [ "b" ] ]
    , [ [ "a", "c" ], [ "d" ], [ "b" ] ]
    , [ [ "a" ], [ "c", "d" ], [ "b" ] ]
    , [ [ "a" ], [ "c" ], [ "d" ], [ "b" ] ]
    , [ [ "c" ], [ "a", "b", "d" ] ]
    , [ [ "c" ], [ "a", "b" ], [ "d" ] ]
    , [ [ "c" ], [ "a" ], [ "b", "d" ] ]
    , [ [ "c" ], [ "a" ], [ "b" ], [ "d" ] ]
    , [ [ "c" ], [ "a", "d" ], [ "b" ] ]
    , [ [ "c" ], [ "a" ], [ "d" ], [ "b" ] ]
    , [ [ "c", "d" ], [ "a", "b" ] ]
    , [ [ "c", "d" ], [ "a" ], [ "b" ] ]
    , [ [ "c" ], [ "d" ], [ "a", "b" ] ]
    , [ [ "c" ], [ "d" ], [ "a" ], [ "b" ] ]
    ]


{-| Reduce dimensionality by one.
[ 0. use the seam tree ]
1. find all seam collisions
2. for each seam collision, find new balls
  1. all valid orderings of points (marked or attachment)

-}
degenerate : Ball -> List Ball
degenerate ball =
    ball
        |> findCollisions
        |> List.concatMap (resolveCollision ball)


{-| only looks for base level collisions. need to analyze the whole tree.
-}
findCollisions : Ball -> List Collision
findCollisions ball =
    partialOrderings ball


{-| need to exclude trivial collisions.
-}
resolveCollision : Ball -> Collision -> List Ball
resolveCollision ball collision =
    let
        -- assume the points are unique coming in, so we can repack seams
        x : List (List (List Point))
        x =
            collision
                |> List.map Tuple.second
                |> allOrderings (toString)

        transferSeams : Ball -> List (List Point) -> List Seam
        transferSeams ball groups =
            ball
                |> List.map Tuple.first
                |> List.map2 (\g n -> ( n, g )) groups
    in
        x |> List.map (transferSeams ball)


{-| [a, b, c] => [[a], [b], [c], [a, b], [b, c], [a, b, c]]
-}
partialOrderings : List a -> List (List a)
partialOrderings elements =
    case elements of
        [] ->
            [ [] ]

        x :: [] ->
            [ [ x ] ]

        x :: xs ->
            elements
                |> List.Extra.inits
                |> List.filter (\ys -> (List.length ys) > 0)
                |> (++) (partialOrderings xs)


{-| get valid orderings of colliding points. does not filter out trivial case.
-}
allOrderings : (a -> comparable) -> List (List a) -> List (List (List a))
allOrderings compare groups =
    let
        values =
            groups |> List.concatMap identity

        deepCompare xs =
            List.map (List.map compare) xs
    in
        values
            |> List.Extra.permutations
            |> List.filter (inOrder compare groups)
            |> List.concatMap toSubSequences
            |> List.map (List.map (List.sortBy compare))
            |> List.Extra.uniqueBy deepCompare


inOrder : (a -> comparable) -> List (List a) -> List a -> Bool
inOrder compare groups values =
    let
        -- for each group, the members must be in order
        -- can't really tell if elements are unique...
        groupIsOk g =
            let
                members =
                    values |> List.filter (\x -> List.member x g)
            in
                (List.sortBy compare members) == members
    in
        groups |> List.all groupIsOk


toSubSequences : List a -> List (List (List a))
toSubSequences elements =
    let
        open : List (List a) -> List a -> List a -> List (List (List a))
        open soFar current toGo =
            case ( toGo, current ) of
                -- end of the line
                ( [], [] ) ->
                    [ soFar ]

                -- end of the line, something in the register
                ( [], y :: _ ) ->
                    [ soFar ++ [ current ] ]

                -- midway, something in the register
                ( x :: xs, y :: _ ) ->
                    -- expand register
                    (open soFar (current ++ [ x ]) xs)
                        -- or close it and make a new one
                        ++
                            (open (soFar ++ [ current ]) [ x ] xs)

                -- midway, nothing in the register
                ( x :: xs, [] ) ->
                    open soFar [ x ] xs
    in
        open [] [] elements



-- TYPES


type Tree
    = Branch (List Tree)
    | Leaf Int


type Number
    = One
    | Two
    | Three
    | Four


{-| maybe this should be (Ball3 a) = {one: a, two: a, three: a}. represent
collisions as (Ball3 Bool).
-}
type alias Ball =
    List Seam


type alias Seam =
    ( List Number, List Point )


type Point
    = MarkedPoint Number
    | Attachment Ball


type alias Collision =
    List Seam



-- INSPECT


findAllMarkedPoints : Ball -> List Number
findAllMarkedPoints ball =
    let
        getPoints point =
            case point of
                MarkedPoint n ->
                    [ n ]

                Attachment ball ->
                    findAllMarkedPoints ball

        lookInSeam ( _, points ) =
            List.concatMap getPoints points

        results =
            ball |> List.concatMap lookInSeam
    in
        results



-- VIEW


blackBorder : Html.Attribute msg
blackBorder =
    style [ ( "border", "2px solid black" ) ]


ballMargin : Html.Attribute msg
ballMargin =
    style [ ( "margin", "5px" ) ]


drawPoint : Point -> Html.Html msg
drawPoint point =
    case point of
        MarkedPoint number ->
            div [] [ number |> toLetter |> text ]

        Attachment ball ->
            div [] [ drawBall ball ]


drawBall : Ball -> Html.Html msg
drawBall ball =
    let
        seams =
            List.map drawSeam ball
    in
        div [ blackBorder, ballMargin ] seams


drawSeam : Seam -> Html.Html msg
drawSeam ( numbers, points ) =
    let
        points_ =
            List.map drawPoint points

        numbers_ =
            numbers |> List.map toNumber |> String.join "-"
    in
        div [ class numbers_ ] points_


div1 : List (Html.Attribute msg) -> Html.Html msg -> Html.Html msg
div1 attributes content =
    div attributes [ content ]


replaceString : String -> String -> String -> String
replaceString s1 s2 s =
    String.split s1 s |> String.join s2


toNumberString : a -> String
toNumberString a =
    a
        |> toString
        |> replaceString "One" "1"
        |> replaceString "Two" "2"
        |> replaceString "Three" "3"


toLetter : Number -> String
toLetter number =
    case number of
        One ->
            "a"

        Two ->
            "b"

        Three ->
            "c"

        Four ->
            "d"


toNumber : Number -> String
toNumber number =
    case number of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"
