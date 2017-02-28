module WFC exposing (..)

import Html exposing (text)
import Array exposing (Array)
import Dict exposing (Dict)


type alias Wave =
    Array Point


type alias Point =
    Array Bool


type alias Propagator =
    IndexS -> IndexS -> Edge -> Bool


{-| type variable should be `comparable`
-}
type alias Selector a =
    Point -> Maybe a


type alias Collapser =
    Point -> Maybe Point


type alias IndexS =
    Int


type alias IndexW =
    Int


type alias Edge =
    ( IndexW, IndexW )


type alias Edges =
    Dict IndexW (List IndexW)


type alias Entropy =
    Float


type alias Color =
    Int


type alias T3 a =
    ( a, a, a )


type alias State =
    T3 (T3 Color)


propagate : Wave -> IndexW -> Edges -> Propagator -> ( Wave, List IndexW )
propagate wave iW1 edges propagator =
    let
        -- current states
        states : List IndexS
        states =
            Array.get iW1 wave
                |> Maybe.withDefault Array.empty
                |> maskToIndex

        -- test if a single neighboring state agrees
        agree : IndexW -> IndexS -> Bool
        agree iW2 iS2 =
            states |> List.all (\iS1 -> propagator iS1 iS2 ( iW1, iW2 ))

        -- map over neighboring states, only testing if true
        update : IndexW -> Point -> Point
        update iW2 point =
            point
                |> Array.indexedMap
                    (\iS2 s ->
                        if s then
                            agree iW2 iS2
                        else
                            s
                    )

        solveNeighbor iW2 =
            let
                oldPoint =
                    Array.get iW2 wave |> Maybe.withDefault Array.empty

                newPoint =
                    update iW2 oldPoint
            in
                if newPoint == oldPoint then
                    Nothing
                else
                    Just newPoint

        neighbors =
            edges
                |> Dict.get iW1
                |> Maybe.withDefault []

        foldIt iW2 ( waveSoFar, toDo ) =
            case solveNeighbor iW2 of
                Just newPoint ->
                    let
                        nextWave =
                            Array.set iW2 newPoint waveSoFar

                        toDoTwo =
                            Dict.get iW2 edges |> Maybe.withDefault [] |> (++) toDo
                    in
                        ( nextWave, toDoTwo )

                Nothing ->
                    ( waveSoFar, toDo )
    in
        neighbors |> List.foldl foldIt ( wave, [] )


{-| Observe returns Nothing if no Point was chosen.
-}
observe : Wave -> Selector comparable -> Collapser -> Maybe Wave
observe wave selector collapser =
    let
        index =
            wave
                |> Array.toList
                |> List.filterMap selector
                |> argminList

        newPoint =
            index
                |> Maybe.andThen (\i -> Array.get i wave)
                |> Maybe.andThen collapser
    in
        case ( index, newPoint ) of
            ( Just i, Just val ) ->
                Just (Array.set i val wave)

            _ ->
                Nothing


findEntropy : Point -> Array State -> Entropy
findEntropy point states =
    point
        |> maskToIndex
        |> List.filterMap ((flip Array.get) states)
        |> counter
        |> Dict.values
        |> shannon2


getTiles : Array State -> Point -> List State
getTiles states point =
    point
        |> maskToIndex
        |> List.filterMap ((flip Array.get) states)



-- UTILITIES


maskToIndex : Array Bool -> List Int
maskToIndex boolArray =
    let
        f ( i, b ) =
            if b then
                Just i
            else
                Nothing
    in
        boolArray |> Array.toIndexedList |> List.filterMap f


shannon2 : List Int -> Float
shannon2 counts =
    let
        plogp x =
            -x * (logBase 2 (x))

        n =
            List.sum counts |> toFloat
    in
        counts
            |> List.map toFloat
            |> List.map plogp
            |> List.sum
            |> (\x -> x / n + logBase 2 n)


foldF : comparable -> Dict comparable Int -> Dict comparable Int
foldF x dict =
    let
        updateF mv =
            case mv of
                Nothing ->
                    Just 1

                Just v ->
                    Just (v + 1)
    in
        Dict.update x updateF dict


counter : List comparable -> Dict comparable Int
counter xs =
    List.foldl foldF Dict.empty xs



-- argmin_ : comparable -> Array.Array comparable -> ( Int, Int, comparable )


argmin_ : a -> b -> ((comparable -> ( number, number, comparable ) -> ( number1, number, comparable )) -> ( number2, number3, a ) -> b -> c) -> c
argmin_ maxValue arr foldl =
    let
        go newValue ( lastIndex, minIndex, minValue ) =
            if (newValue < minValue) then
                ( lastIndex + 1, lastIndex + 1, newValue )
            else
                ( lastIndex + 1, minIndex, minValue )
    in
        foldl go ( -1, 0, maxValue ) arr


argminArray : Array.Array comparable -> Maybe Int
argminArray arr =
    let
        third ( _, a, _ ) =
            a
    in
        case Array.get 0 arr of
            Just val ->
                Just (argmin_ val arr Array.foldl |> third)

            Nothing ->
                Nothing


argminList : List comparable -> Maybe Int
argminList xs =
    let
        third ( _, a, _ ) =
            a
    in
        case xs of
            x :: rest ->
                Just (argmin_ x xs List.foldl |> third)

            [] ->
                Nothing


main : Html.Html msg
main =
    text ""
