module WFC.Utilities exposing (..)

import WFC.Types exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra exposing (lift2)


pointToStates : Array a -> Array Bool -> List a
pointToStates states point =
    point
        |> maskToIndex
        |> List.filterMap ((flip Array.get) states)


offsets : List ( Int, Int )
offsets =
    [ ( -1, -1 )
    , ( -1, 0 )
    , ( -1, 1 )
    , ( 0, -1 )
    , ( 0, 1 )
    , ( 1, -1 )
    , ( 1, 0 )
    , ( 1, 1 )
    ]


toOffset : Int -> Int -> Edge -> ( Int, Int )
toOffset x y ( index1, index2 ) =
    let
        to2D =
            to2DIndex x y

        ( ( i1, j1 ), ( i2, j2 ) ) =
            ( to2D index1, to2D index2 )
    in
        ( i2 - i1, j2 - j1 )


{-| no periodic boundaries
-}
toLinearIndex : Int -> Int -> ( Int, Int ) -> Maybe Int
toLinearIndex x y ( i, j ) =
    let
        index =
            i * x + j
    in
        if (inRange -1 x j) && (inRange -1 y i) then
            Just index
        else
            Nothing


{-| row order. returns (i, j)
-}
to2DIndex : Int -> Int -> Int -> ( Int, Int )
to2DIndex x y index =
    ( index // x, index % y )


inRange : comparable -> comparable -> comparable -> Bool
inRange less more value =
    (less < value) && (value < more)


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


repeatApply : (a -> a) -> Int -> a -> a
repeatApply f n x =
    if n == 0 then
        x
    else
        repeatApply f (n - 1) (f x)


toNothing : comparable -> comparable -> Maybe comparable
toNothing x y =
    if x == y then
        Nothing
    else
        Just y


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


maybeMember : List a -> a -> Maybe a
maybeMember xs x =
    if List.member x xs then
        Just x
    else
        Nothing


edgeToOffset : Int -> Edge -> Maybe ( Int, Int )
edgeToOffset n edge =
    (edge |> toOffset n n |> maybeMember offsets)


{-| Return linear index for subrectangle with offset.
-}
slice : Int -> Int -> Int -> List Int
slice n i j =
    let
        offset ( a, b ) =
            ( a + i, b + j )

        keep ( a, b ) =
            a >= 0 && b >= 0 && a < n && b < n

        flatten ( a, b ) =
            a * n + b
    in
        lift2 (,) (List.range 0 (n - 1)) (List.range 0 (n - 1))
            |> List.map offset
            |> List.filter keep
            |> List.map flatten


gridEdges : Int -> Int -> Edges
gridEdges x y =
    let
        grid =
            lift2 (,) (List.range 0 (x - 1)) (List.range 0 (y - 1))

        neighbors ( i, j ) =
            offsets
                |> List.map (\( a, b ) -> ( a + i, b + j ))
                |> List.filterMap (toLinearIndex x y)
    in
        grid
            |> List.map neighbors
            |> List.indexedMap (,)
            |> Dict.fromList
