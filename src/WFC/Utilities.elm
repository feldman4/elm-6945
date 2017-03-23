module WFC.Utilities exposing (..)

import WFC.Types exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra exposing (lift2)
import Dict.Extra


aMap2 : (a -> b) -> Array (Array a) -> Array (Array b)
aMap2 f a =
    Array.map (Array.map f) a


aFind2 : (a -> b -> Bool) -> Array a -> Array b -> List Int
aFind2 f xs ys =
    List.map2 (,) (xs |> Array.toList) (ys |> Array.toList)
        |> List.Extra.findIndices (\( a, b ) -> f a b)


collapsePoint2 : IndexW -> IndexS -> Wave2 -> Wave2
collapsePoint2 iW iS wave2 =
    let
        point =
            Array.repeat wave2.m False |> Array.set iS True
    in
        { wave2 | wave = Array.set iW point wave2.wave }


wave2wave : Wave2 -> Wave
wave2wave { n, m, edges, support } =
    supportToWave n
        m
        edges
        support


unsafeJust : Maybe a -> a
unsafeJust a =
    case a of
        Just val ->
            val

        Nothing ->
            Debug.crash "unsafe"


waveToSupport : Edges -> Compatibility -> Wave -> Support
waveToSupport edges compatibility wave =
    let
        -- flatten
        f ( x, ys ) =
            List.map (\y -> ( x, y )) ys

        -- every (iW1, iW2) edge
        edgesList =
            edges |> Dict.toList |> List.concatMap f

        m =
            wave
                |> Array.get 0
                |> Maybe.map Array.length
                |> unsafeJust

        iSs =
            List.range 0 (m - 1)

        -- for a given edge and target state
        sumSupport edge iS2 =
            iSs
                |> List.filter (\x -> compatibility x iS2 edge)
                |> List.length

        buildSupport ( edge, iS2 ) =
            ( ( edge, iS2 ), sumSupport edge iS2 )
    in
        List.Extra.lift2 (,) edgesList iSs
            |> List.map buildSupport
            |> Dict.fromList


supportVector : Int -> Support -> IndexW -> IndexW -> List IndexS
supportVector m support iW1 iW2 =
    List.range 0 (m - 1)
        |> List.filterMap (\iS -> Dict.get ( ( iW1, iW2 ), iS ) support)


{-| Build a wave out of supported points. The dimensions of the wave are not
quite specified by the Edge representation, although we could include them in
s
-}
supportToWave : Int -> Int -> Edges -> Support -> Wave
supportToWave n m edges support =
    let
        edges2 =
            invertDict edges

        iWs =
            List.range 0 (n - 1)

        iSs =
            List.range 0 (m - 1)

        f iS iW2 iW1 =
            Dict.get ( ( iW1, iW2 ), iS ) support
                |> Maybe.withDefault 0
                |> (<) 0

        allSupport iW2 iS =
            let
                -- need support from all these edges
                iW1s =
                    Dict.get iW2 edges2 |> Maybe.withDefault []
            in
                iW1s |> List.all (f iS iW2)

        makePoint iW2 =
            iSs |> List.map (allSupport iW2) |> Array.fromList
    in
        iWs |> List.map makePoint |> Array.fromList


{-| Dict x (List y) -> Dict y (List x)
-}
invertDict : Dict comparable (List comparable1) -> Dict comparable1 (List comparable)
invertDict dict =
    let
        f ( x, ys ) =
            List.map (\y -> ( x, y )) ys

        g _ xys =
            List.map Tuple.first xys
    in
        dict
            |> Dict.toList
            |> List.concatMap f
            |> Dict.Extra.groupBy Tuple.second
            |> Dict.map g


anyArray : (a -> Bool) -> Array a -> Bool
anyArray test array =
    let
        n =
            Array.length array

        iter i =
            case (Array.get n array) of
                Just val ->
                    if test val then
                        iter (i + 1)
                    else
                        False

                Nothing ->
                    False
    in
        iter 0


pointToStatesIndexed : Array a -> Point -> List ( Int, a )
pointToStatesIndexed states point =
    let
        get i =
            case Array.get i states of
                Just state ->
                    Just ( i, state )

                Nothing ->
                    Nothing
    in
        point
            |> maskToIndex
            |> List.filterMap get


pointToStates : Array a -> Point -> List a
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


{-| inRange low high val = low < val < high
-}
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


edgeToOffset : Int -> Int -> Edge -> Maybe ( Int, Int )
edgeToOffset x y edge =
    (edge |> toOffset x y |> maybeMember offsets)


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
