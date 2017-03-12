module WFC exposing (..)

import WFC.View exposing (..)
import WFC.Types exposing (..)
import WFC.Utilities exposing (..)
import WFC.Tile exposing (..)
import Html exposing (text, li, div, tr, td, br)
import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra exposing (lift2, lift4, (!!))
import Random.Pcg exposing (Seed)


-- CORE


propagate : Edges -> Compatibility -> Propagator
propagate edges propagator wave iW1 =
    let
        -- current states
        states : List IndexS
        states =
            Array.get iW1 wave
                |> Maybe.withDefault Array.empty
                |> maskToIndex

        -- test if a single neighboring state agrees with at least one
        agree : IndexW -> IndexS -> Bool
        agree iW2 iS2 =
            states |> List.any (\iS1 -> propagator iS1 iS2 ( iW1, iW2 ))

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

        -- returns Nothing if none of the coefficients in Point change
        solveNeighbor : IndexW -> Maybe Point
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
        neighbors
            |> List.foldl foldIt ( wave, [] )
            |> (\( w, xs ) -> ( w, List.Extra.unique xs ))


{-| Observe returns Nothing if no Point was chosen.
-}
observe : Selector comparable -> Collapser -> Wave -> Maybe ( Wave, IndexW )
observe selector collapser wave =
    let
        applySelector i a =
            case selector a of
                Just x ->
                    Just ( x, i )

                Nothing ->
                    Nothing

        index =
            wave
                |> Array.indexedMap applySelector
                |> Array.toList
                |> List.filterMap identity
                |> List.minimum
                |> Maybe.map Tuple.second

        newPoint =
            index
                |> Maybe.andThen (\i -> Array.get i wave)
                |> Maybe.andThen collapser
    in
        case ( index, newPoint ) of
            ( Just i, Just val ) ->
                Just ( Array.set i val wave, i )

            _ ->
                Nothing



-- WRAPPERS


observeThenPropagate : Selector comparable -> Collapser -> Propagator -> Wave -> Wave
observeThenPropagate selector collapser prop wave =
    observe selector collapser wave
        |> Maybe.map (\( a, b ) -> ( a, [ b ] ))
        |> Maybe.map (propagateFold prop)
        |> Maybe.withDefault wave


propagateFold : Propagator -> ( Wave, List IndexW ) -> Wave
propagateFold prop ( wave, targets ) =
    let
        g target ( wave2, soFar ) =
            prop wave2 target
                |> (\( a, b ) -> ( a, b ++ soFar ))

        ( finalWave, finalTargets ) =
            List.foldl g ( wave, [] ) targets
    in
        if List.isEmpty finalTargets then
            finalWave
        else
            propagateFold prop ( finalWave, finalTargets )


{-| Return the new Wave and a history of intermediates.
-}
propagateWithHistory : Propagator -> ( Wave, List IndexW ) -> ( Wave, List IndexW, List ( Wave, IndexW ) )
propagateWithHistory indexWListWaveIndexWWave ( wave, indexWList ) =
    let
        newWave =
            Array.empty

        targets =
            []

        visited =
            []
    in
        ( newWave, targets, visited )



-- HELPFUL


contradiction : Wave -> Bool
contradiction wave =
    let
        innerTest point =
            anyArray identity point
    in
        anyArray innerTest wave


collapseSpecific : IndexW -> IndexS -> Wave -> Maybe Wave
collapseSpecific iW iS wave =
    case Array.get iW wave of
        Nothing ->
            Nothing

        Just point ->
            let
                m =
                    Array.length point

                newPoint =
                    False |> List.repeat m |> Array.fromList |> Array.set iS True
            in
                if inRange -1 m iS then
                    Just (Array.set iW newPoint wave)
                else
                    Nothing


selectorSimple : Point -> Maybe IndexS
selectorSimple point =
    point |> Array.filter identity |> Array.length |> (*) -1 |> Just


selectorEntropy : Array State -> Point -> Maybe number
selectorEntropy statesArray point =
    pointToStates statesArray point
        |> tileEntropy
        |> (\x ->
                if x < 3 then
                    Nothing
                else
                    Just x
           )


{-| Returns Nothing if there are no True coefficients in the Point.
-}
simpleCollapser : Collapser
simpleCollapser point =
    let
        singleTrue i =
            Array.repeat (Array.length point) False
                |> Array.set i True
    in
        Array.indexedMap (,) point
            |> Array.filter Tuple.second
            |> Array.get 0
            |> Maybe.map Tuple.first
            |> Maybe.map singleTrue


{-| Bases the selection off of the index with the most counts, among those
considered.
-}
collapser : List Int -> Collapser
collapser counts =
    (\x -> Nothing)


{-| Selects a valid index, weighted by counts.
-}
collapserRandom : List Int -> Seed -> Collapser
collapserRandom counts seed =
    (\x -> Nothing)


main : Html.Html msg
main =
    let
        n =
            5

        edges =
            gridEdges n n

        states =
            checkerboardABCDEF

        k =
            List.length states

        statesArray =
            states |> Array.fromList

        initialWave =
            Array.repeat (List.length states) True
                |> Array.repeat (n * n)

        cornerPoint =
            True :: (List.repeat (k - 1) False)

        changedWave =
            initialWave
                |> Array.set 0 (cornerPoint |> Array.fromList)

        propagator : Compatibility
        propagator =
            makeTileCompatibility states (edgeToOffset n n)

        observationDeck =
            observeThenPropagate
                selectorSimple
                simpleCollapser
                (propagate edges propagator)

        drawPointDiv2 i point =
            drawPointDiv statesArray point []

        showWave wave =
            drawWaveDiv n n drawPointDiv2 wave
                |> List.repeat 1
                |> div [ waveContainer (n * 20) (n * 20) ]
    in
        div []
            [ initialWave |> showWave
              -- , initialWave |> repeatApply observationDeck 1 |> showWave
              -- , initialWave |> repeatApply observationDeck 2 |> showWave
              -- , initialWave |> repeatApply observationDeck 3 |> showWave
              -- , initialWave |> repeatApply observationDeck 4 |> showWave
              -- , initialWave |> repeatApply observationDeck 5 |> showWave
            , initialWave |> repeatApply observationDeck 6 |> showWave
            ]



-- , finalWave |> viewWave n n
--   -- , br [] []
--   -- , testViewOverlaps n (propagator 4 5)
-- , br [] []
-- , waveX |> viewWave n n
-- , br [] []
-- , targets |> toString |> text
