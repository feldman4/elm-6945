module WFC.Tile exposing (..)

import WFC.Types exposing (..)
import WFC.Utilities exposing (..)
import List.Extra exposing ((!!), lift4)
import Dict
import Array


tileEntropy : List State -> Float
tileEntropy states =
    let
        flatten ( ( a1, a2, a3 ), ( b1, b2, b3 ), ( c1, c2, c3 ) ) =
            [ a1, a2, a3, b1, b2, b3, c1, c2, c3 ]
    in
        states
            |> List.map flatten
            |> List.Extra.transpose
            |> List.map (counter >> Dict.values >> shannon2)
            |> List.sum


{-| deals with tile-specific agreement. internally converts Edge to an overlap
using the given function
-}
makeTileCompatibility : List State -> (Edge -> Maybe Offset) -> Compatibility
makeTileCompatibility stateList converter =
    let
        states =
            stateList |> Array.fromList

        n =
            Array.length states

        offsetRange =
            (List.range -1 1)

        stateRange =
            (List.range 0 (n - 1))

        toList : State -> List Color
        toList ( ( a0, a1, a2 ), ( a3, a4, a5 ), ( a6, a7, a8 ) ) =
            [ a0, a1, a2, a3, a4, a5, a6, a7, a8 ]

        getColors iS =
            Array.get iS states
                |> Maybe.map toList
                |> Maybe.withDefault []

        testOverlap : DeltaI -> DeltaJ -> IndexS -> IndexS -> Bool
        testOverlap i j iS1 iS2 =
            let
                -- get the subrectangle of a Tile corresponding to the (i,j) offset
                colors1 =
                    slice 3 i j |> List.filterMap ((!!) << getColors <| iS1)

                colors2 =
                    slice 3 -i -j |> List.filterMap ((!!) << getColors <| iS2)
            in
                List.map2 (==) colors1 colors2 |> List.all identity

        table =
            lift4 (,,,) offsetRange offsetRange stateRange stateRange
                |> List.map (\( i, j, iS1, iS2 ) -> ( ( i, j, iS1, iS2 ), testOverlap i j iS1 iS2 ))
                |> Dict.fromList

        propagator iS1 iS2 ( iW1, iW2 ) =
            ( iW1, iW2 )
                |> converter
                |> Maybe.andThen
                    (\( i, j ) -> Dict.get ( i, j, iS1, iS2 ) table)
                |> Maybe.withDefault False
    in
        propagator


{-| 1
-}
checkerboardA : State
checkerboardA =
    ( ( 0, 1, 0 )
    , ( 1, 0, 1 )
    , ( 0, 1, 0 )
    )


{-| 2
-}
checkerboardB : State
checkerboardB =
    ( ( 1, 0, 1 )
    , ( 0, 1, 0 )
    , ( 1, 0, 1 )
    )


{-| 4
-}
checkerboardC : State
checkerboardC =
    ( ( 1, 0, 0 )
    , ( 0, 1, 1 )
    , ( 1, 0, 0 )
    )


{-| 8
-}
checkerboardD : State
checkerboardD =
    ( ( 0, 0, 1 )
    , ( 1, 1, 0 )
    , ( 0, 0, 1 )
    )


{-| 16
-}
checkerboardE : State
checkerboardE =
    ( ( 0, 1, 1 )
    , ( 1, 0, 0 )
    , ( 0, 1, 1 )
    )


{-| 32
-}
checkerboardF : State
checkerboardF =
    ( ( 1, 1, 0 )
    , ( 0, 0, 1 )
    , ( 1, 1, 0 )
    )


checkerboardAB : List State
checkerboardAB =
    [ checkerboardA, checkerboardB ]


checkerboardABCDEF : List State
checkerboardABCDEF =
    [ checkerboardA
    , checkerboardB
    , checkerboardC
    , checkerboardD
    , checkerboardE
    , checkerboardF
    ]


knot : List State
knot =
    [ ( ( 0, 0, 0 ), ( 0, 1, 0 ), ( 0, 1, 0 ) )
    , ( ( 0, 0, 0 ), ( 0, 1, 1 ), ( 0, 1, 0 ) )
    , ( ( 0, 0, 0 ), ( 0, 1, 1 ), ( 0, 1, 1 ) )
    , ( ( 0, 0, 0 ), ( 1, 0, 1 ), ( 1, 0, 1 ) )
    , ( ( 0, 0, 0 ), ( 1, 1, 0 ), ( 0, 1, 0 ) )
    , ( ( 0, 0, 0 ), ( 1, 1, 0 ), ( 1, 1, 0 ) )
    , ( ( 0, 0, 0 ), ( 1, 1, 1 ), ( 0, 0, 0 ) )
    , ( ( 0, 0, 0 ), ( 1, 1, 1 ), ( 0, 0, 1 ) )
    , ( ( 0, 0, 0 ), ( 1, 1, 1 ), ( 1, 0, 0 ) )
    , ( ( 0, 0, 0 ), ( 1, 1, 1 ), ( 1, 1, 1 ) )
    , ( ( 0, 0, 1 ), ( 1, 0, 1 ), ( 1, 0, 1 ) )
    , ( ( 0, 0, 1 ), ( 1, 1, 1 ), ( 0, 0, 0 ) )
    , ( ( 0, 0, 1 ), ( 1, 1, 1 ), ( 1, 1, 1 ) )
    , ( ( 0, 1, 0 ), ( 0, 0, 0 ), ( 1, 1, 1 ) )
    , ( ( 0, 1, 0 ), ( 0, 1, 0 ), ( 0, 0, 0 ) )
    , ( ( 0, 1, 0 ), ( 0, 1, 0 ), ( 0, 1, 0 ) )
    , ( ( 0, 1, 0 ), ( 0, 1, 0 ), ( 0, 1, 1 ) )
    , ( ( 0, 1, 0 ), ( 0, 1, 0 ), ( 1, 1, 0 ) )
    , ( ( 0, 1, 0 ), ( 0, 1, 1 ), ( 0, 0, 0 ) )
    , ( ( 0, 1, 0 ), ( 1, 1, 0 ), ( 0, 0, 0 ) )
    , ( ( 0, 1, 1 ), ( 0, 0, 0 ), ( 1, 1, 1 ) )
    , ( ( 0, 1, 1 ), ( 0, 1, 0 ), ( 0, 1, 0 ) )
    , ( ( 0, 1, 1 ), ( 0, 1, 1 ), ( 0, 0, 0 ) )
    , ( ( 0, 1, 1 ), ( 0, 1, 1 ), ( 0, 1, 1 ) )
    , ( ( 0, 1, 1 ), ( 0, 1, 1 ), ( 1, 1, 1 ) )
    , ( ( 1, 0, 0 ), ( 1, 0, 1 ), ( 1, 0, 1 ) )
    , ( ( 1, 0, 0 ), ( 1, 1, 1 ), ( 0, 0, 0 ) )
    , ( ( 1, 0, 1 ), ( 0, 0, 0 ), ( 1, 1, 1 ) )
    , ( ( 1, 0, 1 ), ( 0, 0, 1 ), ( 1, 1, 1 ) )
    , ( ( 1, 0, 1 ), ( 1, 0, 0 ), ( 1, 1, 1 ) )
    , ( ( 1, 0, 1 ), ( 1, 0, 1 ), ( 0, 0, 0 ) )
    , ( ( 1, 0, 1 ), ( 1, 0, 1 ), ( 0, 0, 1 ) )
    , ( ( 1, 0, 1 ), ( 1, 0, 1 ), ( 1, 0, 0 ) )
    , ( ( 1, 0, 1 ), ( 1, 0, 1 ), ( 1, 0, 1 ) )
    , ( ( 1, 1, 0 ), ( 0, 0, 0 ), ( 1, 1, 1 ) )
    , ( ( 1, 1, 0 ), ( 0, 1, 0 ), ( 0, 1, 0 ) )
    , ( ( 1, 1, 0 ), ( 1, 1, 0 ), ( 0, 0, 0 ) )
    , ( ( 1, 1, 0 ), ( 1, 1, 0 ), ( 1, 1, 0 ) )
    , ( ( 1, 1, 1 ), ( 0, 0, 0 ), ( 0, 1, 0 ) )
    , ( ( 1, 1, 1 ), ( 0, 0, 0 ), ( 0, 1, 1 ) )
    , ( ( 1, 1, 1 ), ( 0, 0, 0 ), ( 1, 0, 1 ) )
    , ( ( 1, 1, 1 ), ( 0, 0, 0 ), ( 1, 1, 0 ) )
    , ( ( 1, 1, 1 ), ( 0, 0, 0 ), ( 1, 1, 1 ) )
    , ( ( 1, 1, 1 ), ( 0, 0, 1 ), ( 1, 0, 1 ) )
    , ( ( 1, 1, 1 ), ( 1, 0, 0 ), ( 1, 0, 1 ) )
    , ( ( 1, 1, 1 ), ( 1, 1, 0 ), ( 1, 1, 0 ) )
    , ( ( 1, 1, 1 ), ( 1, 1, 1 ), ( 0, 0, 0 ) )
    , ( ( 1, 1, 1 ), ( 1, 1, 1 ), ( 1, 0, 0 ) )
    , ( ( 1, 1, 1 ), ( 1, 1, 1 ), ( 1, 1, 1 ) )
    ]
