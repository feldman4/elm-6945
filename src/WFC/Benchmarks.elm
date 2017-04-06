module WFC.Benchmarks exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Array
import Dict
import WFC.Types exposing (..)
import WFC.App exposing (..)
import WFC.Tile exposing (..)
import WFC.Utilities exposing (..)


main : BenchmarkProgram
main =
    (Benchmark.describe "Dict"
        [ -- getSimpleTuple
          -- getCompatibility
          -- getSimpleTupleOffset
          -- convertEdge
          getSimple
        , basicOps
        ]
    )
        |> program


{-| 2us to look up compatibility once. numpy takes 30us to look up all the
pairwise compatibilities making it up to (M^2 / 15) faster:

self.wave[a+i,b+j,coeffs2] = \
        self.table[a, b][np.ix_(coeffs1, coeffs2)].any(axis=0)
-}
getCompatibility : Benchmark
getCompatibility =
    (Benchmark.benchmark3
        "get compatibility"
        data.compatibility
        0
        0
        ( 0, 1 )
    )


{-| 57 ns per call.
-}
getSimple : Benchmark
getSimple =
    (Benchmark.benchmark2
        "get simple"
        Dict.get
        "1"
        (Dict.fromList [ ( "1", 2 ) ])
    )


{-| 148 ns per call
-}
getSimpleTuple : Benchmark
getSimpleTuple =
    (Benchmark.benchmark2
        "get simple tuple key"
        Dict.get
        ( 0, 0, 0, 0 )
        (Dict.fromList [ ( ( 0, 0, 0, 0 ), 2 ) ])
    )


{-|
- 6 ns for identity with 1 arg.
- 20 ns with 2 or more args.
- overhead for (+) is at least 10X lower than calling a function of 2 args.
- list of 100 numbers
  - sum takes 1.5 us (750 ns for sum(list) in python)
  - Array.toList takes 1.4 us, proportional to length
-}
basicOps : Benchmark
basicOps =
    let
        xs =
            List.range 0 1000

        ys =
            Array.fromList xs
    in
        (benchmark1
            "identity"
            -- (Array.foldl (+) 0)
            -- (Array.toList >> List.sum)
            Array.toList
            ys
        )


{-| 1.2 us per call, independent of dictionary size.
Due to defining compatibility table over (i,j) offsets, not (iW1, iW2) edges.
     (i,j) table size: M * M * Ne = ~100K
(iW1, iW2) table size: M * M * N  = ~10K * N = ~10M => 100 megabytes?

or, memoize the converter edgeToOffset within makeTileCompatibility
can pick where to memoize, what's case-specific is how the compatibility
table factorizes. e.g., for simple bitmaps it is translation-invariant over
points, so we can turn

(iW1, iW2, iS1, iS2) -> Bool

into

((iW1, iW2) -> offset) ->
  ((offset, iS1, iS2) -> Bool) ->
  (iW1, iW2, iS1, iS2) -> Bool

Tile compatibility is also symmetric over offsets, so we have the equivalence
(offset, iS1, iS2) = (-offset, iS2, iS1). However wrapping a simpler table
(e.g., only non-negative offsets, or only iS1 <= iS2) incurs an additional
lookup

-}
getSimpleTupleOffset : Benchmark
getSimpleTupleOffset =
    let
        converter =
            (edgeToOffset 7 7)

        table =
            List.range 0 10000
                |> List.map (\x -> ( ( x, x, x, x ), True ))
                |> Dict.fromList

        propagator iS1 iS2 ( iW1, iW2 ) =
            ( iW1, iW2 )
                |> converter
                |> Maybe.andThen
                    (\( i, j ) -> Dict.get ( i, j, iS1, iS2 ) table)
                |> Maybe.withDefault False
    in
        (Benchmark.benchmark3
            "get simple tuple offset"
            propagator
            0
            0
            ( 0, 0 )
        )


convertEdge : Benchmark
convertEdge =
    (benchmark1 "convert edge to offset" (edgeToOffset 7 7) ( 0, 1 ))


data : { model : Model, compatibility : Compatibility }
data =
    let
        ( height, width ) =
            ( 7, 7 )

        tileset =
            CheckerboardABCDEF

        compatibility =
            makeTileCompatibility (getTileSet tileset) (edgeToOffset height width)
    in
        { model = init height width tileset, compatibility = compatibility }


m : Int
m =
    Array.get 0 data.model.wave |> Maybe.map Array.length |> Maybe.withDefault -1


pointMod : ( number, List Int )
pointMod =
    ( 0, List.range 1 (m - 1) )
