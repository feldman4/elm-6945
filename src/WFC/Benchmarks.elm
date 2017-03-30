module WFC.Benchmarks exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict
import Array
import List.Extra exposing (lift2)
import WFC.Types exposing (..)
import WFC.App exposing (..)
import WFC.Tile exposing (..)
import WFC.Utilities exposing (..)


get : Benchmark
get =
    Benchmark.benchmark2 "Dict.get" Dict.get "a" (Dict.singleton "a" 1)


main : BenchmarkProgram
main =
    program propagate


tileset : TileSet
tileset =
    CheckerboardABCDEF


height : number
height =
    7


width : number
width =
    7


model : Model
model =
    init height width tileset


m : Int
m =
    Array.get 0 model.wave |> Maybe.map Array.length |> Maybe.withDefault -1


pointMod : ( number, List Int )
pointMod =
    ( 0, List.range 1 (m - 1) )


propagate : Benchmark
propagate =
    benchmark2 "propagator2" model.propagator2 model.wave2 pointMod


compatibility : Compatibility
compatibility =
    makeTileCompatibility (getTileSet tileset) (edgeToOffset height width)


support : Support
support =
    model.wave2.support


edges : Edges
edges =
    model.wave2.edges


wave : Wave
wave =
    model.wave2.wave


wave2 : Wave2
wave2 =
    model.wave2


iW1 : number
iW1 =
    0


dPoint : List Int
dPoint =
    List.range 1 (m - 1)


iSs : List Int
iSs =
    List.range 0 (m - 1)


iW2s : List IndexW
iW2s =
    Dict.get iW1 edges |> Maybe.withDefault []



-- change in support due to losing iS1s in dPoint
-- could be faster to store compatibility for all iS1 as an Array
-- and then filter it instead of up to M dictionary calls


dSupport : Edge -> IndexS -> Int
dSupport edge iS2 =
    dPoint
        |> List.filter (\iS1 -> compatibility iS1 iS2 edge)
        |> List.length



-- for a single iW2 and iS2, decrement support based on dPoint


updateSupport : ( IndexW, IndexS ) -> Support -> Support
updateSupport ( iW2, iS2 ) s =
    let
        edge =
            ( iW1, iW2 )

        f x =
            x - (dSupport edge iS2)
    in
        Dict.update ( edge, iS2 ) (Maybe.map f) s



-- update support for each neighbor and all states


newSupport : Support
newSupport =
    lift2 (,) iW2s iSs
        |> List.foldl updateSupport support



-- get the new dPoints
-- new support must be zero
-- old support must be positive
-- wave coefficient cannot be zero


lostSupport : IndexW -> List Int
lostSupport iW2 =
    let
        old =
            supportVector m support iW1 iW2

        new =
            supportVector m newSupport iW1 iW2

        test ( old_, new_ ) =
            let
                wtf =
                    if old_ < 0 || new_ < 0 then
                        Debug.log "wtf" ( iW1, iW2, old, new )
                    else
                        ( 0, 0, [], [] )
            in
                (new_ == 0) && (old_ > 0)
    in
        List.map2 (,) old new
            |> List.Extra.findIndices test


toPoint2 : Int -> Maybe ( Int, List Int )
toPoint2 iW2 =
    let
        -- if this point is collapsed, don't bother
        coefficients =
            Array.get iW2 wave
                |> Maybe.map maskToIndex
                |> Maybe.withDefault []

        -- only collapse if the coefficient is positive
        f iS =
            List.member iS coefficients
    in
        if List.length coefficients > 1 then
            case lostSupport iW2 |> List.filter f of
                [] ->
                    Nothing

                iSs ->
                    Just ( iW2, iSs )
        else
            Nothing


nextPoints : List ( Int, List Int )
nextPoints =
    iW2s |> List.filterMap toPoint2



-- zero out the wave coefficients


setToZero :
    ( Int, List Int )
    -> Array.Array (Array.Array Bool)
    -> Array.Array (Array.Array Bool)
setToZero ( iW, iSs ) w =
    let
        newPoint =
            Array.get iW w
                |> Maybe.withDefault Array.empty

        newPoint2 =
            List.foldl (\i a -> Array.set i False a) newPoint iSs
    in
        Array.set iW newPoint2 w


newWave : Array.Array Point
newWave =
    List.foldl setToZero wave nextPoints



-- in
--     ( { wave2 | support = newSupport, wave = newWave }, nextPoints )
