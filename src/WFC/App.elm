module WFC.App exposing (..)

import WFC exposing (..)
import WFC.Types exposing (..)
import WFC.View exposing (..)
import WFC.Tile exposing (..)
import WFC.Utilities exposing (..)
import Html exposing (text, div, a, li, br)
import Html.Events exposing (onClick, onDoubleClick, onMouseEnter, onMouseLeave)
import Array
import Task exposing (Task)
import Time exposing (Time, inSeconds)
import AnimationFrame exposing (times)
import List.Extra


main : Program Never Model Action
main =
    Html.program
        { init = (init 7 7 CheckerboardABCDEF) ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Int -> Int -> TileSet -> Model
init height width tileset =
    let
        model =
            { wave = Array.empty
            , tileset = tileset
            , edges = gridEdges height width
            , propagator = (\a b -> a [ b ])
            , pending = []
            , height = height
            , width = width
            , mouseOver = Nothing
            , mouseTouch = -1
            }

        compatibility =
            makeTileCompatibility (getTileSet model.tileset) (edgeToOffset height width)
    in
        { model | wave = initWave model, propagator = propagate model.edges compatibility }


type alias Model =
    { wave : Wave
    , tileset : TileSet
    , edges : Edges
    , propagator : Propagator
    , pending : List IndexW
    , height : Int
    , width : Int
    , mouseOver : Maybe Int
    , mouseTouch : Int
    }


type Action
    = Observe IndexW
    | Reset
    | MouseEnter IndexW
    | Collapse IndexS
    | Propagate IndexW
    | ChangeTileSet TileSet
    | Select IndexW
    | AnimationFrame Float
    | TaskDone ( Wave, List IndexW )
    | ChangeSize ( Int, Int )


type TileSet
    = CheckerboardAB
    | CheckerboardABCDEF
    | Knot
    | Dungeon


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Reset ->
            { model | wave = initWave model } ! []

        Observe _ ->
            { model | wave = observationDeck model } ! []

        MouseEnter val ->
            { model | mouseTouch = val } ! []

        Collapse iS ->
            (collapseFromInventory model iS) ! []

        Select iW ->
            case model.mouseOver of
                Just index ->
                    if index == iW then
                        { model | mouseOver = Nothing } ! []
                    else
                        { model | mouseOver = Just iW } ! []

                Nothing ->
                    { model | mouseOver = Just iW } ! []

        Propagate iW ->
            let
                wave =
                    propagateFold model.propagator ( model.wave, [ iW ] )
            in
                { model | wave = wave } ! []

        TaskDone ( wave, pending ) ->
            -- set-join pending queues?
            -- or does that just build up anyway
            let
                pending2 =
                    List.Extra.unique pending
            in
                { model | wave = wave, pending = pending2 } ! []

        ChangeTileSet tileset ->
            (init model.height model.width tileset) ! []

        ChangeSize ( x, y ) ->
            (init x y model.tileset) ! []

        AnimationFrame diff ->
            let
                go =
                    propagateOnce

                stop m =
                    List.isEmpty m.pending

                done m =
                    TaskDone ( m.wave, m.pending )
            in
                if not (stop model) |> Debug.log "tested" then
                    model ! [ runUntil go stop done model (diff * 0.5) ]
                else
                    model ! []


{-| slow, tried deque but it didn't help much
-}
propagateOnce : Model -> Model
propagateOnce model =
    case model.pending of
        iW :: rest ->
            let
                ( newWave, newTargets ) =
                    model.propagator model.wave iW
            in
                { model | wave = newWave, pending = newTargets ++ rest }

        [] ->
            model


{-| Transforms the input using the `go` function until either the `stop` condition
is met, or the alloted time `period` has elapsed. Isn't working well for periods
below 100 ms.
-}
runUntil : (a -> a) -> (a -> Bool) -> (a -> msg) -> a -> Time -> Cmd msg
runUntil go stop done input period =
    let
        initialData t =
            let
                z =
                    Debug.log "launched"
            in
                ( t + period, input )

        doUpdate ( t, m ) =
            ( t, go m )

        checkTime : ( Time, ( Time, a ) ) -> Task Never a
        checkTime ( t_, ( t, m ) ) =
            if (t < t_ || stop m) then
                Task.succeed m
            else
                Task.succeed ( t, m ) |> loop

        startLoop =
            Time.now |> Task.map initialData

        loop : Task Never ( Time, a ) -> Task Never a
        loop task =
            let
                z =
                    Debug.log "looped" 0
            in
                task
                    |> Task.map doUpdate
                    |> Task.map2 (,) Time.now
                    |> Task.andThen checkTime
    in
        Task.perform done (loop startLoop)


collapseFromInventory : Model -> IndexS -> Model
collapseFromInventory model iS =
    case model.mouseOver of
        Just iW ->
            case collapseSpecific iW iS model.wave of
                Just wave ->
                    { model | wave = wave, pending = iW :: model.pending }

                Nothing ->
                    (model |> Debug.log "bad collapse")

        Nothing ->
            model


getTileSet : TileSet -> List State
getTileSet tileset =
    case tileset of
        CheckerboardAB ->
            checkerboardAB

        CheckerboardABCDEF ->
            checkerboardABCDEF

        Knot ->
            knot

        Dungeon ->
            dungeon


observationDeck : Model -> Wave
observationDeck { edges, propagator, wave } =
    observeThenPropagate
        selectorSimple
        simpleCollapser
        propagator
        wave


initWave : { b | height : Int, tileset : TileSet, width : Int } -> Wave
initWave { height, width, tileset } =
    let
        n =
            height * width

        m =
            tileset |> getTileSet |> List.length
    in
        True |> Array.repeat m |> Array.repeat n


view : Model -> Html.Html Action
view { wave, height, width, tileset, mouseOver, mouseTouch } =
    let
        states =
            getTileSet tileset

        statesArray =
            Array.fromList states

        drawPointDiv2 i point =
            let
                border =
                    case mouseOver of
                        Just j ->
                            if i == j then
                                [ selectBorder ]
                            else
                                []

                        Nothing ->
                            []
            in
                drawPointDiv statesArray
                    point
                    ([ onMouseEnter (MouseEnter i)
                     , onClick (Select i)
                     , centerPixel
                     ]
                        ++ border
                    )

        drawStateDiv2 iS state =
            div [ onClick (Collapse iS) ] [ drawStateDiv state ]

        waveDiv =
            drawWaveDiv width height drawPointDiv2 wave

        styles =
            waveContainer (width * 20) (height * 20)

        inventoryIndex =
            case mouseOver of
                Just i ->
                    i

                Nothing ->
                    mouseTouch

        pointDiv =
            Array.get inventoryIndex wave
                |> Maybe.map (\point -> viewPointInventory states drawStateDiv2 point)
                |> Maybe.withDefault (div [] [])
    in
        div []
            [ viewLinks
            , div [ onDoubleClick (Observe 0), onMouseLeave (MouseEnter -1), styles ] [ waveDiv ]
            , pointDiv
            ]


viewLinks : Html.Html Action
viewLinks =
    let
        message =
            ("Click on a tile to view states; click state to observe. "
                ++ "Double-click the map to automatically observe and propagate. "
                ++ "Click on a tileset name to reset. "
            )
                |> (\t -> a [] [ t |> text ])

        tileset ( name1, name2 ) =
            li [ onClick (ChangeTileSet name2) ] [ name1 |> text ]

        tilesets =
            [ ( "simple checkerboard", CheckerboardAB )
            , ( "dual checkerboard", CheckerboardABCDEF )
            , ( "knot", Knot )
            , ( "dungeon", Dungeon )
            ]
                |> List.map tileset

        sizes =
            [ ( " small ", ( 7, 7 ) )
            , ( " medium ", ( 14, 14 ) )
            , ( " huge (slow) ", ( 28, 28 ) )
            ]
                |> List.map sizeLink

        sizeLink ( name, shape ) =
            a [ onClick (ChangeSize shape) ] [ name |> text ]
    in
        div [] ([ message ] ++ tilesets ++ [ br [] [] ] ++ sizes)


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch [ AnimationFrame.diffs AnimationFrame ]



-- Sub.batch []
-- look at animation diff
-- get the time, project finish time
-- do some work
-- get the time. if it's past finishing, stash pending work and continuation
--   function.
-- otherwise, call continuation
-- 2. interactivity I: click to observe. double-click to reset.
-- 3. interactivity II: mouse-over to expand Point into available States (also
--    show entropy)
-- 4. interactivity III: JS file drop zone, convert to tileset
-- 5. interactivity IV: shift-click to spray uncertainty. double-click to reset.
-- 1. incorporate random Seed into selection. try using generate msg. need to
--    write a random version of observe that does random collapse. insulate
--    it from empty lists (aka Point with all False values).
