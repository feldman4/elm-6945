module Verlet exposing (..)

import Html exposing (text, div)
import Plot exposing (..)
import Plot.Line as Line
import Svg


main : Html.Html msg
main =
    let
        raw =
            doExample |> List.reverse |> toString |> text

        data =
            doExample |> List.map (\a -> ( a.t, a.x ))

        truth =
            doExample |> List.map (\a -> ( a.t, sin a.t ))
    in
        div []
            [ viewPlot data truth
              -- , div [] [ raw ]
            ]


doExample : History
doExample =
    let
        h =
            0.3

        tFinal =
            100

        g =
            sin
    in
        evolve (tFinal / h - 2 |> round) h (initKnown sin h)


viewPlot : List Point -> List Point -> Svg.Svg a
viewPlot data data2 =
    plot
        [ size ( 600, 300 )
        , margin ( 100, 100, 40, 100 )
        , id "PlotHint"
        , style [ ( "position", "relative" ) ]
        ]
        [ line
            [ Line.stroke "black"
            , Line.strokeWidth 1
            ]
            data
        , line
            [ Line.stroke "pink"
            , Line.strokeWidth 1
            ]
            data2
        ]


type alias Value =
    Float


type alias Time =
    Float


type alias Derivative2 =
    Float


type alias History =
    List { x : Value, t : Time }


type alias Forcing =
    Time -> Value -> Derivative2


type alias StepSize =
    Float


type alias Integrator =
    History -> Value


evolve : Int -> StepSize -> History -> History
evolve n h history =
    let
        integrator =
            stormer2 sinForcing h
    in
        if n == 0 then
            history
        else
            let
                history2 =
                    stepper h integrator history
            in
                evolve (n - 1) h history2


{-| Extend history. Sort of trivial.
-}
stepper : StepSize -> Integrator -> History -> History
stepper h integrator history =
    let
        ( t, x ) =
            getAt 0 history
    in
        { t = t + h, x = integrator history } :: history


{-| History was found with the same step size...
This function gives you the next value by sampling the forcing function to
approximate the second derivative.
-}
stormer2 : Forcing -> StepSize -> History -> Value
stormer2 f h history =
    let
        ( t0, x0 ) =
            getAt 0 history

        ( t1, x1 ) =
            getAt 1 history

        ( t2, x2 ) =
            getAt 2 history

        d2 =
            (h * h / 12) * (13 * (f t0 x0) - 2 * (f t1 x1) + (f t2 x2))
    in
        d2 + 2 * x0 - x1


getAt : Int -> History -> ( Time, Value )
getAt offset history =
    let
        { t, x } =
            history
                |> List.drop offset
                |> List.head
                |> Maybe.withDefault { x = 0, t = 0 }
    in
        ( t, x )


initKnown : (Time -> Value) -> StepSize -> History
initKnown x h =
    let
        g i =
            { t = 0 + i * h, x = x (0 + i * h) }
    in
        [ 2, 1, 0 ] |> List.map g


sinForcing : Forcing
sinForcing t x =
    -x
