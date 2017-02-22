module Verlet exposing (..)

import Html exposing (text)
import Plot exposing (..)


main : Html.Html msg
main =
    doExample |> List.reverse |> toString |> text



-- data1 : List ( Float, Float )
-- data1 =
--     [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ), ( 4, 5 ), ( 5, 8 ), ( 6, 13 ), ( 7, 21 ), ( 8, 34 ), ( 9, 55 ), ( 10, 87 ) ]
--
--
--
-- viewPlot =
--     plot
--         [ size ( 600, 300 )
--         , margin ( 100, 100, 40, 100 )
--         , id "PlotHint"
--         , style [ ( "position", "relative" ) ]
--         ]
--         [ line
--             [ Line.stroke "#556270"
--             , Line.strokeWidth 1
--             ]
--              data1


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


doExample : History
doExample =
    let
        h =
            0.01

        tFinal =
            1

        g =
            sin
    in
        evolve (tFinal / h - 2 |> round) h (initKnown sin h)


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
            (h ^ 2 / 12) * (13 * (f t0 x0) - 2 * (f t1 x1) + (f t2 x2))
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
