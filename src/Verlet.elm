module Verlet exposing (..)

import Html exposing (text, div, p, br, span, pre)
import Plot exposing (..)
import Plot.Line as Line
import Svg
import Arithmetic exposing (..)


-- EXAMPLES


doExample : History
doExample =
    let
        h =
            Num 0.1

        n =
            300

        t =
            Num 0

        x =
            makeUnaryOp "sin" sin

        ( x0, x1, x2 ) =
            ( x t, x (t -. h), x (t -. (makeNum 2) *. h) )

        history =
            makeInitialHistory t h x0 x1 x2
    in
        evolver sinForcing h stormer2 history n


doExample2 : Value
doExample2 =
    let
        ( h, t ) =
            ( Sym "h", Sym "t" )

        ( x0, x1, x2 ) =
            ( Sym "x[t]", Sym "x[t-h]", Sym "x[t-2h]" )

        x =
            makeUnaryOp "x" sin

        history =
            makeInitialHistory t h x0 x1 x2
    in
        evolver sinForcing h stormer2 history 3 |> getAt 0 |> Tuple.second


sinForcing : Forcing
sinForcing t x =
    let
        negate =
            makeUnaryOp "negate" (\x -> -x)
    in
        negate x



-- TYPES


{-| Redefine Quantity and Arithmetic.elm to change arithmetic.
-}
type alias Value =
    Quantity


type alias Time =
    Quantity


type alias Derivative2 =
    Quantity


type alias History =
    List { x : Value, t : Time }


type alias Forcing =
    Time -> Value -> Derivative2


type alias StepSize =
    Time


type alias Integrator =
    History -> Value


makeNum : Number -> Quantity
makeNum n =
    Num n



-- INTEGRATOR


stepper : StepSize -> Integrator -> History -> History
stepper h integrator history =
    let
        ( t, x ) =
            getAt 0 history
    in
        { t = t +. h, x = integrator history } :: history


{-| Create a function that can update a history, with the following built in:
- integration scheme
- step size
- forcing F, i.e., the right side of the differential equation

Why do we pass makeIntegrator, instead of just making the integrator before
we call this?
-}
evolver : Forcing -> StepSize -> (Forcing -> StepSize -> Integrator) -> (History -> Int -> History)
evolver f h makeIntegrator =
    let
        integrator =
            makeIntegrator f h

        step =
            stepper h integrator

        evolve history n =
            if n > 0 then
                evolve (step history) (n - 1)
            else
                history
    in
        evolve


{-| Provide first three points and step size:
(t, x0
(t - h, x1)
(t - 2h, x2)
-}
makeInitialHistory : Time -> StepSize -> Value -> Value -> Value -> History
makeInitialHistory t h x0 x1 x2 =
    [ { t = t, x = x0 }
    , { t = t -. h, x = x1 }
    , { t = t -. ((makeNum 2) *. h), x = x2 }
    ]


{-| This function gives you the next value by sampling the forcing function to
approximate the second derivative.
-}
stormer2 : Forcing -> StepSize -> Integrator
stormer2 f h history =
    let
        ( t0, x0 ) =
            getAt 0 history

        ( t1, x1 ) =
            getAt 1 history

        ( t2, x2 ) =
            getAt 2 history

        ( a, b, c, d ) =
            ( makeNum 12, makeNum 13, makeNum -2, makeNum 2 )

        d2 =
            (h *. h /. a) *. (b *. (f t0 x0) +. c *. (f t1 x1) +. (f t2 x2))
    in
        (d *. x0 +. ((makeNum -1) *. x1)) +. d2


getAt : Int -> History -> ( Time, Value )
getAt offset history =
    let
        { t, x } =
            history
                |> List.drop offset
                |> List.head
                |> Maybe.withDefault { x = Num 0, t = Num 0 }
    in
        ( t, x )



-- VIEW


main : Html.Html msg
main =
    let
        ( ts, xs ) =
            doExample |> List.reverse |> historyToPoints |> List.unzip |> Debug.log "w"

        trueXs =
            ts |> List.map sin

        firstThing =
            viewPlot (List.map2 (,) ts xs) (List.map2 (,) ts trueXs)

        secondThing =
            doExample2
                |> toString
                |> pp 0
                |> Debug.log "string"
                |> paragraph
    in
        div [] [ firstThing, secondThing ]


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


{-| Scheme style printing, not smart about line breaks.
-}
pp : Int -> String -> String
pp indent string =
    let
        indentation =
            String.repeat indent "  "
    in
        case String.uncons string of
            Just ( '(', tail ) ->
                "\n" ++ indentation ++ "(" ++ (pp (indent + 1) tail)

            Just ( ')', tail ) ->
                String.cons ')' (pp (indent - 1) tail)

            Just ( c, tail ) ->
                String.cons c (pp indent tail)

            Nothing ->
                ""


{-| Preserve newlines in HTML.
-}
paragraph : String -> Html.Html msg
paragraph input =
    String.split "\n" input
        |> List.map (\s -> span [] [ text s ])
        |> List.intersperse (br [] [])
        |> pre []


{-| Unwrap for plotting.
-}
historyToPoints : History -> List Point
historyToPoints history =
    let
        unwrap { t, x } =
            case ( t, x ) of
                ( Num t_, Num x_ ) ->
                    ( t_, x_ )

                _ ->
                    ( 0, 0 )
    in
        history |> List.map unwrap
