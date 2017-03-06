module WFC.View exposing (..)

import WFC.Types exposing (..)
import WFC.Utilities exposing (..)
import WFC.Tile exposing (..)
import Html exposing (text, li, div, tr, td, br)
import Html.Attributes exposing (style)
import Array exposing (Array)
import List.Extra exposing (lift2, lift4, (!!))
import Collage exposing (rect, circle, filled, group, collage, move, moveX, moveY, toForm, scale, Shape, Form)
import Element exposing (Element, widthOf, heightOf)
import Color exposing (greyscale, red)


drawBitmap : State -> Form
drawBitmap state =
    let
        drawPixel i x =
            rect 1 1
                |> filled (greyscale x)
                |> moveX (i |> toFloat)

        drawRow : T3 Int -> Form
        drawRow ( a, b, c ) =
            [ a, b, c ]
                |> List.map toFloat
                |> List.indexedMap drawPixel
                |> group

        drawSquare ( a, b, c ) =
            [ a, b, c ]
                |> List.map drawRow
                |> List.indexedMap (\i s -> moveY (-i |> toFloat) s)
                |> group
    in
        drawSquare state


drawPoint : List State -> Form
drawPoint stateList =
    stateList
        |> List.head
        |> Maybe.withDefault checkerboardA
        |> drawBitmap


drawGrid : Int -> Int -> Float -> List Form -> Element
drawGrid x y s forms =
    let
        ( x_, y_ ) =
            ( toFloat x, toFloat y )

        shift n z =
            let
                ( i, j ) =
                    to2DIndex x y n

                adjustX =
                    (toFloat j) - x_ / 2 - 0.5

                adjustY =
                    y_ / 2 - (toFloat i) + 0.5
            in
                move ( adjustX, adjustY ) z

        border z =
            s * (toFloat z) |> round
    in
        forms
            |> List.indexedMap shift
            |> group
            |> Collage.scale s
            |> (\f -> collage (border (x + 2)) (border (y + 2)) [ f ])


drawWave : Int -> Int -> Float -> List State -> Wave -> Element
drawWave x y s stateList wave =
    let
        stateArray =
            stateList |> Array.fromList
    in
        wave
            |> Array.map (pointToStates stateArray)
            |> Array.map drawPoint
            |> Array.toList
            |> drawGrid x y s



-- |> (\f -> collage x y [ f ])


placeSingle : Form -> Html.Html msg
placeSingle form =
    collage 300 300 [ form ] |> Element.toHtml


placeWave : Int -> Int -> Element -> Html.Html msg
placeWave x y element =
    let
        ( dx, dy ) =
            ( widthOf element, heightOf element )
    in
        element
            |> toForm
            |> scale (x // dx |> toFloat)
            |> (\f -> collage x y [ f ])
            |> Element.toHtml


viewWave : Int -> Int -> Wave -> Html.Html msg
viewWave x y wave =
    let
        bitAdd : Array Bool -> Int
        bitAdd array =
            array
                |> Array.indexedMap
                    (\i a ->
                        if a then
                            2 ^ i
                        else
                            0
                    )
                |> Array.foldl (+) 0

        waves =
            wave
                |> Array.map bitAdd
                |> Array.toList
                |> List.Extra.groupsOf x
                |> toTable
    in
        waves


toTable : List (List a) -> Html.Html msg
toTable entries =
    let
        tdStyle =
            style [ ( "border", "1px solid black" ) ]

        makeRow xs =
            xs |> List.map (\x -> td [ tdStyle ] [ x |> toString |> text ])
    in
        entries
            |> List.map (\xs -> tr [] (makeRow xs))
            |> div []


{-| Test neighbor compatibility for grid of States. Get (Edge -> Bool) from
Propagator.
-}
testViewOverlaps : Int -> (Edge -> Bool) -> Html.Html msg
testViewOverlaps n edgeTest =
    let
        test =
            neighbors |> List.map edgeTest

        neighbors =
            List.range 0 (n * 3) |> List.map (\x -> ( n * 3 // 2, x ))

        overlapsDiv =
            List.map2 (,) neighbors test
                |> List.map (\t -> li [] [ t |> toString |> text ])
                |> div []
    in
        overlapsDiv
