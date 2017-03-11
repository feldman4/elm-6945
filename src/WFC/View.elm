module WFC.View exposing (..)

import WFC.Types exposing (..)
import WFC.Utilities exposing (..)
import WFC.Tile exposing (..)
import Html exposing (Html, text, li, div, tr, td, br)
import Html.Attributes exposing (style)
import Array exposing (Array)
import List.Extra exposing (lift2, lift4, (!!))
import Collage exposing (rect, circle, filled, group, collage, move, moveX, moveY, toForm, scale, Shape, Form)
import Element exposing (Element, widthOf, heightOf)
import Color exposing (greyscale, red, toRgb)
import Css exposing (pct, px)


waveContainer : Int -> Int -> Html msg -> Html msg
waveContainer x y contents =
    let
        s =
            [ Css.width (x |> toFloat |> px)
            , Css.height (y |> toFloat |> px)
            , Css.position Css.relative
            , Css.overflow Css.hidden
            ]
    in
        div [ styles s ] [ contents ]


drawPointDiv : Array State -> Point -> Html msg
drawPointDiv stateArray point =
    let
        stateList =
            pointToStates stateArray point
    in
        stateList
            |> superimposeStates
            |> drawStateDivNorm (List.length stateList)


superimposeStates : List State -> State
superimposeStates stateList =
    let
        n =
            List.length stateList

        zero =
            ( ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ) )

        f s1 s2 =
            let
                ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
                    s1

                ( ( b11, b12, b13 ), ( b21, b22, b23 ), ( b31, b32, b33 ) ) =
                    s2
            in
                ( ( a11 + b11, a12 + b12, a13 + b13 )
                , ( a21 + b21, a22 + b22, a23 + b23 )
                , ( a31 + b31, a32 + b32, a33 + b33 )
                )
    in
        List.foldl f zero stateList


drawWaveDiv : Int -> Int -> (Point -> Html msg) -> Wave -> Html msg
drawWaveDiv x y pointToDiv wave =
    let
        positionAndZoom n =
            [ toCssPositionOffset x y ( n % y - 1, n // x - 1 )
            , toCssZoom x y 3
            ]
                |> List.concat
                |> styles

        pointDivs =
            wave
                |> Array.toList
                |> List.map pointToDiv
                |> List.indexedMap (\n a -> div [ positionAndZoom n ] [ a ])
    in
        div [ styles [ Css.height (pct 100), Css.width (pct 100) ] ] pointDivs


drawStateDiv : State -> Html msg
drawStateDiv state =
    let
        drawPixel i x =
            let
                color =
                    [ Css.backgroundColor (x |> toFloat |> greyscale |> toCssColor) ]

                position =
                    toCssPosition 3 3 i
            in
                div [ styles (color ++ position) ] []

        drawSquare ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
            [ a11, a12, a13, a21, a22, a23, a31, a32, a33 ]
                |> List.indexedMap drawPixel
                |> div [ styles [ Css.height (pct 100), Css.width (pct 100) ] ]
    in
        drawSquare state


drawStateDivNorm : Int -> State -> Html msg
drawStateDivNorm norm state =
    let
        drawPixel i x =
            let
                color =
                    [ Css.backgroundColor
                        (x
                            |> (\x -> (toFloat x) / (toFloat norm))
                            |> greyscale
                            |> toCssColor
                        )
                    ]

                position =
                    toCssPosition 3 3 i
            in
                div [ styles (color ++ position) ] []

        drawSquare ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
            [ a11, a12, a13, a21, a22, a23, a31, a32, a33 ]
                |> List.indexedMap drawPixel
                |> div [ styles [ Css.height (pct 100), Css.width (pct 100) ] ]
    in
        drawSquare state


{-| Creates attributes for a div within a grid.
-}
toCssPosition : Int -> Int -> Int -> List Css.Mixin
toCssPosition x y n =
    toCssPositionOffset x y ( n % y, n // x )


toCssPositionOffset : Int -> Int -> ( Int, Int ) -> List Css.Mixin
toCssPositionOffset x y ( i, j ) =
    let
        left =
            i |> toFloat |> (\a -> (a * 100) / (toFloat x)) |> pct

        top =
            j |> toFloat |> (\a -> (a * 100) / (toFloat y)) |> pct

        invPct z =
            (100 / (toFloat z)) |> pct

        shape =
            [ Css.height (invPct y), Css.width (invPct x) ]
    in
        [ Css.position Css.relative, Css.left left, Css.top top, Css.position Css.absolute ] ++ shape


toCssZoom : Int -> Int -> Float -> List Css.Mixin
toCssZoom x y zoom =
    let
        invPct z =
            (zoom * 100 / (toFloat z)) |> pct
    in
        [ Css.height (invPct y), Css.width (invPct x) ]


toCssColor : Color.Color -> Css.Color
toCssColor color =
    let
        c =
            color |> toRgb
    in
        Css.rgba c.red c.green c.blue c.alpha


styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


drawPointBools : Point -> Element
drawPointBools point =
    let
        n =
            Array.length point

        draw i x =
            let
                color =
                    if x then
                        greyscale 1
                    else
                        greyscale 0

                idx =
                    i |> toFloat
            in
                rect 1 1
                    |> filled color
                    |> moveX (0.5 + idx - (n |> toFloat) / 2)
    in
        point
            |> Array.toList
            |> List.indexedMap draw
            |> group
            |> scale 100
            |> collage1 (n * 100) 100


collage1 : Int -> Int -> Form -> Element
collage1 x y form =
    collage x y [ form ]


scaleElement : Float -> Element -> Element
scaleElement s element =
    let
        convert f =
            f element |> toFloat |> (*) s |> ceiling

        ( width, height ) =
            ( convert widthOf, convert heightOf )
    in
        element |> toForm |> scale s |> collage1 width height


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
