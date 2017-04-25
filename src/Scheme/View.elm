module Scheme.View exposing (..)

import Svg exposing (Svg, svg, circle, line, g, text_, text)
import Svg.Attributes exposing (..)
import TreeDiagram exposing (node, Tree, defaultTreeLayout)
import TreeDiagram.Svg exposing (draw)
import Html
import Scheme.Types exposing (..)


-- Tree to draw


coolTree : TreeDiagram.Tree Int
coolTree =
    node
        61
        [ node
            84
            [ node 22 []
            , node 38 []
            ]
        , node
            72
            [ node
                3
                [ node 59 []
                , node 29 []
                , node 54 []
                ]
            , node 25 []
            , node 49 []
            ]
        , node
            24
            [ node 2 []
            ]
        , node
            17
            [ node 26 []
            , node
                68
                [ node 13 []
                , node 36 []
                ]
            , node 86 []
            ]
        ]


(=>) : (String -> a) -> b -> a
(=>) prop value =
    prop (toString value)


{-| Represent edges as straight lines.
-}
drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 => 0, y1 => 0, x2 => targetX, y2 => targetY, stroke "black" ]
        []


{-| Represent nodes as circles with the node value inside.
-}
drawNode : a -> Svg msg
drawNode n =
    drawLabeledNode (n |> toString)


{-| Represent nodes as circles with the node value inside.
-}
drawLabeledNode : String -> Svg msg
drawLabeledNode label =
    g
        []
        [ circle [ r "16", stroke "black", fill "white", cx "0", cy "0" ] []
        , text_ [ textAnchor "middle", transform "translate(0,5)", fontSize "14" ] [ text label ]
        ]


viewTree : (a -> Svg msg) -> a -> Scheme.Types.Tree a -> Html.Html msg
viewTree drawNode branchLabel tree =
    let
        treeMap t =
            case t of
                Leaf x ->
                    node x []

                Branch xs ->
                    node branchLabel (List.map treeMap xs)
    in
        draw defaultTreeLayout drawNode drawLine (tree |> treeMap)


main : Html.Html msg
main =
    draw defaultTreeLayout drawNode drawLine coolTree
