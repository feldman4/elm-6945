module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import WebGL
import Types exposing (..)


view : Model -> Html Action
view { window } =
    let
        message =
            "asdf"
    in
        div
            [ style
                [ ( "width", toString window.width ++ "px" )
                , ( "height", toString window.height ++ "px" )
                , ( "position", "relative" )
                ]
            ]
            [ WebGL.toHtmlWith
                [ WebGL.depth 1
                , WebGL.antialias
                ]
                [ width window.width
                , height window.height
                , style [ ( "display", "block" ) ]
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "font-family", "monospace" )
                    , ( "color", "gray" )
                    , ( "text-align", "center" )
                    , ( "left", "20px" )
                    , ( "right", "20px" )
                    , ( "top", "20px" )
                    ]
                ]
                [ text message ]
            ]
