module Napoleon exposing (..)

import Html
import Minimum
import Frame exposing (Frame)
import EveryDict exposing (EveryDict)
import LevelZero
import Types exposing (..)
import View exposing (..)


main : Program Never Model Action
main =
    Html.program
        { init = init
        , update = (\msg model -> model ! [])
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Action )
init =
    let
        ( minModel, action ) =
            Minimum.init

        model =
            { objects = []
            , interactions = []
            , keys = minModel.keys
            , gamepad = minModel.gamepad
            , window = minModel.window
            , clock = minModel.clock
            , textures = EveryDict.empty
            , camera = Frame.identity
            }
    in
        ( model, Cmd.map MinAction action )
            |> LevelZero.init


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        MinAction act ->
            let
                ( newModel, newAction ) =
                    Minimum.update act model
            in
                ( newModel, Cmd.map MinAction newAction )

        TexAction act ->
            model ! []


subscriptions : Minimum.Model a -> Sub Action
subscriptions model =
    Sub.map MinAction (Minimum.subscriptions model)
