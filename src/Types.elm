module Types exposing (..)

import Minimum
import Common
import Frame exposing (Frame)
import EveryDict exposing (EveryDict)


type Object
    = Brick
    | Napoleon
    | LayerObj Layer


type alias Level =
    List Layer


type alias Layer =
    { tiles : List Tile
    , visible : Bool
    , zdepth : Int
    }


type alias Tile =
    { texture : NamedTexture
    , texCoord : ( Float, Float )
    , destroyed : Bool
    }


type Interaction
    = Physics
    | Collision


type NamedTexture
    = NapoleonT
    | Banana
    | BrickT


type Action
    = MinAction Minimum.Action
    | TexAction (Common.TextureAction NamedTexture)


type alias Model =
    Minimum.Model
        { objects : List Object
        , interactions : List Interaction
        , camera : Frame
        , textures : EveryDict Int Int
        }
