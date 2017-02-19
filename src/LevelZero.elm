module LevelZero exposing (..)

import Types exposing (..)
import Common exposing (..)


parseLevel : String -> Level
parseLevel raw =
    []



{- minimum: collide against each layer's (?) AABB, then collide against contents
   automatic divison of large layers if it's an issue. just make big tiles

   if necessary, only render layers on screen by big tile method

   make Layer an Object with its data hidden inside

   for bullets, keep them inside Shooter. an Interaction can update them.
     - normal objects can be collided by filtering on Effect
     - bullets can be collided by filtering on Shooter
     - outcome depends on collided Object type, type contents, and Effects
       - to be an Effect, outcome must only involve common Object properties
          (no inference about parent Object type)
     - likely outcomes: lower HP, push back, spawn animation, bounce off wall,
          camera shake (Effect)

   for item pickups, collision results in
     - add an Item to Player's internal data

   Shoot Interaction, on keypress
     - get current weapon
     - start animation
     - make bullets
     - might need an Effect to deal with time-delay in consequences
     - cancel if in cooldown
     - in general, implement update as a pipeline that takes care of time-dependent updates
        - physics
        - cooldown
-}


type alias Shooter =
    { weapon : Int
    , bullets : List Int
    }


type alias Rect =
    Int


type alias NewObject =
    Object


collide : Rect -> Model -> Maybe Object
collide rect model =
    Nothing


onCollide : Object -> ( Maybe Object, Maybe NewObject )
onCollide obj =
    ( Just obj, Nothing )


init : ( Model, Cmd Action ) -> ( Model, Cmd Action )
init ( model, action ) =
    let
        textureActions =
            [ NapoleonT, Banana, BrickT ]
                |> List.map (textureAction textureURL)
                |> List.map (Cmd.map TexAction)

        newModel =
            { model | objects = [ Napoleon ] }
    in
        ( newModel, Cmd.batch (action :: textureActions) )


textureURL : NamedTexture -> String
textureURL name =
    case name of
        NapoleonT ->
            "http://i144.photobucket.com/albums/r196/salombo_photos/AB%20Seasons%20Game%20Icons/ABS_SPIcon.jpg"

        Banana ->
            "http://i144.photobucket.com/albums/r196/salombo_photos/AB%20Seasons%20Game%20Icons/ABS_GGGLIcon.jpg"

        BrickT ->
            "http://i144.photobucket.com/albums/r196/salombo_photos/AB%20Seasons%20Game%20Icons/ABS_GGGLIcon.jpg"
