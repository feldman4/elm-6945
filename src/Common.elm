module Common exposing (..)

import WebGL.Texture as WGLT
import Task


type TextureAction a
    = TextureError WGLT.Error
    | TextureLoaded ( a, WGLT.Texture )


{-| Supply a function that turns a NamedTexture type into a URL.
Cmd.map the resulting TextureAction into a regular Action.
-}
textureAction : (a -> String) -> a -> Cmd (TextureAction a)
textureAction textureURL name =
    textureURL name
        |> WGLT.load
        |> Task.attempt
            (\result ->
                case result of
                    Err err ->
                        TextureError err

                    Ok val ->
                        TextureLoaded ( name, val )
            )
