module Texture.LazyTexture exposing (load)

import WebGL exposing (Texture, Error, loadTexture)
import Task exposing (Task)
import Dict exposing (Dict, get, insert)


type Action
    = TextureError Error
    | TextureLoaded String Texture


type alias Model =
    { textures : Dict String Texture }


grass : Model -> Maybe Texture
grass model =
    get "ground_leaves_d" model.textures


sand : Model -> Maybe Texture
sand grass model =
    get "island_sand_d" model.textures


load : String -> Cmd Action
load name =
    loadTexture "textures/terrain/ground_leaves_d.jpg"
        |> Task.attempt
            (\result ->
                case result of
                    Err err ->
                        TextureError err

                    Ok val ->
                        TextureLoaded name val
            )


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TextureError err ->
            ( model, Cmd.none )

        TextureLoaded name texture ->
            ( { model | textures = insert name texture textures }, Cmd.none )
