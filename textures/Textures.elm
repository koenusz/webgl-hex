module Textures.Textures
    exposing
        ( TextureId(..)
        , filename
        , textures
        , loadTextures
        , loadedTextures
        , Model
        , initModel
        , Action
        , update
        )

import AllDict exposing (AllDict)
import WebGL
import Task


type alias Textures =
    AllDict TextureId TextureData String


type State
    = Loading
    | Running


type alias Model =
    { textures : Textures
    , state : State
    , imagesUrl : String
    }


initModel : ( Model, Cmd Action )
initModel =
    let
        model =
            { textures = textures
            , state = Loading
            , imagesUrl = "./terrain"
            }
    in
        ( model
        , loadImage model.imagesUrl Ocean
        )


type Action
    = TextureLoaded TextureId (Maybe WebGL.Texture)


type TextureId
    = Ocean
    | Arctic
    | Tundra
    | Temparate
    | Boreal
    | Jungle
    | Desert
    | Barren
    | Lava
    | Savanna
    | Mountains
    | Plains


filename : TextureId -> String
filename textureId =
    case textureId of
        Ocean ->
            "ocean.jpg"

        Arctic ->
            "arctic.png"

        Tundra ->
            "tundra.png"

        Temparate ->
            "temparate.png"

        Boreal ->
            "boreal.png"

        Jungle ->
            "jungle.png"

        Desert ->
            "desert.png"

        Barren ->
            "barren.png"

        Lava ->
            "lava.png"

        Savanna ->
            "savanna.png"

        Mountains ->
            "mountains.png"

        Plains ->
            "plains.png"


textures : Textures
textures =
    AllDict.fromList
        filename
        [ ( Ocean, TextureData ( 1, 1 ) 1 Nothing )
        , ( Arctic, TextureData ( 1, 1 ) 1 Nothing )
        , ( Tundra, TextureData ( 1, 1 ) 1 Nothing )
        , ( Temparate, TextureData ( 1, 1 ) 1 Nothing )
        , ( Boreal, TextureData ( 1, 1 ) 1 Nothing )
        , ( Jungle, TextureData ( 1, 1 ) 1 Nothing )
        , ( Desert, TextureData ( 1, 1 ) 1 Nothing )
        , ( Barren, TextureData ( 1, 1 ) 1 Nothing )
        , ( Lava, TextureData ( 1, 1 ) 1 Nothing )
        , ( Savanna, TextureData ( 1, 1 ) 1 Nothing )
        , ( Mountains, TextureData ( 1, 1 ) 1 Nothing )
        , ( Plains, TextureData ( 1, 1 ) 1 Nothing )
        ]


loadedTextures : Textures -> Int
loadedTextures textures =
    (1
        - toFloat (List.length (loadTextures textures))
        / toFloat (AllDict.size textures)
    )
        * 100
        |> round


loadTextures : Textures -> List TextureId
loadTextures textures =
    AllDict.toList textures
        |> List.filter (\( id, data ) -> data.texture == Nothing)
        |> List.map Tuple.first


type alias TextureWithSize =
    { size : ( Float, Float )
    , texture : WebGL.Texture
    }


type alias TextureData =
    { size : ( Float, Float )
    , frames : Int
    , texture : Maybe TextureWithSize
    }


loadImage : String -> TextureId -> Cmd Action
loadImage imagesUrl textureId =
    WebGL.loadTexture (imagesUrl ++ "/" ++ filename textureId)
        |> Task.attempt (Result.toMaybe >> TextureLoaded textureId)


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TextureLoaded textureId maybeTexture ->
            let
                loadTexture =
                    case AllDict.get textureId model.textures of
                        Just data ->
                            AllDict.insert
                                textureId
                                { data
                                    | texture =
                                        Maybe.map
                                            (\texture ->
                                                { size =
                                                    ( toFloat (Tuple.first (WebGL.textureSize texture))
                                                    , toFloat (Tuple.second (WebGL.textureSize texture))
                                                    )
                                                , texture = texture
                                                }
                                            )
                                            maybeTexture
                                }
                                model.textures

                        Nothing ->
                            model.textures

                newModel =
                    { model | textures = loadTexture }

                texturesToLoad =
                    loadTextures newModel.textures
            in
                if List.length texturesToLoad == 0 then
                    ( { newModel | state = Running }
                    , Cmd.none
                    )
                else
                    ( { newModel | state = Loading }
                    , Cmd.batch (List.map (loadImage model.imagesUrl) texturesToLoad)
                    )
