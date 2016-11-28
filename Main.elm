module Main exposing (..)

import Color exposing (..)
import Math.Vector3 exposing (..)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html, div, text, hr)
import Html.Attributes exposing (width, height, style)
import Window
import Mouse
import Task exposing (Task)
import VirtualDom
import Json.Decode as Json
import Bitwise


type alias Model =
    { size : Window.Size
    , position : Mouse.Position
    , position2 : Position
    , maybeTexture :
        Maybe WebGL.Texture
        --, textures : ( Maybe Texture, Maybe Texture )
    }


type alias Position =
    { x : Int, y : Int }


init : ( Model, Cmd Action )
init =
    ({ size = { width = 800, height = 800 }
     , position = { x = 0, y = 0 }
     , position2 = { x = 0, y = 0 }
     , maybeTexture = Nothing
     }
        ! [ WebGL.loadTexture "/textures/textures_terrain.jpg"
                |> Task.attempt
                    (\result ->
                        case result of
                            Err err ->
                                TextureError err

                            Ok val ->
                                TextureLoad val
                    )
            -- , Task.perform Resize Window.size
          ]
    )


type Action
    = MouseClick Mouse.Position
    | Resize Window.Size
    | MouseClick2 Mouse.Position
    | TextureLoad WebGL.Texture
    | TextureError WebGL.Error


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ Window.resizes Resize
        , Mouse.clicks MouseClick
        ]


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Resize size ->
            ( { model | size = size }, Cmd.none )

        MouseClick position ->
            ( { model | position = position }, Cmd.none )

        MouseClick2 position ->
            ( { model | position2 = position }, Cmd.none )

        TextureLoad texture ->
            { model | maybeTexture = Just texture } ! []

        TextureError _ ->
            Debug.crash "Error loading texture"


view : Model -> Html Action
view model =
    div []
        [ WebGL.toHtml
            [ width model.size.width
            , height model.size.height
            , style
                [ ( "margin", "200px" )
                , ( "backgroundColor", "blue" )
                ]
            , VirtualDom.onWithOptions "click" options (Json.map MouseClick2 offsetPosition)
            ]
            (case model.maybeTexture of
                Nothing ->
                    []

                Just texture ->
                    scene texture
            )
        , hr [] []
        , text (toString model)
        ]


type alias Options =
    { preventDefault : Bool, stopPropagation : Bool }


options : Options
options =
    { preventDefault = True, stopPropagation = True }


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MESHES - create a cube in which each vertex has a position and color


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


hexagon : Color -> Drawable Vertex
hexagon color =
    Triangle
        << List.concat
    <|
        [ hexface color
        ]


colorToVec : Color -> Vec3
colorToVec color =
    let
        c =
            toRgb color
    in
        vec3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)


hexface : List ( Vertex, Vertex, Vertex )
hexface =
    let
        center =
            vec3 0 0 0

        a =
            add (vec3 -1 -(sqrt (3)) 0) center

        b =
            add (vec3 1 -(sqrt (3)) 0) center

        c =
            add (vec3 2 0 0) center

        d =
            add (vec3 1 (sqrt (3)) 0) center

        e =
            add (vec3 -1 (sqrt (3)) 0) center

        f =
            add (vec3 -2 0 0) center

        color =
            colorToVec Color.red

        --vred position = Vertex Color.red position
        vertex position =
            Vertex color position
    in
        [ ( Vertex color a, Vertex color b, Vertex color center )
        , ( Vertex color b, Vertex color c, Vertex color center )
        , ( Vertex color c, Vertex color d, Vertex color center )
        , ( Vertex color d, Vertex color e, Vertex color center )
        , ( Vertex color e, Vertex color f, Vertex color center )
        , ( Vertex color f, Vertex color a, Vertex color center )
        ]



-- VIEW


scene : Texture -> List Renderable
scene texture =
    [ render vertexShader fragmentShader (hexagon green) (uniforms 0 0 (vec2 1 1) texture)
      -- , render vertexShader fragmentShader (hexagon red )    (uniforms 1 0 2 texture)
      -- , render vertexShader fragmentShader (hexagon yellow ) (uniforms 1 2 3 texture)
      -- , render vertexShader fragmentShader (hexagon yellow ) (uniforms 2 1 4 texture)
      -- , render vertexShader fragmentShader (hexagon yellow ) (uniforms 2 0 1 texture)
      -- , render vertexShader fragmentShader (hexagon blue )   (uniforms 0 1 1 texture)
      -- , render vertexShader fragmentShader (hexagon yellow ) (uniforms 0 -1 1 texture)
      -- , render vertexShader fragmentShader (hexagon yellow ) (uniforms 1 1 1 texture)
    ]


makeOffset : Int -> Int -> Mat4
makeOffset x y =
    let
        uneven =
            Bitwise.and x 1

        unevenoffset =
            toFloat uneven * (sqrt (3) / 2)
    in
        makeTranslate <| vec3 (toFloat x * 3 / 2) (toFloat y * (sqrt (3)) - unevenoffset) 0


type alias Uniforms =
    { offset : Mat4
    , rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    , textureSize : Vec2
    , texture : Texture
    , texturePos : Vec2
    }


uniforms : Int -> Int -> Vec2 -> Texture -> Uniforms
uniforms x y texturePos texture =
    { offset = makeOffset x y
    , rotation = mul (makeRotate 0 (vec3 0 1 0)) (makeRotate 0 (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 15) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    , textureSize = vec2 (toFloat (Tuple.first (WebGL.textureSize texture))) (toFloat (Tuple.second (WebGL.textureSize texture)))
    , texture = texture
    , texturePos = texturePos
    }


type alias Varyings =
    { vTexturePos : Vec2 }


type alias Attributes =
    { position : Vec3
    , color : Vec3
    }



-- SHADERS


vertexShader : Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|

attribute vec3 position;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
uniform mat4 offset;
varying vec2 vTexturePos;
void main () {
    gl_Position = perspective * camera * offset * rotation * vec4(position, 2.0);
    vTexturePos = vec2 (position.x, position.y);
}

|]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

  precision mediump float;
  uniform float shade;
  uniform vec2 textureSize;
  uniform vec2 texturePos;
  uniform sampler2D texture;
  varying vec2 vTexturePos;
  void main () {
      gl_FragColor = texture2D(texture, vTexturePos);
    }
  |]
