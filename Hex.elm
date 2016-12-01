module Hex exposing (..)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Task
import Time exposing (Time)
import WebGL exposing (..)
import Html exposing (Html)


-- import AnimationFrame

import Html.Attributes exposing (width, height)
import Debug
import Bitwise


type TextureName
    = Arctic
    | Barren
    | Ocean


filename : TextureName -> String
filename name =
    case name of
        Arctic ->
            "arctic"

        Barren ->
            "barren"

        Ocean ->
            "ocean"


type alias TextureRecord =
    { name : TextureName
    , texture : Texture
    }


type alias Model =
    { records : List TextureRecord
    , theta : Float
    }


type Action
    = TextureError Error
    | TextureLoaded TextureName Texture
    | Animate Time


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TextureError err ->
            Debug.log "eror" <|
                ( model, Cmd.none )

        TextureLoaded name texture ->
            let
                newRecord =
                    { name = name, texture = texture }
            in
                Debug.log "loading" <|
                    ( { model | records = newRecord :: model.records }, Cmd.none )

        Animate dt ->
            ( { model
                | theta =
                    model.theta
                        + dt
                        / 10000
              }
            , Cmd.none
            )


loadTex : TextureName -> Cmd Action
loadTex name =
    let
        path =
            "textures/terrain/"
                ++ filename name
                ++ ".jpg"
    in
        loadTexture path
            |> Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            TextureError
                                err

                        Ok val ->
                            TextureLoaded
                                name
                                val
                )


init : ( Model, Cmd Action )
init =
    ({ records = [], theta = 0 }
        ! [ loadTex Arctic
          , loadTex Barren
          , loadTex Ocean
          ]
     -- ! [ Cmd.none ]
     -- , loadTexture "textures/terrain/arctic.jpg"
     --     |> Debug.log "loading texture"
     --     |> Task.attempt
     --         (\result ->
     --             case result of
     --                 Err err ->
     --                     TextureError
     --                         err
     --
     --                 Ok val ->
     --                     TextureLoaded
     --                         Barren
     --                         val
     -- )
    )


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions =
            (\model -> Sub.none)
            --(\model -> AnimationFrame.diffs Animate)
        , update = update
        }



-- MESHES


type alias Attributes =
    { pos : Vec3, coord : Vec3 }


hexagon : Int -> Int -> Drawable Attributes
hexagon x y =
    TriangleFan <|
        hexcorners <|
            { pos = (vec3 (toFloat x) (toFloat y) 0), coord = (vec3 0.5 0.5 0) }


makeOffset : Int -> Int -> Mat4
makeOffset x y =
    let
        size =
            1

        width =
            size * 2

        height =
            (sqrt (3) / 2) * width

        uneven =
            Bitwise.and x 1

        unevenoffset =
            toFloat uneven * height / 2
    in
        makeTranslate <| vec3 (toFloat x * (1 / 2)) (toFloat y * (height / 2.35) - unevenoffset) 0


hexcorners : Attributes -> List Attributes
hexcorners center =
    let
        meshX =
            getX center.pos

        meshY =
            getY center.pos

        texX =
            getX center.coord

        texY =
            getY center.coord

        a =
            { pos = hexCorner meshX meshY 1 0, coord = hexCorner texX texY 0.5 0 }

        b =
            { pos = hexCorner meshX meshY 1 1, coord = hexCorner texX texY 0.5 1 }

        c =
            { pos = hexCorner meshX meshY 1 2, coord = hexCorner texX texY 0.5 2 }

        d =
            { pos = hexCorner meshX meshY 1 3, coord = hexCorner texX texY 0.5 3 }

        e =
            { pos = hexCorner meshX meshY 1 4, coord = hexCorner texX texY 0.5 4 }

        f =
            { pos = hexCorner meshX meshY 1 5, coord = hexCorner texX texY 0.5 5 }

        corners =
            hexCorner
    in
        [ center
        , a
        , b
        , c
        , d
        , e
        , f
        ]


hexCorner : Float -> Float -> Float -> Int -> Vec3
hexCorner x y size i =
    let
        angle_deg =
            60 * i

        angle_rad =
            pi / 180 * toFloat angle_deg
    in
        vec3 (x + size * cos (angle_rad)) (y + size * sin (angle_rad)) 0



-- VIEW


perspective : Float -> Mat4
perspective angle =
    List.foldr mul
        Math.Matrix4.identity
        [ perspectiveMatrix
        , camera
        , makeRotate (3 * angle) (vec3 0 1 0)
        , makeRotate (2 * angle) (vec3 1 0 0)
        ]


perspectiveMatrix : Mat4
perspectiveMatrix =
    makePerspective 45 1 0.01 100


camera : Mat4
camera =
    makeLookAt (vec3 0 0 15) (vec3 0 0 0) (vec3 0 1 0)


view : Model -> Html Action
view model =
    [ renderHex 0 0 Arctic model
    , renderHex 0 1 Barren model
    , renderHex 0 2 Arctic model
    , renderHex 0 3 Arctic model
    , renderHex 1 0 Barren model
    , renderHex 2 0 Ocean model
    , renderHex 1 1 Barren model
    ]
        |> WebGL.toHtml [ width 800, height 800 ]


renderHex : Int -> Int -> TextureName -> Model -> Renderable
renderHex x y textureName model =
    let
        tex =
            List.head (List.filter (\rec -> rec.name == textureName) model.records)
    in
        case tex of
            Nothing ->
                Renderable

            Just tex ->
                render vertexShader fragmentShader (hexagon x y) { selectedTexture = tex.texture, perspective = perspective model.theta, gridoffset = makeOffset x y }



-- SHADERS


vertexShader : Shader { pos : Vec3, coord : Vec3 } { u | perspective : Mat4, gridoffset : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

attribute vec3 pos;
attribute vec3 coord;
uniform mat4 perspective;
uniform mat4 gridoffset;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * gridoffset * vec4(pos, 1);
  vcoord = coord.xy;
}

|]


fragmentShader : Shader {} { u | selectedTexture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|

precision mediump float;
uniform sampler2D selectedTexture;
varying vec2 vcoord;


void main () {
  gl_FragColor = texture2D(selectedTexture, vcoord);
}

|]
