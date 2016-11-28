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


filename : TextureName -> String
filename name =
    case name of
        Arctic ->
            "arctic"

        Barren ->
            "barren"


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


init : ( Model, Cmd Action )
init =
    ( { records = [], theta = 0 }
      -- ! [ Cmd.none ]
    , loadTexture "textures/terrain/arctic.jpg"
        |> Debug.log "loading texture"
        |> Task.attempt
            (\result ->
                case result of
                    Err err ->
                        TextureError
                            err
                            |> Debug.log "result"

                    Ok val ->
                        TextureLoaded
                            Barren
                            val
                            |> Debug.log "result2"
            )
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


hexagon : Float -> Float -> Drawable Attributes
hexagon x y =
    TriangleFan <|
        hexcorners <|
            { pos = (vec3 x y 0), coord = (vec3 0.5 0.5 0) }


makeOffset : Int -> Int -> Mat4
makeOffset x y =
    let
        uneven =
            Bitwise.and x 1

        unevenoffset =
            toFloat uneven * (sqrt (3) / 2)
    in
        makeTranslate <| vec3 (toFloat x * 3 / 2) (toFloat y * (sqrt (3)) - unevenoffset) 0


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
            |> Debug.log "hexcorner"



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
view { records, theta } =
    let
        tex =
            List.head (List.filter (\rec -> rec.name == Barren) records)
                |> Debug.log "records"
    in
        case tex of
            Nothing ->
                []
                    |> WebGL.toHtml [ width 800, height 800 ]

            Just tex ->
                [ render vertexShader fragmentShader (hexagon 0 0) { crate = tex.texture, perspective = perspective theta, gridoffset = makeOffset 0 0 }
                , render vertexShader fragmentShader (hexagon 0 1) { crate = tex.texture, perspective = perspective theta, gridoffset = makeOffset 0 1 }
                , render vertexShader fragmentShader (hexagon 1 0) { crate = tex.texture, perspective = perspective theta, gridoffset = makeOffset 1 0 }
                , render vertexShader fragmentShader (hexagon 2 0) { crate = tex.texture, perspective = perspective theta, gridoffset = makeOffset 2 0 }
                , render vertexShader fragmentShader (hexagon 1 1) { crate = tex.texture, perspective = perspective theta, gridoffset = makeOffset 1 1 }
                ]
                    |> WebGL.toHtml [ width 800, height 800 ]



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


fragmentShader : Shader {} { u | crate : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;


void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]
