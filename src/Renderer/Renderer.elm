port module Renderer.Renderer exposing (..)

import Browser.Dom
import Browser.Events
import ControlPanel exposing (ControlPanel)
import Dict exposing (keys)
import Element exposing (Color, Element)
import Html.Attributes
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Renderer.Orientation.Vertical as Vertical exposing (Vertical)
import Renderer.Player exposing (Player)
import Renderer.Shaders as Shaders
import Task
import WebGL exposing (Mesh)


port pointerLock : (Bool -> msg) -> Sub msg


type alias Size =
    { width : Int
    , height : Int
    }


type alias Keys =
    { forward : Bool
    , backward : Bool
    , left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , speedUp : Bool
    }


initKeys : Keys
initKeys =
    { forward = False
    , backward = False
    , left = False
    , right = False
    , up = False
    , down = False
    , speedUp = False
    }


updateKeys : Bool -> String -> Keys -> Keys
updateKeys pressed key keys =
    case key of
        "w" ->
            { keys | forward = pressed }

        "s" ->
            { keys | backward = pressed }

        "a" ->
            { keys | left = pressed }

        "d" ->
            { keys | right = pressed }

        " " ->
            { keys | up = pressed }

        "Shift" ->
            { keys | down = pressed }

        "CapsLock" ->
            if pressed then
                { keys | speedUp = not keys.speedUp }

            else
                keys

        _ ->
            keys


type alias Model =
    { canvasSize : Size
    , pressedKeys : Keys
    , player : Player Vertical
    , pointerLocked : Bool
    }


type Msg
    = GetElement (Result Browser.Dom.Error Browser.Dom.Element)
    | Refresh
    | Resize Int Int
    | Frame Float
    | KeyOp Bool String Bool
    | MouseMove Int Int
    | PointerLock Bool


id : String
id =
    "renderer"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { canvasSize = { width = 0, height = 0 }
      , pressedKeys = initKeys
      , player =
            Renderer.Player.init (vec3 0 0 0) <|
                Vertical.asOrientation
                    { phi = pi / 4
                    , theta = 0
                    }
      , pointerLocked = False
      }
    , Browser.Dom.getElement id
        |> Task.attempt GetElement
    )


mesh : Mesh Shaders.Attributes
mesh =
    WebGL.triangleFan
        [ Shaders.attributes -1 -1
        , Shaders.attributes -1 1
        , Shaders.attributes 1 1
        , Shaders.attributes 1 -1
        ]


colorToVec3 : Color -> Vec3
colorToVec3 color =
    let
        { red, green, blue } =
            Element.toRgb color
    in
    vec3 red green blue


view : ControlPanel -> Model -> Element Msg
view controlPanel model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute <| Html.Attributes.id id
        ]
    <|
        Element.html <|
            WebGL.toHtml
                [ Html.Attributes.id "canvas"
                , Html.Attributes.width model.canvasSize.width
                , Html.Attributes.height model.canvasSize.height
                ]
                [ WebGL.entity
                    Shaders.vertex
                    Shaders.fragment
                    mesh
                    { aspectRatio = toFloat model.canvasSize.width / toFloat model.canvasSize.height
                    , position = model.player.position
                    , view = model.player.view
                    , center = vec3 0 0 0
                    , size = 2
                    , materialColor = colorToVec3 controlPanel.materialColor
                    , shadowColor = colorToVec3 controlPanel.shadowColor
                    , backgroundColor = colorToVec3 controlPanel.backgroundColor
                    , fov = controlPanel.fov.value
                    , hitFactor = 10 ^ -controlPanel.detail.value
                    , glowLength = controlPanel.glowLength.value
                    }
                ]


sensitivity : Float
sensitivity =
    10 / 1920


boolToFloat : Bool -> Float
boolToFloat bool =
    if bool then
        1

    else
        0


calculateVelocity : Keys -> Vec3
calculateVelocity pressedKeys =
    let
        velocity =
            vec3
                (boolToFloat pressedKeys.right - boolToFloat pressedKeys.left)
                (boolToFloat pressedKeys.up - boolToFloat pressedKeys.down)
                (boolToFloat pressedKeys.forward - boolToFloat pressedKeys.backward)
    in
    if velocity == vec3 0 0 0 then
        velocity

    else
        Vec3.normalize velocity


setCanvasSize : ( Int, Int ) -> Model -> Model
setCanvasSize ( width, height ) model =
    { model | canvasSize = { width = width, height = height } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetElement (Ok { element }) ->
            ( setCanvasSize ( floor element.width, floor element.height ) model
            , Cmd.none
            )

        GetElement (Err _) ->
            ( model, Cmd.none )

        Refresh ->
            ( { model | canvasSize = { width = 0, height = 0 } }, Cmd.none )

        Resize _ _ ->
            ( model
            , Browser.Dom.getElement id
                |> Task.attempt GetElement
            )

        Frame deltaTimeMilliseconds ->
            let
                dt =
                    deltaTimeMilliseconds / 1000
            in
            ( { model
                | player = model.player |> Renderer.Player.handle dt
              }
            , Cmd.none
            )

        KeyOp pressed key False ->
            let
                pressedKeys =
                    updateKeys pressed key model.pressedKeys
            in
            ( { model
                | pressedKeys = pressedKeys
                , player = model.player |> Renderer.Player.updateVelocity (calculateVelocity pressedKeys)
              }
            , Cmd.none
            )

        KeyOp _ _ _ ->
            ( model, Cmd.none )

        MouseMove dx dy ->
            if model.pointerLocked then
                let
                    dphi =
                        sensitivity * toFloat dx

                    dtheta =
                        sensitivity * toFloat dy
                in
                ( { model
                    | player = Renderer.Player.rotate dphi dtheta model.player
                  }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        PointerLock lock ->
            ( { model | pointerLocked = lock }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onKeyDown <|
            Decode.map2 (KeyOp True)
                (Decode.field "key" Decode.string)
                (Decode.field "repeat" Decode.bool)
        , Browser.Events.onKeyUp <|
            Decode.map2 (KeyOp False)
                (Decode.field "key" Decode.string)
                (Decode.field "repeat" Decode.bool)
        , Browser.Events.onMouseMove <|
            Decode.map2 MouseMove
                (Decode.field "movementX" Decode.int)
                (Decode.field "movementY" Decode.int)
        , pointerLock PointerLock
        ]
