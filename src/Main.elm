module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (keys)
import Element
import Html exposing (Html)
import Html.Attributes exposing (height, id, width)
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Orientation.Vertical as Vertical exposing (Vertical)
import Player exposing (Player)
import Shaders
import Task
import WebGL exposing (Mesh)


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
    }


type Msg
    = Element (Result Browser.Dom.Error Browser.Dom.Element)
    | Resize Int Int
    | Frame Float
    | KeyOp Bool String Bool
    | MouseMove Int Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { canvasSize = { width = 0, height = 0 }
      , pressedKeys = initKeys
      , player =
            Player.init (vec3 0 0 0) <|
                Vertical.asOrientation
                    { phi = pi / 4
                    , theta = 0
                    }
      }
    , Browser.Dom.getElement root
        |> Task.attempt Element
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
        ]


sensitivity : Float
sensitivity =
    4 / 1920


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Element (Ok { element }) ->
            ( { model
                | canvasSize =
                    { width = floor element.width
                    , height = floor element.height
                    }
              }
            , Cmd.none
            )

        Element (Err _) ->
            ( model, Cmd.none )

        Resize _ _ ->
            ( model
            , Browser.Dom.getElement root
                |> Task.attempt Element
            )

        Frame deltaTimeMilliseconds ->
            let
                dt =
                    deltaTimeMilliseconds / 1000
            in
            ( { model
                | player = model.player |> Player.handle dt
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
                , player = model.player |> Player.updateVelocity (calculateVelocity pressedKeys)
              }
            , Cmd.none
            )

        KeyOp _ _ _ ->
            ( model, Cmd.none )

        MouseMove dx dy ->
            let
                dphi =
                    sensitivity * toFloat dx

                dtheta =
                    sensitivity * toFloat dy
            in
            ( { model
                | player = Player.rotate dphi dtheta model.player
              }
            , Cmd.none
            )


root : String
root =
    "root"


mesh : Mesh Shaders.Attributes
mesh =
    WebGL.triangleFan
        [ Shaders.attributes -1 -1
        , Shaders.attributes -1 1
        , Shaders.attributes 1 1
        , Shaders.attributes 1 -1
        ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute <| id root
        ]
    <|
        Element.html <|
            WebGL.toHtml
                [ id "canvas"
                , width model.canvasSize.width
                , height model.canvasSize.height
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
                    }
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
