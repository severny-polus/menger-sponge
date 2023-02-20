module Main exposing (main)


import Browser
import Browser.Dom
import Browser.Events
import Element
import Html exposing (Html)
import Html.Attributes exposing (height, id, width)
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import Shaders
import WebGL exposing (Mesh)


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


type alias Model =
  { size : Size
  , pressedKeys : PressedKeys
  , position : Vec3
  , velocity : Vec3
  , phi : Float
  , theta : Float
  , worldForward : Vec3
  , worldRight : Vec3
  , worldUp : Vec3
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { size = { width = 0, height = 0 }
    , pressedKeys = initKeys
    , position = vec3 -6 -12 -30
    , velocity = vec3 0 0 0
    , phi = 0
    , theta = 0
    , worldRight = vec3 1 0 0
    , worldUp = vec3 0 1 0
    , worldForward = vec3 0 0 1
    }
  , Browser.Dom.getElement root
    |> Task.attempt Element
  )


type Msg
  = Element (Result Browser.Dom.Error Browser.Dom.Element)
  | Resize Int Int
  | Frame Float
  | KeyOp Bool String Bool
  | MouseMove Int Int


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Browser.Events.onResize Resize
    , Browser.Events.onAnimationFrameDelta Frame
    , Browser.Events.onKeyDown
      <| Decode.map2 (KeyOp True)
        (Decode.field "key" Decode.string)
        (Decode.field "repeat" Decode.bool)
    , Browser.Events.onKeyUp
      <| Decode.map2 (KeyOp False)
        (Decode.field "key" Decode.string)
        (Decode.field "repeat" Decode.bool)
    , Browser.Events.onMouseMove
      <| Decode.map2 MouseMove
        (Decode.field "movementX" Decode.int)
        (Decode.field "movementY" Decode.int)
    ]


maxVelocity : Float
maxVelocity =
  3


sensitivity : Float
sensitivity =
  pi * 0.0005


camera : Model -> Mat4
camera model =
  Mat4.mul
    (Mat4.makeRotate model.phi model.worldUp)
    (Mat4.makeRotate model.theta model.worldRight)


updateVelocity : Model -> Model
updateVelocity model =
  let
    rotate =
      Mat4.makeRotate model.phi model.worldUp
  in
  { model
  | velocity =
    Vec3.scale maxVelocity
      <| velocityDirection
        (Mat4.transform rotate model.worldRight)
        model.worldUp
        (Mat4.transform rotate model.worldForward)
        model.pressedKeys
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Element (Ok { element }) ->
      ( { model
        | size =
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

    Frame dt ->
      ( if model.velocity == vec3 0 0 0 then
          model
        else
          { model
          | position =
            Vec3.add
              model.position
              (Vec3.scale (dt / 1000) model.velocity)
          }
      , Cmd.none
      )

    KeyOp pressed key False ->
      ( { model | pressedKeys = updateKeys pressed key model.pressedKeys }
        |> updateVelocity
      , Cmd.none
      )

    KeyOp _ _ True ->
      ( model, Cmd.none )

    MouseMove dx dy ->
      ( { model
        | phi = model.phi + sensitivity * toFloat dx
        , theta = max (-pi / 2) <| min (pi / 2) <| model.theta + sensitivity * toFloat dy
        } |> updateVelocity
      , Cmd.none
      )


root : String
root =
  "root"


view : Model -> Html Msg
view model =
  Element.layout
    [ Element.width Element.fill
    , Element.height Element.fill
    , Element.htmlAttribute <| id root
    ]
    <| Element.html
    <| WebGL.toHtml
      [ id "canvas"
      , width model.size.width
      , height model.size.height
      ]
      [ WebGL.entity
        Shaders.vertex
        Shaders.fragment
        mesh
        { ratio = toFloat model.size.width / toFloat model.size.height
        , position = model.position
        , camera = camera model
        , center = vec3 0 0 0
        , size = 81
        }
      ]


type alias Size =
  { width : Int
  , height : Int
  }


mesh : Mesh Shaders.Attributes
mesh =
  WebGL.triangles
    [ ( Shaders.attributes -1 -1
      , Shaders.attributes -1 1
      , Shaders.attributes 1 1
      )
    , ( Shaders.attributes -1 -1
      , Shaders.attributes 1 1
      , Shaders.attributes 1 -1
      )
    ]


type alias PressedKeys =
  { forward : Bool
  , backward : Bool
  , left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  }


initKeys : PressedKeys
initKeys =
  { forward = False
  , backward = False
  , left = False
  , right = False
  , up = False
  , down = False
  }


updateKeys : Bool -> String -> PressedKeys -> PressedKeys
updateKeys pressed key keys =
  case key of
    "w" -> { keys | forward = pressed }
    "s" -> { keys | backward = pressed }
    "a" -> { keys | left = pressed }
    "d" -> { keys | right = pressed }
    " " -> { keys | up = pressed }
    "Shift" -> { keys | down = pressed }
    _ -> keys


velocityDirection : Vec3 -> Vec3 -> Vec3 -> PressedKeys -> Vec3
velocityDirection right up forward keys =
  let
    direction =
      List.foldl Vec3.add (vec3 0 0 0)
        [ vecOrZero keys.forward forward
        , vecOrZero keys.backward <| Vec3.negate forward
        , vecOrZero keys.left <| Vec3.negate right
        , vecOrZero keys.right right
        ]
  in
  if direction == vec3 0 0 0 then
    direction
  else
    Vec3.normalize direction


vecOrZero : Bool -> Vec3 -> Vec3
vecOrZero bool vec =
  if bool then
    vec
  else
    vec3 0 0 0