module Main exposing (main)

import Browser
import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as Attributes
import Renderer.Renderer as Renderer


type alias Model =
    { renderer : Renderer.Model
    }


type Msg
    = Renderer Renderer.Msg
    | PointerLock


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( renderer, rendererCmd ) =
            Renderer.init ()
    in
    ( { renderer = renderer
      }
    , rendererCmd |> Cmd.map Renderer
    )


view : Renderer.Model -> Html Renderer.Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute <| Attributes.id "root"
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Renderer.view model
            , Element.column
                [ Element.width <| Element.px 0
                ]
                []
            ]


main : Program () Renderer.Model Renderer.Msg
main =
    Browser.element
        { init = Renderer.init
        , update = Renderer.update
        , view = view
        , subscriptions = Renderer.subscriptions
        }
