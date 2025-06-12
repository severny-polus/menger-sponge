module Main exposing (main)

import Browser
import Browser.Dom
import Components.ControlPanel as ControlPanel exposing (ControlPanel)
import Element exposing (alignRight, el, fill, height, htmlAttribute, inFront, layout, rgba, row, width)
import Fractal.Renderer as Renderer
import Html exposing (Html)
import Html.Attributes
import Task


type Msg
    = RendererMsg Renderer.Msg
    | ControlPanelMsg ControlPanel.Msg


type alias Model =
    { renderer : Renderer.Model
    , controlPanelOpened : Bool
    , controlPanel : ControlPanel
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( renderer, rendererCmd ) =
            Renderer.init ()
    in
    ( { renderer = renderer
      , controlPanelOpened = False
      , controlPanel =
            ControlPanel.init
      }
    , Cmd.batch
        [ rendererCmd |> Cmd.map RendererMsg
        ]
    )


view : Model -> Html Msg
view model =
    layout
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.id "root"
        ]
    <|
        row
            [ width fill
            , height fill
            ]
            [ el
                [ width fill
                , height fill
                , inFront <|
                    if model.controlPanelOpened then
                        Element.none

                    else
                        el [ alignRight ] <|
                            ControlPanel.button48px
                                "resources/settings--adjust-white.svg"
                                (rgba 1 1 1 0)
                                (rgba 1 1 1 0.05)
                                (rgba 1 1 1 0.1)
                            <|
                                ControlPanelMsg <|
                                    ControlPanel.Open True
                ]
              <|
                Element.map RendererMsg <|
                    Renderer.view model.controlPanel model.renderer
            , if model.controlPanelOpened then
                Element.map ControlPanelMsg <|
                    ControlPanel.view model.controlPanel

              else
                Element.none
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RendererMsg rendererMsg ->
            let
                ( renderer, cmd ) =
                    model.renderer |> Renderer.update rendererMsg
            in
            ( { model | renderer = renderer }
            , cmd |> Cmd.map RendererMsg
            )

        ControlPanelMsg (ControlPanel.Open bool) ->
            ( { model
                | controlPanelOpened = bool
                , renderer =
                    Renderer.setCanvasSize
                        ( model.renderer.canvasSize.width
                            + (if bool then
                                -256

                               else
                                256
                              )
                        , model.renderer.canvasSize.height
                        )
                        model.renderer
              }
            , Browser.Dom.getElement Renderer.id
                |> Task.attempt Renderer.GetElement
                |> Cmd.map RendererMsg
            )

        ControlPanelMsg controlPanelMsg ->
            ( { model | controlPanel = model.controlPanel |> ControlPanel.update controlPanelMsg }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model.renderer |> Renderer.subscriptions |> Sub.map RendererMsg
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
