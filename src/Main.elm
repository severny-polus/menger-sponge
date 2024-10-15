module Main exposing (main)

import Browser
import Browser.Dom
import ControlPanel exposing (ControlPanel)
import Element exposing (alignRight, el, fill, height, htmlAttribute, inFront, layout, rgb, rgba, row, width)
import Html exposing (Html)
import Html.Attributes
import Renderer.Renderer as Renderer
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
            { materialColor = rgb 1 0.7 0.2
            , shadowColor = rgb 0 0.2 0
            , backgroundColor = rgb 0 0.2 0.5
            , fov = 120
            , minHitDistance = 0.0002
            , renderSteps = 50
            }
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
                                "svg/settings--adjust-white.svg"
                                (rgba 1 1 1 0)
                                (rgba 1 1 1 0.1)
                                (rgba 1 1 1 0.2)
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
            let
                renderer =
                    model.renderer
            in
            ( { model
                | controlPanelOpened = bool
                , renderer =
                    { renderer
                        | canvasSize =
                            { width =
                                renderer.canvasSize.width
                                    + (if bool then
                                        -256

                                       else
                                        256
                                      )
                            , height = renderer.canvasSize.height
                            }
                    }
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
