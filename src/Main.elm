module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation
import ColorPicker exposing (colorPicker)
import Element exposing (Color, Element, alignRight, alignTop, column, el, fill, focused, height, htmlAttribute, image, inFront, layout, map, mouseDown, mouseOver, noHover, padding, px, rgb, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Fractal exposing (Fractal)
import Html exposing (Html)
import Html.Attributes
import Renderer.Renderer as Renderer
import Task


type Msg
    = RendererMsg Renderer.Msg
    | FractalMsg Fractal.Msg
    | OpenControlPanel Bool


type alias Model =
    { renderer : Renderer.Model
    , controlPanelOpened : Bool
    , fractal : Fractal
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( renderer, rendererCmd ) =
            Renderer.init ()
    in
    ( { renderer = renderer
      , controlPanelOpened = False
      , fractal =
            { materialColor = rgb 1 0.7 0.2
            , shadowColor = rgb 0 0.2 0
            , backgroundColor = rgb 0 0.2 0.5
            }
      }
    , Cmd.batch
        [ rendererCmd |> Cmd.map RendererMsg
        ]
    )


button48px : String -> Color -> Color -> Color -> Msg -> Element Msg
button48px icon bg hoverBg activeBg msg =
    Input.button
        [ width <| px 48
        , height <| px 48
        , padding 12
        , Background.color bg
        , mouseDown
            [ Background.color activeBg
            ]
        , mouseOver
            [ Background.color hoverBg
            ]
        , focused []
        ]
        { onPress = Just msg
        , label =
            image
                [ width <| px 24
                , height <| px 24
                ]
                { src = icon
                , description = ""
                }
        }


h3 : String -> Element Msg
h3 string =
    el
        [ Font.size 16
        , Font.bold
        ]
    <|
        text string


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
                            button48px
                                "svg/settings--adjust-white.svg"
                                (rgba 1 1 1 0)
                                (rgba 1 1 1 0.1)
                                (rgba 1 1 1 0.2)
                            <|
                                OpenControlPanel True
                ]
              <|
                Element.map RendererMsg <|
                    Renderer.view model.fractal model.renderer
            , if model.controlPanelOpened then
                column
                    [ width <| px 256
                    , alignTop
                    ]
                    [ column
                        [ width fill
                        ]
                        [ row [ alignRight ]
                            [ button48px
                                "svg/chevron--right.svg"
                                (rgb 1 1 1)
                                (rgba 0 0 0 0.05)
                                (rgba 0 0 0 0.1)
                              <|
                                OpenControlPanel False
                            ]
                        , column
                            [ width fill
                            , padding 16
                            , spacing 16
                            ]
                            [ h3 "Material color"
                            , colorPicker model.fractal.materialColor Fractal.SetMaterialColor |> Element.map FractalMsg
                            , h3 "Shadow color"
                            , colorPicker model.fractal.shadowColor Fractal.SetShadowColor |> Element.map FractalMsg
                            , h3 "Background color"
                            , colorPicker model.fractal.backgroundColor Fractal.SetBackgroundColor |> Element.map FractalMsg
                            ]
                        ]
                    ]

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

        FractalMsg fractalMsg ->
            ( { model | fractal = model.fractal |> Fractal.update fractalMsg }
            , Cmd.none
            )

        OpenControlPanel bool ->
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model.renderer |> Renderer.subscriptions |> Sub.map RendererMsg
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
