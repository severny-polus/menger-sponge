module Components.Slider exposing (Msg, Slider, init, slider, update)

import Element exposing (Element, behindContent, centerY, el, fill, fillPortion, focused, height, mouseOver, padding, paddingXY, px, rgb, rgba, row, scale, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (labelHidden)
import Html.Attributes exposing (value)
import Palette


type alias Slider =
    { value : Float, text : String }


type Msg
    = ChangeValue Float
    | ChangeText String


floatToText : Float -> String
floatToText value =
    String.fromFloat ((toFloat <| round (100 * value)) / 100)


init : Float -> Slider
init value =
    { value = value, text = floatToText value }


update : Msg -> Slider -> Slider
update msg model =
    case msg of
        ChangeValue value ->
            init value

        ChangeText text ->
            { value = String.toFloat text |> Maybe.withDefault model.value
            , text = text
            }


slider : Float -> Float -> Slider -> Maybe Float -> Element Msg
slider min max model step =
    row
        [ width fill
        , spacing 8
        , Font.size 14
        ]
        [ row
            [ width fill
            , spacing 4
            ]
            [ text <| String.fromFloat min
            , Element.Input.slider
                [ centerY
                , behindContent <|
                    row
                        [ width fill
                        , height <| px 2
                        , centerY
                        , paddingXY 7 0
                        ]
                        [ el
                            [ width <| fillPortion <| round ((model.value - min) / (max - min) * 100)
                            , height fill
                            , Background.color <| rgb 0 0 0
                            ]
                            Element.none
                        , el
                            [ width <| fillPortion <| round ((max - model.value) / (max - min) * 100)
                            , height fill
                            , Background.color <| rgb 0.7 0.7 0.7
                            ]
                            Element.none
                        ]
                ]
                { onChange = ChangeValue
                , label = labelHidden ""
                , min = min
                , max = max
                , value = model.value
                , thumb =
                    Element.Input.thumb
                        [ width <| px 14
                        , height <| px 14
                        , Background.color <| rgb 0 0 0
                        , Border.rounded 7
                        , Border.color <| rgba 0 0 0 0
                        , focused
                            [ Border.glow (rgba 0 0 0 0) 0

                            -- , Background.color Palette.focusColor
                            ]
                        , mouseOver
                            [ scale 1.5
                            ]
                        ]
                , step = step
                }
            , text <| String.fromFloat max
            ]
        , Element.Input.text
            [ width <| px 48
            , focused
                [ Border.color Palette.focusColor
                , Border.glow (rgba 0 0 0 0) 0
                ]
            , padding 8
            , Background.color <| rgba 1 1 1 0.1
            , Border.color <| rgb 0.8 0.8 0.8
            , Border.width 2
            , Border.rounded 0
            ]
            { onChange = ChangeText
            , text = model.text
            , placeholder = Just (Element.Input.placeholder [ Font.color <| rgb 0.5 0.5 0.5 ] <| text "Value")
            , label = labelHidden ""
            }
        ]
