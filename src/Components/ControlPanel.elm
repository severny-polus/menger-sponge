module Components.ControlPanel exposing (..)

import Components.ColorPicker as ColorPicker
import Components.Slider as Slider exposing (Slider)
import Components.Toggle exposing (toggle)
import Dict exposing (Dict)
import Element exposing (Color, Element, alignRight, alignTop, column, el, fill, focused, height, image, mouseDown, mouseOver, padding, px, rgb, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input


type alias ColorToken =
    Int


materialColor : ColorToken
materialColor =
    0


backgroundColor : ColorToken
backgroundColor =
    1


shadowColor : ColorToken
shadowColor =
    2


glowColor : ColorToken
glowColor =
    3


type alias ControlPanel =
    { colors : Dict ColorToken Color
    , bindGlowColor : Bool
    , fov : Slider
    , detail : Slider
    , glowLength : Slider
    }


type Msg
    = Open Bool
    | SetColor ColorToken Color
    | BindGlowColor Bool
    | SetFov Slider.Msg
    | SetDetail Slider.Msg
    | SetGlowLength Slider.Msg


init : ControlPanel
init =
    { colors =
        Dict.fromList
            [ ( materialColor, rgb 1 0.7 0.2 )
            , ( backgroundColor, rgb 0 0.2 0.5 )
            , ( shadowColor, rgb 0 0.2 0 )
            , ( glowColor, rgb 0 0.2 0 )
            ]
    , bindGlowColor = False
    , fov = Slider.init 120
    , detail = Slider.init <| -(logBase 10 0.0002)
    , glowLength = Slider.init 0.1
    }


button48px : String -> Color -> Color -> Color -> msg -> Element msg
button48px icon bg hoverBg activeBg msg =
    Element.Input.button
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


controlLabel : String -> Element Msg
controlLabel string =
    el
        [ Font.size 14
        , Font.color <| rgb 0.3 0.3 0.3
        ]
    <|
        text string


labeled : String -> Element Msg -> Element Msg
labeled label element =
    column
        [ width fill
        , spacing 8
        ]
        [ controlLabel label
        , element
        ]


getColor : ColorToken -> ControlPanel -> Color
getColor token controlPanel =
    Maybe.withDefault (rgb 0 0 0) <| Dict.get token controlPanel.colors


colorPicker : ColorToken -> ControlPanel -> Element Msg
colorPicker token controlPanel =
    ColorPicker.colorPicker (getColor token controlPanel) <| SetColor token


view : ControlPanel -> Element Msg
view controlPanel =
    column
        [ width <| px 256
        , alignTop
        ]
        [ column
            [ width fill
            ]
            [ row [ alignRight ]
                [ button48px
                    "resources/close.svg"
                    (rgba 0 0 0 0)
                    (rgba 0 0 0 0.05)
                    (rgba 0 0 0 0.1)
                  <|
                    Open False
                ]
            , column
                [ width fill
                , padding 16
                , spacing 16
                ]
                [ labeled "Material color" <| colorPicker materialColor controlPanel
                , labeled "Background color" <| colorPicker backgroundColor controlPanel
                , labeled "Shadow color" <| colorPicker shadowColor controlPanel
                , labeled "Glow color" <| colorPicker glowColor controlPanel
                -- , labeled "Bind glow color" <| toggle controlPanel.bindGlowColor BindGlowColor
                , labeled "Field of view" <| Element.map SetFov <| Slider.slider 60 150 controlPanel.fov (Just 1)
                , labeled "Detail" <| Element.map SetDetail <| Slider.slider 1 6 controlPanel.detail Nothing
                , labeled "Glow length" <| Element.map SetGlowLength <| Slider.slider 0 1 controlPanel.glowLength Nothing
                ]
            ]
        ]


update : Msg -> ControlPanel -> ControlPanel
update msg controlPanel =
    case msg of
        Open _ ->
            controlPanel

        SetColor token color ->
            let
                colors =
                    Dict.insert token color controlPanel.colors
            in
            { controlPanel
                | colors =
                    if controlPanel.bindGlowColor then
                        case token of
                            2 ->
                                Dict.update glowColor (\_ -> Dict.get shadowColor colors) colors

                            3 ->
                                Dict.update shadowColor (\_ -> Dict.get glowColor colors) colors

                            _ ->
                                colors

                    else
                        colors
            }

        BindGlowColor flag ->
            { controlPanel
                | bindGlowColor = flag
                , colors =
                    if flag then
                        Dict.update glowColor (\_ -> Dict.get shadowColor controlPanel.colors) controlPanel.colors

                    else
                        controlPanel.colors
            }

        SetFov fov ->
            { controlPanel | fov = Slider.update fov controlPanel.fov }

        SetDetail detail ->
            { controlPanel | detail = Slider.update detail controlPanel.detail }

        SetGlowLength glowLength ->
            { controlPanel | glowLength = Slider.update glowLength controlPanel.glowLength }
