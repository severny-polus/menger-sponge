module ControlPanel exposing (..)

import Element exposing (Color, Element, alignRight, alignTop, column, el, fill, focused, height, image, mouseDown, mouseOver, padding, px, rgb, rgba, row, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (Thumb, labelAbove, labelHidden)
import Slider exposing (Slider)


type alias ControlPanel =
    { materialColor : Color
    , shadowColor : Color
    , backgroundColor : Color
    , fov : Slider
    , detail : Slider
    , glowLength : Slider
    }


type Msg
    = Open Bool
    | SetMaterialColor Color
    | SetShadowColor Color
    | SetBackgroundColor Color
    | SetFov Slider.Msg
    | SetDetail Slider.Msg
    | SetGlowLength Slider.Msg


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


colorPickerThumb : Color -> Thumb
colorPickerThumb color =
    Element.Input.thumb
        [ width <| px 2
        , height <| px 32
        , Background.color color
        , focused
            [ Border.shadow
                { offset = ( 0, 0 )
                , size = 0
                , blur = 0
                , color = rgba 0 0 0 0
                }
            ]
        ]


negative : Color -> Color
negative color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgb (1 - red) (1 - green) (1 - blue)


colorPickerSlider : Maybe String -> Float -> (Float -> Color) -> Element Color
colorPickerSlider label value color =
    Element.Input.slider
        [ height <| px 32
        , Background.gradient
            { angle = pi / 2
            , steps =
                [ color 0
                , color 1
                ]
            }
        ]
        { onChange = color
        , label =
            Maybe.withDefault (labelHidden "") <|
                Maybe.map
                    (labelAbove
                        [ Font.size 12
                        , Font.color <| rgb 0.5 0.5 0.5
                        ]
                        << text
                    )
                    label
        , min = 0
        , max = 1
        , value = value
        , thumb =
            colorPickerThumb <| negative <| color value
        , step = Nothing
        }


colorPicker : Color -> (Color -> msg) -> Element msg
colorPicker color msg =
    let
        { red, green, blue } =
            toRgb color
    in
    column
        [ width fill
        , spacing 4
        ]
    <|
        List.map
            (Element.map msg)
            [ colorPickerSlider Nothing red <| \r -> Element.rgb r green blue
            , colorPickerSlider Nothing green <| \g -> Element.rgb red g blue
            , colorPickerSlider Nothing blue <| \b -> Element.rgb red green b
            ]


controlLabel : String -> Element Msg
controlLabel string =
    el
        [ Font.size 14
        , Font.color <| rgb 0.3 0.3 0.3
        ]
    <|
        text string


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
                    "svg/chevron--right.svg"
                    (rgb 1 1 1)
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
                [ controlLabel "Material color"
                , colorPicker controlPanel.materialColor SetMaterialColor
                , controlLabel "Shadow color"
                , colorPicker controlPanel.shadowColor SetShadowColor
                , controlLabel "Background color"
                , colorPicker controlPanel.backgroundColor SetBackgroundColor
                , controlLabel "Field of view"
                , Slider.slider 60 150 controlPanel.fov (Just 1) |> Element.map SetFov
                , controlLabel "Detail"
                , Slider.slider 1 6 controlPanel.detail Nothing |> Element.map SetDetail
                , controlLabel "Glow length"
                , Slider.slider 0 1 controlPanel.glowLength Nothing |> Element.map SetGlowLength
                ]
            ]
        ]


update : Msg -> ControlPanel -> ControlPanel
update msg controlPanel =
    case msg of
        Open _ ->
            controlPanel

        SetMaterialColor color ->
            { controlPanel | materialColor = color }

        SetShadowColor color ->
            { controlPanel | shadowColor = color }

        SetBackgroundColor color ->
            { controlPanel | backgroundColor = color }

        SetFov fov ->
            { controlPanel | fov = Slider.update fov controlPanel.fov }

        SetDetail detail ->
            { controlPanel | detail = Slider.update detail controlPanel.detail }

        SetGlowLength glowLength ->
            { controlPanel | glowLength = Slider.update glowLength controlPanel.glowLength }
