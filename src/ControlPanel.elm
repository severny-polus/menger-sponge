module ControlPanel exposing (..)

import Element exposing (Color, Element, alignRight, alignTop, behindContent, centerY, column, el, fill, fillPortion, focused, height, image, mouseDown, mouseOver, padding, paddingXY, px, rgb, rgba, row, scale, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (Thumb, labelAbove, labelHidden)
import String exposing (fromFloat)


type alias ControlPanel =
    { materialColor : Color
    , shadowColor : Color
    , backgroundColor : Color
    , fov : Float
    , minHitDistance : Float
    , renderSteps : Int
    }


type Msg
    = Open Bool
    | SetMaterialColor Color
    | SetShadowColor Color
    | SetBackgroundColor Color
    | SetFov Float
    | SetDetail Float


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


slider : Float -> Float -> Float -> Maybe Float -> (Float -> msg) -> Element msg
slider min max value step msg =
    row
        [ width fill
        , spacing 8
        , Font.size 14
        ]
        [ -- Element.Input.text
          -- [ width <| px 48
          -- , focused [ Border.glow (rgba 0 0 0 0) 0 ]
          -- , padding 8
          -- , Background.color <| rgba 1 1 1 0.1
          -- , Border.color <| rgb 0.8 0.8 0.8
          -- , Border.width 2
          -- , Border.rounded 0
          -- ]
          -- { onChange = String.replace "." "" >> String.toFloat >> Maybe.withDefault value >> msg
          -- , text = fromFloat <| (toFloat <| round (value * 100)) / 100
          -- , placeholder = Just (Element.Input.placeholder [ Font.color <| rgb 0.5 0.5 0.5 ] <| text "Val")
          -- , label = labelHidden ""
          -- }
          text <| fromFloat min
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
                        [ width <| fillPortion <| round ((value - min) / (max - min) * 100)
                        , height fill
                        , Background.color <| rgb 0 0 0
                        ]
                        Element.none
                    , el
                        [ width <| fillPortion <| round ((max - value) / (max - min) * 100)
                        , height fill
                        , Background.color <| rgb 0.7 0.7 0.7
                        ]
                        Element.none
                    ]
            ]
            { onChange = msg
            , label = labelHidden ""
            , min = min
            , max = max
            , value = value
            , thumb =
                Element.Input.thumb
                    [ width <| px 14
                    , height <| px 14
                    , Background.color <| rgb 0 0 0
                    , Border.rounded 7
                    , Border.color <| rgba 0 0 0 0
                    , focused [ Border.glow (rgba 0 0 0 0) 0 ]
                    , mouseOver
                        [ scale 1.5
                        ]
                    ]
            , step = step
            }
        , text <| fromFloat max
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
                , slider 60 150 controlPanel.fov (Just 1) SetFov
                , controlLabel "Detail"
                , slider 1 7 -(logBase 10 controlPanel.minHitDistance) Nothing SetDetail
                ]
            ]
        ]


update : Msg -> ControlPanel -> ControlPanel
update msg control =
    case msg of
        SetMaterialColor color ->
            { control | materialColor = color }

        SetShadowColor color ->
            { control | shadowColor = color }

        SetBackgroundColor color ->
            { control | backgroundColor = color }

        SetFov fov ->
            { control | fov = fov }

        SetDetail detail ->
            { control | minHitDistance = 10 ^ -detail }

        _ ->
            control
