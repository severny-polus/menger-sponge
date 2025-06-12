module Components.ColorPicker exposing (..)

import Element exposing (Color, Element, Length, column, fill, focused, height, px, rgb, rgba, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (Thumb, labelAbove, labelHidden)


colorPickerHeight : Length
colorPickerHeight =
    px 24


colorPickerThumb : Color -> Thumb
colorPickerThumb color =
    Element.Input.thumb
        [ width <| px 2
        , height colorPickerHeight
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


negate : Color -> Color
negate color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgb (1 - red) (1 - green) (1 - blue)


slider : Maybe String -> Float -> (Float -> Color) -> Element Color
slider label value color =
    Element.Input.slider
        [ height colorPickerHeight
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
            colorPickerThumb <| negate <| color value
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
            [ slider Nothing red <| \r -> Element.rgb r green blue
            , slider Nothing green <| \g -> Element.rgb red g blue
            , slider Nothing blue <| \b -> Element.rgb red green b
            ]
