module ColorPicker exposing (colorPicker)

import Element exposing (Color, Element, column, fill, focused, height, padding, paddingEach, px, rgb, rgba, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (Thumb, labelAbove, slider, thumb)


thumb : Color -> Thumb
thumb color =
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


slider : String -> Float -> (Float -> Color) -> Element Color
slider label value color =
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
            labelAbove
                [ Font.size 12
                , Font.color <| rgb 0.5 0.5 0.5
                ]
            <|
                text label
        , min = 0
        , max = 1
        , value = value
        , thumb =
            thumb <| negative <| color value
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
            [ slider "Red" red <| \r -> Element.rgb r green blue
            , slider "Green" green <| \g -> Element.rgb red g blue
            , slider "Blue" blue <| \b -> Element.rgb red green b
            ]
