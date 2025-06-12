module Components.Toggle exposing (..)

import Element exposing (Element, behindContent, el, focused, height, none, padding, paddingXY, px, rgb, rgba, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Palette


toggleHeight : Int
toggleHeight =
    16


toggle : Bool -> (Bool -> msg) -> Element msg
toggle state msg =
    Input.button
        [ Background.color <|
            if state then
                Palette.colorOn

            else
                Palette.colorOff
        , height <| px toggleHeight
        , width <| px <| 2 * toggleHeight
        , Border.rounded <| toggleHeight // 2
        , behindContent <|
            row
                [ paddingXY
                    (3
                        + (if state then
                            toggleHeight

                           else
                            0
                          )
                    )
                    3
                ]
                [ el
                    [ width <| px <| toggleHeight - 6
                    , height <| px <| toggleHeight - 6
                    , Border.rounded <| (toggleHeight - 6) // 2
                    , Background.color <| rgb 1 1 1
                    ]
                    none
                ]
        , focused
            [ Border.color Palette.colorFocus
            , Border.glow (rgba 0 0 0 0) 0
            ]
        ]
        { onPress = Just <| msg <| not state
        , label = none
        }
