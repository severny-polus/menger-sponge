module Fractal exposing (..)

import Element exposing (Color)


type alias Fractal =
    { materialColor : Color
    , shadowColor : Color
    , backgroundColor : Color
    }


type Msg
    = SetMaterialColor Color
    | SetShadowColor Color
    | SetBackgroundColor Color


update : Msg -> Fractal -> Fractal
update msg uniforms =
    case msg of
        SetMaterialColor color ->
            { uniforms | materialColor = color }

        SetShadowColor color ->
            { uniforms | shadowColor = color }

        SetBackgroundColor color ->
            { uniforms | backgroundColor = color }
