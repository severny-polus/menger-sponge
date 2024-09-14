module Orientation exposing (..)

import Math.Matrix4 exposing (Mat4)


type alias Orientation a =
    { impl : a
    , basis : a -> Mat4
    , view : a -> Mat4
    , rotate : Float -> Float -> a -> a
    }


basis : Orientation a -> Mat4
basis orientation =
    orientation.basis orientation.impl


view : Orientation a -> Mat4
view orientation =
    orientation.view orientation.impl


rotate : Float -> Float -> Orientation a -> Orientation a
rotate dphi dtheta orientation =
    { orientation | impl = orientation.rotate dphi dtheta orientation.impl }
