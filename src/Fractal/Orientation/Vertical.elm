module Fractal.Orientation.Vertical exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Fractal.Orientation exposing (Orientation)
import Fractal.World


type alias Vertical =
    { phi : Float
    , theta : Float
    }


basis : Vertical -> Mat4
basis vertical =
    Mat4.makeRotate vertical.phi Fractal.World.up


view : Vertical -> Mat4
view vertical =
    Mat4.mul
        (Mat4.makeRotate vertical.phi Fractal.World.up)
        (Mat4.makeRotate vertical.theta Fractal.World.right)


rotate : Float -> Float -> Vertical -> Vertical
rotate dphi dtheta vertical =
    let
        phi =
            vertical.phi + dphi

        theta =
            vertical.theta + dtheta
    in
    { phi =
        if phi > 2 * pi then
            phi - 2 * pi

        else if phi < 0 then
            phi + 2 * pi

        else
            phi
    , theta = max (-pi / 2) <| min theta (pi / 2)
    }


asOrientation : Vertical -> Orientation Vertical
asOrientation vertical =
    { impl = vertical
    , basis = basis
    , view = view
    , rotate = rotate
    }
