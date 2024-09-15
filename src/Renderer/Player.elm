module Renderer.Player exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Renderer.Orientation exposing (Orientation)


type alias Player a =
    { position : Vec3
    , relativeVelocity : Vec3
    , absoluteVelocity : Vec3
    , orientation : Orientation a
    , view : Mat4
    }


makeAbsoluteVelocity : Orientation a -> Vec3 -> Vec3
makeAbsoluteVelocity orientation relativeVelocity =
    relativeVelocity
        |> Mat4.transform (orientation |> Renderer.Orientation.basis)


init : Vec3 -> Orientation a -> Player a
init position orientation =
    { position = position
    , relativeVelocity = vec3 0 0 0
    , absoluteVelocity = makeAbsoluteVelocity orientation (vec3 0 0 0)
    , orientation = orientation
    , view = orientation |> Renderer.Orientation.view
    }


handle : Float -> Player a -> Player a
handle dt player =
    { player
        | position =
            player.position
                |> Vec3.add
                    (player.absoluteVelocity
                        |> Vec3.scale
                            (1
                                - ([ player.position |> Vec3.getX
                                   , player.position |> Vec3.getY
                                   , player.position |> Vec3.getZ
                                   ]
                                    |> List.map abs
                                    |> List.maximum
                                    |> Maybe.withDefault 0
                                  )
                            )
                        |> Vec3.scale dt
                    )
    }


velocityValue : Float
velocityValue =
    1


ctrlVelocityValue : Float
ctrlVelocityValue =
    3


updateVelocity : Vec3 -> Player a -> Player a
updateVelocity relativeVelocity player =
    { player
        | relativeVelocity = relativeVelocity
        , absoluteVelocity =
            makeAbsoluteVelocity player.orientation relativeVelocity
    }


rotate : Float -> Float -> Player a -> Player a
rotate dphi dtheta player =
    let
        orientation =
            Renderer.Orientation.rotate dphi dtheta player.orientation
    in
    { player
        | orientation = orientation
        , view = Renderer.Orientation.view orientation
        , absoluteVelocity = makeAbsoluteVelocity orientation player.relativeVelocity
    }
