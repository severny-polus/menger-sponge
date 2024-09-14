module World exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


right : Vec3
right =
    vec3 1 0 0


up : Vec3
up =
    vec3 0 1 0


forward : Vec3
forward =
    vec3 0 0 1
