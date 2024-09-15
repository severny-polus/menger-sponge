module Renderer.Shaders exposing (..)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3)
import WebGL


type alias Attributes =
    { coord : Vec2
    }


attributes : Float -> Float -> Attributes
attributes x y =
    { coord = vec2 x y
    }


type alias Uniforms =
    { aspectRatio : Float
    , position : Vec3
    , view : Mat4
    , center : Vec3
    , size : Float
    }


type alias Varyings =
    { vcoord : Vec2
    }


vertex : WebGL.Shader Attributes Uniforms Varyings
vertex =
    [glsl|
    attribute vec2 coord;
    varying vec2 vcoord;
    void main() {
      gl_Position = vec4(coord, 0, 1);
      vcoord = coord;
    }
  |]


fragment : WebGL.Shader {} Uniforms Varyings
fragment =
    [glsl|
    precision highp float;
    uniform float aspectRatio;
    uniform vec3 position;
    uniform mat4 view;
    uniform vec3 center;
    uniform float size;
    varying vec2 vcoord;

    struct Sphere {
        vec3 center;
        float radius;
    };

    float sphereDistance(vec3 point, Sphere sphere) {
        return abs(distance(point, sphere.center) - sphere.radius);
    }

    struct Cube {
        vec3 center;
        float side;
    };

    float cubeDistance(vec3 point, Cube cube) {
        vec3 corner = abs(cube.center - point) - 0.5 * cube.side;
        if (corner.x < 0.0 && corner.y < 0.0 && corner.z < 0.0) {
            return min(abs(corner.x), min(abs(corner.y), abs(corner.z)));
        }
        return length(max(corner, 0.0));
    }

    float planeDistance(vec3 point, vec4 plane) {
        return max((dot(point, plane.xyz) + plane.w) / dot(plane.xyz, plane.xyz), 0.0);
    }

    vec3 mod3(float value, vec3 point) {
        return mod(point, value);
    }

    vec3 mirror(vec3 point, vec4 plane) {
        float relation = dot(position, plane.xyz) + plane.w;
        if (relation < 0.0) {
            return point - 2.0 * relation * plane.xyz / dot(plane.xyz, plane.xyz);
        }
        return point;
    }

    vec3 shift(vec3 point, vec4 plane, float by) {
        float relation = dot(position, plane.xyz) + plane.w;
        if (relation < 0.0) {
            return point + plane.xyz * by;
        }
        return point;
    }

    vec3 shrink(float value, vec3 origin, vec3 point) {
        return origin + value * (point - origin);
    }

    vec3 mirror3(vec3 origin, vec3 point) {
        return origin + abs(point - origin);
    }

    vec3 replicate3(vec3 origin, vec3 point) {
        vec3 p = point - origin;
        float m = min(p.x, min(p.y, p.z));
        if (m > 0.0) {
            return point;
        }
        if (p.x == m) {
            return origin + vec3(-p.x, p.y, p.z);
        }
        if (p.y == m) {
            return origin + vec3(p.x, -p.y, p.z);
        }
        if (p.z == m) {
            return origin + vec3(p.x, p.y, -p.z);
        }
        return point;
    }

    const int maxSteps = 50;
    const float minHitDistance = 0.0001;

    struct Material {
        vec3 color;
        vec4 glow;
        float glowDistance;
        float glowStrength;
    };

    vec3 objectColor(int step, Material material) {
        float a = material.glow.a * pow(float(step) / float(maxSteps), 1.0 - material.glowStrength);
        return material.glow.rgb * a + material.color * (1.0 - a);
    }

    vec3 backgroundColor(float minDistance, vec3 background, Material material) {
        float a = material.glow.a * exp(-minDistance / material.glowDistance);
        return material.glow.rgb * a + background * (1.0 - a);
    }

    vec3 background = vec3(0.0, 0.2, 0.5);

    Material minecraftSponge = Material(
        vec3(1, 0.7, 0.2),
        vec4(0.0, 0.2, 0.0, 1),
        0.1,
        0.0
    );

    const int iters = 11;

    float mengerSpongeDistance(vec3 point) {
        float shrinkFactor = 3.0;
        for (int i = 0; i < iters; i++) {
            point = mirror3(center, point);
            point = replicate3(center + vec3(size / 6.0), point);
            point = shrink(shrinkFactor, center + vec3(size / 2.0), point);
        }
        return cubeDistance(point, Cube(center, size)) / pow(shrinkFactor, float(iters));
    }

    vec3 march(vec3 direction, vec3 position, Material material) {
        float glowFactor = 2000000000.;
        float distanceWalked = 0.;
        for (int i = 0; i < maxSteps; i++) {
            float dist = mengerSpongeDistance(position);
            if (dist < minHitDistance * distanceWalked || dist < 0.000001) {
                return objectColor(i, material);
            }
            position = position + dist * direction;
            distanceWalked += dist;
            glowFactor = min(glowFactor, dist);
        }
        return backgroundColor(glowFactor, background, material);
    }

    const float fov = 120.;
    const float z = 1. / tan(radians(fov) / 2.);

    void main() {
        vec3 direction = mat3(view) * normalize(vec3(vcoord.x, vcoord.y / aspectRatio, z));
        vec3 color = march(direction, position, minecraftSponge);
        gl_FragColor = vec4(color, 1);
    }
    |]
