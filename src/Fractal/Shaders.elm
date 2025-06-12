module Fractal.Shaders exposing (..)

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
    , materialColor : Vec3
    , shadowColor : Vec3
    , backgroundColor : Vec3
    , fov : Float
    , hitFactor : Float
    , glowLength : Float
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
    
    varying vec2 vcoord;

    uniform float aspectRatio;
    uniform vec3 position;
    uniform mat4 view;
    uniform vec3 center;
    uniform float size;

    uniform vec3 materialColor;
    uniform vec3 shadowColor;
    uniform vec3 backgroundColor;

    uniform float fov;
    uniform float hitFactor;
    uniform float glowLength;

    const int numIterations = 11;
    const int renderSteps = 50;

    const float minHitDistance = 0.000001;

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

    struct Material {
        vec3 color;
        vec4 glowColor;
        float glowLength;
        float glowStrength;
    };

    vec3 getObjectColor(int currentStep, Material material) {
        float a = float(currentStep) / float(renderSteps);
        return material.color * (1.0 - a) + shadowColor * a;
    }

    vec3 getBackgroundColor(float glowDistance, Material material) {
        float a = material.glowColor.a * exp(-glowDistance / material.glowLength);
        return backgroundColor * (1.0 - a) + material.glowColor.rgb * a;
    }

    float mengerSpongeDistance(vec3 point) {
        float shrinkFactor = 3.0;
        for (int i = 0; i < numIterations; i++) {
            point = mirror3(center, point);
            point = replicate3(center + vec3(size / 6.0), point);
            point = shrink(shrinkFactor, center + vec3(size / 2.0), point);
        }
        return cubeDistance(point, Cube(center, size)) / pow(shrinkFactor, float(numIterations));
    }

    vec3 march(vec3 direction, vec3 position, Material material) {
        float glowDistance = 2000000000.;
        float distanceWalked = 0.;
        for (int i = 0; i < renderSteps; i++) {
            float dist = mengerSpongeDistance(position);
            if (dist < hitFactor * distanceWalked || dist < minHitDistance) {
                return getObjectColor(i, material);
            }
            position = position + dist * direction;
            distanceWalked += dist;
            glowDistance = min(glowDistance, dist);
        }
        return getBackgroundColor(glowDistance, material);
    }

    void main() {
        Material material = Material(
            materialColor,
            vec4(shadowColor, 1),
            glowLength,
            0.0
        );
        float z = 1. / tan(radians(fov) / 2.);
        vec3 direction = mat3(view) * normalize(vec3(vcoord.x, vcoord.y / aspectRatio, z));
        vec3 color = march(direction, position, material);
        gl_FragColor = vec4(color, 1);
    }
    |]
