module Shaders exposing (..)


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
  { ratio : Float
  , position : Vec3
  , camera : Mat4
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
    precision mediump float;
    uniform float ratio;
    uniform vec3 position;
    uniform mat4 camera;
    uniform vec3 center;
    uniform float size;
    varying vec2 vcoord;

    struct Sphere {
      vec3 center;
      float radius;
    };

    float SphereDistance(vec3 point, Sphere sphere) {
      return abs(distance(point, sphere.center) - sphere.radius);
    }

    struct Cube {
      vec3 center;
      float side;
    };

    float CubeDistance(vec3 point, Cube cube) {
      vec3 corner = abs(cube.center - point) - 0.5 * cube.side;
      if (corner.x < 0.0 && corner.y < 0.0 && corner.z < 0.0) {
        return min(abs(corner.x), min(abs(corner.y), abs(corner.z)));
      }
      return length(max(corner, 0.0));
    }

    float PlaneDistance(vec3 point, vec4 plane) {
      return max((dot(point, plane.xyz) + plane.w) / dot(plane.xyz, plane.xyz), 0.0);
    }

    vec3 Mod(float value, vec3 point) {
      return mod(point, value);
    }

    vec3 Reflect(vec3 point, vec4 plane) {
      float relation = dot(position, plane.xyz) + plane.w;
      if (relation < 0.0) {
        return point - 2.0 * relation * plane.xyz / dot(plane.xyz, plane.xyz);
      }
      return point;
    }

    vec3 Shift(vec3 point, vec4 plane, float shift) {
      float relation = dot(position, plane.xyz) + plane.w;
      if (relation < 0.0) {
        return point + plane.xyz * shift;
      }
      return point;
    }

    vec3 Shrink(float value, vec3 origin, vec3 point) {
      return origin + value * (point - origin);
    }

    vec3 Triflect(vec3 origin, vec3 point) {
      return origin + abs(point - origin);
    }

    vec3 Triplicate(vec3 origin, vec3 point) {
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
    const float hitDistance = 0.001;

    struct Material {
      vec3 color;
      vec4 glow;
      float glowLength;
      float glowStrength;
    };

    vec3 Shape(int step, Material material) {
      float a = material.glow.a * pow(float(step) / float(maxSteps), 1.0 - material.glowStrength);
      return material.glow.rgb * a + material.color * (1.0 - a);
    }

    vec3 Glow(float minDistance, vec3 background, Material material) {
      float a = material.glow.a * exp(-minDistance / material.glowLength);
      return material.glow.rgb * a + background * (1.0 - a);
    }

    vec3 background = vec3(0.0, 0.2, 0.5);

    Material minecraftSponge = Material(
      vec3(1, 0.8, 0.1),
      vec4(0.0, 0.2, 0.0, 1),
      2.0,
      0.0
    );

    const int iters = 9;

    float Distance(vec3 point) {
      float shrinkFactor = 3.0;
      for (int i = 0; i < iters; i++) {
        point = Triflect(center, point);
        point = Triplicate(center + vec3(size / 6.0), point);
        point = Shrink(shrinkFactor, center + vec3(size / 2.0), point);
      }
      return CubeDistance(point, Cube(center, size)) / pow(shrinkFactor, float(iters));
    }

    vec3 Trace(vec3 direction, vec3 position, Material material) {
      float minDistance = 2000000000.0;
      for (int i = 0; i < maxSteps; i++) {
        float distance = Distance(position);
        minDistance = min(minDistance, distance);
        if (distance < hitDistance) {
          return Shape(i, material);
        }
        position = position + distance * direction;
      }
      return Glow(minDistance, background, material);
    }

    void main() {
      float z = 0.7;
      vec3 direction = mat3(camera) * normalize(vec3(vcoord.x, vcoord.y / ratio, z));
      vec3 color = Trace(direction, position, minecraftSponge);
      gl_FragColor = vec4(color, 1);
    }
  |]