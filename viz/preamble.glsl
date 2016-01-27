#version 130

const float PI = 3.14159265358979323846;
const float ISOSURFACE = 0.0;
const vec4 BACKGROUND = vec4(0.1, 0.1, 0.3, 1);

uniform vec2 resolution;
uniform sampler2D nodeColors;

float smin(float k, float a, float b) {
    float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
    return mix(b, a, h) - k * h * (1.0 - h);
}

////////////////////////////////////////////////////////////////////////////////
// Quaternion support.  Note that quaternions are represented
// with the vector portion in xyz and the scalar in w.

// Conjugate.
vec4 qconj(vec4 q) {
  return vec4(-q.xyz, q.w);
}

// Quaternion multiplication.
vec4 qmul(vec4 p, vec4 q) {
  return vec4(p.w * q.xyz + q.w * p.xyz + cross(p.xyz, q.xyz),
              p.w * q.w - dot(p.xyz, q.xyz));
}

// Rotate vector by quaternion, giving new vector.
vec3 qrot(vec4 q, vec3 v) {
  return qmul(qmul(q, vec4(v, 0)), qconj(q)).xyz;
}


////////////////////////////////////////////////////////////////////////////////
// Primitive distance field evaluators.  These are defined here and invoked
// from generated code to reduce duplication, because while I'm pretty
// confident that GLSL compilers can inline, I'm not sure they can extract
// functions.

float dfSphere(float radius, vec3 p) {
  return length(p) - radius;
}

float dfBox(vec3 corner, vec3 p) {
  vec3 d = abs(p) - corner;
  return min(max(d.x, max(d.y, d.z)), 0.)
       + length(max(d, 0.));
}

float dfCapsule(float halfh, float r, vec3 q) {
  q = abs(q);
  float t = clamp((q.z * halfh) / (halfh * halfh), 0.0, 1.0);
  return length(q - vec3(0, 0, halfh * t)) - r;
}

vec3 radialProject(vec3 pos, float period, float shift) {
  float a = pos.x == 0. ? (sign(pos.y) * (PI / 2.)) : atan(pos.y, pos.x);
  float d = length(pos.xy);
  float a_ = mod(a + (period/2.), period) - (period/2.) + shift;
  return vec3(d * cos(a_),
              d * sin(a_),
              pos.z);
}
  

////////////////////////////////////////////////////////////////////////////////
// Global distance field evaluator.

// Prototype; generated at runtime and appended to this file.
float distanceField(vec3 r0);

// Computes the distance field gradient using the method of central
// differences, which also happens to approximate the normal of a
// nearby surface.  We use it for lighting calculations.
vec3 distanceFieldNormal(vec3 pos, float eps) {
  // TODO: should this actually depend on the intersection threshold?
  float h = eps / 2.;

  vec3 g = vec3(
    distanceField(pos + vec3(h, 0, 0)) - distanceField(pos - vec3(h, 0, 0)),
    distanceField(pos + vec3(0, h, 0)) - distanceField(pos - vec3(0, h, 0)),
    distanceField(pos + vec3(0, 0, h)) - distanceField(pos - vec3(0, 0, h)));

  return normalize(g);
}


////////////////////////////////////////////////////////////////////////////////
// Object discrimination.

// Prototype; generated at runtime and appended to this file.
uint nearestNodeId(vec3 r0);

vec3 matColor(uint nid) {
  return texelFetch(nodeColors, ivec2(nid % 512u, nid / 512u), 0).rgb;
}


////////////////////////////////////////////////////////////////////////////////
// False-color visualization support.

// Converts an HSV color to its RGB equivalent.  Generating false colors in
// HSV is *far* more convenient, so we pay the (low) cost.
// Source: http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
vec3 hsv2rgb(vec3 c) {
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

// Generates a color intended to make gradations between 0 and 1 more obvious.
// The current implementation wraps around above 1, so you may want to clamp.
vec4 falseColor(float level) {
  return vec4(hsv2rgb(vec3(level + (120. / 360.), 1, 1)), 1);
}
