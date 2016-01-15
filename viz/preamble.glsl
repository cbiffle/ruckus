#version 130

const float PI = 3.14159265358979323846;
const float ISOSURFACE = 0.0;
const vec3 LIGHT = vec3(0, 0, 100);
const vec4 BACKGROUND = vec4(0.1, 0.1, 0.3, 1);

const float TRUST = 1.0;

const float i_a = 0.3;
const float i_d = 0.6;
const float i_s = 0.4;


const float k_a = 0.6;
const float k_d = 0.7;
const float k_s = 0.8;
const float alpha = 8.;
const vec3 MAT_COLOR = vec3(1, 0.8, 0.3);

uniform vec2 resolution;
uniform vec4 orientation;
uniform float zoom;
uniform float closeEnough;
uniform int stepLimit;
uniform bool showComplexity;
uniform bool showDistance;

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


////////////////////////////////////////////////////////////////////////////////
// Global distance field evaluator.

// Prototype; generated at runtime and appended to this file.
float distanceField(vec3 r0);

// Computes the distance field gradient using the method of central
// differences, which also happens to approximate the normal of a
// nearby surface.  We use it for lighting calculations.
vec3 distanceFieldNormal(vec3 pos) {
  // TODO: should this actually depend on the intersection threshold?
  float h = closeEnough / 2.;

  vec3 g = vec3(
    distanceField(pos + vec3(h, 0, 0)) - distanceField(pos - vec3(h, 0, 0)),
    distanceField(pos + vec3(0, h, 0)) - distanceField(pos - vec3(0, h, 0)),
    distanceField(pos + vec3(0, 0, h)) - distanceField(pos - vec3(0, 0, h)));

  return normalize(g);
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


////////////////////////////////////////////////////////////////////////////////
// Fragment shader entry.

void main() {
  // Paperwork!

  // Clipping plane Z coordinates.
  float near =  1500.;
  float far  = -1500.;

  // TODO: camera Z needs to depend on clip plane positions!
  vec3 cameraPosition = vec3(0, 0, near + near);

  // Position of pixel being traced on the near clip plane.
  vec3 pos = vec3(gl_FragCoord.xy - resolution.xy / 2., near);
  vec3 npos = pos / resolution.x;
  
  // Direction in which this ray will advance.
  vec3 dir = normalize(pos - cameraPosition);
  // Shorthand for the object-to-eye vector, used for specular reflection.
  vec3 toEye = qrot(orientation, -dir);

  // Initialized with black to handle the "too many steps" case, this will
  // be overwritten if the ray either hits a surface, or escapes past the
  // far clipping plane.
  vec4 color = vec4(0, 0, 0, 1);

  // Rotate the light position into object space.
  vec3 rLight = qrot(orientation, LIGHT);

  // Ray-marching loop!

  int stepsTaken = 0;
  float d = 0.;
  for (int steps = 0; steps < stepLimit; ++steps) {
    // GLSL won't let us use an externally declared iteration variable, so
    // we copy 'steps' to 'stepsTaken' at each iteration so that we may use
    // it later to show march complexity.
    stepsTaken = steps;

    // Early-exit the marching process at the far clip plane.  This improves
    // performance significantly when software rendering with Mesa; unclear
    // whether it helps on hardware.
    if (pos.z < far) {
      color = BACKGROUND;
      break;
    }

    // Transform the ray's position into object space.
    // TODO: couldn't we just do the marching in object space?  Sure, it
    // makes the far clip plane check more expensive, but it's just a plane
    // test....
    vec3 tpos = qrot(orientation, pos) / zoom;
    
    d = (distanceField(tpos) - ISOSURFACE) * zoom;
    if (d <= closeEnough) {
      // Hit!
      vec3 normal = distanceFieldNormal(tpos);
      vec3 nl_m = normalize(rLight);
      vec3 r_m = -reflect(nl_m, normal);
      
      float ambient = i_a * k_a;
      float diffuse = i_d * k_d * max(dot(nl_m, normal), 0.);
      float spec = i_s * k_s * pow(max(dot(r_m, toEye), 0.), alpha);

      vec3 light = MAT_COLOR * (ambient + diffuse) + spec;
      color = vec4(light, 1);
      break;
    } else {
      // Keep on marching.  The TRUST ratio is used to de-rate the distance
      // estimate if we think it may not be a true lower bound.  TBD.
      pos += d * TRUST * dir;
    }
  }

  // At exit from the loop we've either hit, in which case 'color' is set,
  // or flown off into space, in which case we've got the background color.
  // In the latter case, 'd' is basically random.

  // Compute the alternate viz modes.
  vec4 complexity = falseColor(float(stepsTaken) / float(stepLimit));
  vec4 distance = falseColor(clamp(d / closeEnough, 0., 1.));

  gl_FragColor = showComplexity ? complexity
                                : showDistance ? distance : color;
}

