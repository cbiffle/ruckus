const float PI = 3.14159265358979323846;
const float ISOSURFACE = 0.0;
const vec3 LIGHT = vec3(0, 0, 100);

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
uniform float closeEnough;
uniform int stepLimit;
uniform bool showComplexity;
uniform bool showDistance;

//////////////////////////////////////////////////////////////
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

float distanceField(vec3 r0);

vec3 distanceFieldNormal(vec3 pos) {
  // Computes the distance field gradient using the method of central
  // differences, which also happens to approximate the normal of a
  // nearby surface.
  float h = closeEnough / 2.;

  vec3 g = vec3(
    distanceField(pos + vec3(h, 0, 0)) - distanceField(pos - vec3(h, 0, 0)),
    distanceField(pos + vec3(0, h, 0)) - distanceField(pos - vec3(0, h, 0)),
    distanceField(pos + vec3(0, 0, h)) - distanceField(pos - vec3(0, 0, h)));

  // TODO: is normalizing this vector necessary?
  return normalize(g);
}

vec3 hsv2rgb(vec3 c) {
  // Source: http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec4 falseColor(float level) {
  return vec4(hsv2rgb(vec3(level + (120. / 360.), 1, 1)), 1);
}

void main() {
  float near =  resolution.x * 1.2 / 2.;
  float far  = -resolution.x * 1.2 / 2.;

  vec3 cameraPosition = vec3(0, 0, 20);

  vec3 pos = vec3(gl_FragCoord.xy - resolution.xy / 2., near);
  vec3 npos = pos / resolution.x;
  
  vec3 dir = normalize(npos - cameraPosition);
  vec3 toEye = -dir;

  vec4 color = vec4(0.1, 0.1, 0.3, 1);

  vec3 rLight = qrot(orientation, LIGHT);

  int stepsTaken = 0;
  float d = 0.;
  for (int steps = 0; steps < stepLimit; ++steps) {
    stepsTaken = steps;

    if (pos.z < far) break;

    vec3 tpos = qrot(orientation, pos);
    
    d = distanceField(tpos) - ISOSURFACE;
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
      pos += d * TRUST * dir;
    }
  }

  vec4 complexity = falseColor(float(stepsTaken) / float(stepLimit));
  vec4 distance = falseColor(clamp(d / closeEnough, 0., 1.));

  gl_FragColor = showComplexity ? complexity
                                : showDistance ? distance : color;
}

