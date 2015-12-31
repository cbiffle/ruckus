const float PI = 3.14159265358979323846;
const int QUAL = 50;
const float EPSILON = 0.5;
const float ISOSURFACE = 0.0;
const vec3 LIGHT = vec3(0, 0, 100);

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
  const float h = EPSILON;

  vec3 g = vec3(
    distanceField(pos + vec3(h, 0, 0)) - distanceField(pos - vec3(h, 0, 0)),
    distanceField(pos + vec3(0, h, 0)) - distanceField(pos - vec3(0, h, 0)),
    distanceField(pos + vec3(0, 0, h)) - distanceField(pos - vec3(0, 0, h)));

  // TODO: is normalizing this vector necessary?
  return normalize(g);
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
  for (int steps = 0; steps < QUAL; ++steps) {
    stepsTaken = steps;

    if (pos.z < far) break;

    vec3 tpos = qrot(orientation, pos);
    
    float d = distanceField(tpos) - ISOSURFACE;
    if (d < EPSILON) {
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
      pos += d * dir;
    }
  }

  vec4 complexity = vec4(sin(float(stepsTaken - 8) / 8.),
                         sin(float(stepsTaken) / 8.),
                         sin(clamp(float(stepsTaken) / 4. + PI/2., PI/2., PI)),
                         1);

  gl_FragColor = color; // complexity;
}

