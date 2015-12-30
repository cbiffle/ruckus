const float PI = 3.14159265358979323846;
const int QUAL = 50;
const float EPSILON = 0.5;
const float ISOSURFACE = 0.0;
const float RADIUS = 100.0;
const vec3 LIGHT = vec3(0, 0, -1);

const float i_a = 0.3;
const float i_d = 0.6;
const float i_s = 0.4;


const float k_a = 0.6;
const float k_d = 0.7;
const float k_s = 0.8;
const float alpha = 8.;
const vec3 MAT_COLOR = vec3(1, 0.8, 0.3);

uniform vec2 resolution;

float sphere(vec3 center, float radius, vec3 p) {
  return length(p - center) - radius;
}

float halfspace(vec3 n, float d, vec3 p) {
  return dot(p, n) - d;
}

float cube(vec3 center, float size, vec3 pRaw) {
  vec3 p = pRaw - center;
  float h = size / 2.;
  float a = halfspace(vec3(+1, 0, 0), h, p);
  float b = halfspace(vec3(-1, 0, 0), h, p);
  float c = halfspace(vec3(0, +1, 0), h, p);
  float d = halfspace(vec3(0, -1, 0), h, p);
  float e = halfspace(vec3(0, 0, +1), h, p);
  float f = halfspace(vec3(0, 0, -1), h, p);

  return max(a, max(b, max(c, max(d, max(e, f)))));
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
  float near = -resolution.x * 1.2 / 2.;
  float far  =  resolution.x * 1.2 / 2.;

  vec3 cameraPosition = vec3(0, 0, -2);

  vec3 pos = vec3(gl_FragCoord.xy - resolution.xy / 2., near);
  vec3 npos = pos / resolution.x;
  
  vec3 dir = normalize(npos - cameraPosition);
  vec3 toEye = -dir;

  vec4 color = vec4(0.1, 0.1, 0.3, 1);

  int stepsTaken = 0;
  for (int steps = 0; steps < QUAL; ++steps) {
    stepsTaken = steps;

    if (pos.z > far) break;
    
    float d = distanceField(pos) - ISOSURFACE;
    if (d < EPSILON) {
      // Hit!
      vec3 normal = distanceFieldNormal(pos);
      vec3 nl_m = normalize(LIGHT);
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

