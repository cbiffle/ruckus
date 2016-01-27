const vec3 LIGHT = vec3(0, 0, 100);
const float TRUST = 1.0;

const float i_a = 0.3;
const float i_d = 0.6;
const float i_s = 0.4;

const float k_a = 0.6;
const float k_d = 0.7;
const float k_s = 0.8;
const float alpha = 8.;

uniform vec4 orientation;
uniform float zoom;
uniform float closeEnough;
uniform int stepLimit;
uniform bool showComplexity;
uniform bool showDistance;

out vec4 fragColor;

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
      uint nid = nearestNodeId(tpos);
      vec3 normal = distanceFieldNormal(tpos, closeEnough);
      vec3 nl_m = normalize(rLight);
      vec3 r_m = -reflect(nl_m, normal);
      
      float ambient = i_a * k_a;
      float diffuse = i_d * k_d * max(dot(nl_m, normal), 0.);
      float spec = i_s * k_s * pow(max(dot(r_m, toEye), 0.), alpha);

      vec3 light = matColor(nid) * (ambient + diffuse) + spec;
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

  fragColor = showComplexity ? complexity
                             : showDistance ? distance : color;
}

