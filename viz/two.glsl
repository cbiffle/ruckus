uniform float zoom;
uniform float closeEnough;

out vec4 fragColor;

void main() {
  vec3 pos = vec3(gl_FragCoord.xy - resolution.xy / 2., 0) / zoom;
    
  float d = (distanceField(pos) - ISOSURFACE) * zoom;
  if (d <= 0) {
    // Hit something!
    uint nid = nearestNodeId(pos);
    if (d < -closeEnough) {
      // Internal surface.
      fragColor = vec4(matColor(nid), 1);
    } else {
      // Edge.
      fragColor = vec4(matColor(nid) * 0.5, 1);
    }
  } else {
    // Miss.
    fragColor = vec4(1,1,1,1);
  }
}

