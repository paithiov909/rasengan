#include <cpp11.hpp>

using namespace cpp11;

[[cpp11::register]]
doubles bounce_cpp(int n, double init, double velocity, double damping, double mass,
                   double min, double max) {
  writable::doubles result(n);
  double pos = init;
  for (int i = 0; i < n; i++) {
    result[i] = pos;
    pos += velocity;
    if (pos + mass > max) {
      pos = max;
      velocity *= damping;
    } else if (pos - mass < min) {
      pos = min;
      velocity *= damping;
    }
  }
  return result;
}
