#include <cpp11.hpp>
#include "fbm/fbm.hpp"
#include "fbm/generate_normal_random.hpp"

using namespace cpp11;

[[cpp11::register]]
doubles fbm_1d_cpp(double hurst_idx, const std::vector<double>& X) {
  const std::size_t N = X.size();
  const auto d = fbm::fractional_brownian_motion_1d(hurst_idx, N, X);
  return cpp11::as_sexp(d);
}

[[cpp11::register]]
doubles fbr_1d_cpp(double hurst_idx, const std::vector<double>& X) {
  const std::size_t N = X.size();
  const auto d = fbm::fractional_brownian_bridge_1d(hurst_idx, N, X);
  return cpp11::as_sexp(d);
}

[[cpp11::register]]
doubles fbr_2d_cpp(double hurst_idx, const std::vector<double>& X) {
  const std::size_t N = std::sqrt(X.size());
  const auto d = fbm::fractional_brownian_bridge_2d(hurst_idx, N, X, false);
  return cpp11::as_sexp(d);
}

// [[cpp11::register]]
// doubles fbr_3d_cpp(double hurst_idx, const std::vector<double>& X) {
//   const std::size_t N = std::cbrt(X.size());
//   const auto d = fbm::fractional_brownian_bridge_3d(hurst_idx, N, X, false);
//   return cpp11::as_sexp(d);
// }
