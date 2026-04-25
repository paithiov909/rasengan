#include "rasengan_types.h"
#include <cpp11.hpp>

[[cpp11::register]]
cpp11::external_pointer<BouncingPoints> new_bouncing_pts_cpp(
    const cpp11::doubles x, const cpp11::doubles y, const cpp11::doubles vx,
    const cpp11::doubles vy, double xmin, double xmax, double ymin, double ymax,
    double restitution) {
  if (x.size() != y.size() || x.size() != vx.size() || x.size() != vy.size()) {
    cpp11::stop("x, y, vx, vy must have the same length");
  }

  std::vector<Point2D> pts;
  pts.reserve(x.size());

  for (R_xlen_t i = 0; i < x.size(); ++i) {
    pts.push_back(Point2D{x[i], y[i], vx[i], vy[i]});
  }

  return cpp11::external_pointer<BouncingPoints>(
      new BouncingPoints(std::move(pts), xmin, xmax, ymin, ymax, restitution),
      true);
}

[[cpp11::register]]
void proceed_bouncing_pts_cpp(cpp11::external_pointer<BouncingPoints> state,
                              double dt, int n_steps) {
  if (n_steps < 1) {
    cpp11::stop("n_steps must be >= 1");
  }
  state->proceed(dt, n_steps);
}

[[cpp11::register]]
void reset_bouncing_pts_cpp(cpp11::external_pointer<BouncingPoints> state) {
  state->reset();
}

[[cpp11::register]]
cpp11::list bouncing_pts_as_list_cpp(
    cpp11::external_pointer<BouncingPoints> state) {
  std::size_t n = state->size();

  cpp11::writable::integers id(n);
  cpp11::writable::doubles x(n), y(n), vx(n), vy(n);

  for (std::size_t i = 0; i < n; ++i) {
    const auto& p = state->points_[i];
    id[i] = static_cast<int>(i + 1);
    x[i] = p.x;
    y[i] = p.y;
    vx[i] = p.vx;
    vy[i] = p.vy;
  }

  cpp11::writable::list out;
  out.push_back(id);
  out.push_back(x);
  out.push_back(y);
  out.push_back(vx);
  out.push_back(vy);
  out.names() = {"id", "x", "y", "vx", "vy"};

  return out;
}
