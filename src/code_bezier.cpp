#include "Bezier.h"
#include <cpp11.hpp>

[[cpp11::register]]
cpp11::doubles_matrix<> bezier_value_at_cpp(const cpp11::doubles_matrix<>& pts,
                                            const cpp11::doubles& t) {
  const auto nr = pts.nrow();
  const auto len = t.size();
  cpp11::writable::doubles_matrix<> out(nr * len, 4);

  for (int i = 0; i < nr; ++i) {
    const bezier::Bezier<3> bz({{pts(i, 0), pts(i, 1)},
                                {pts(i, 2), pts(i, 3)},
                                {pts(i, 4), pts(i, 5)},
                                {pts(i, 6), pts(i, 7)}});
    for (int j = 0; j < len; ++j) {
      const auto [x, y] = bz.valueAt(t[j]);
      out(i * len + j, 0) = i + 1;
      out(i * len + j, 1) = j + 1;
      out(i * len + j, 2) = x;
      out(i * len + j, 3) = y;
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::doubles_matrix<> bezier_derivative_at_cpp(
    const cpp11::doubles_matrix<>& pts, const cpp11::doubles& t) {
  const auto nr = pts.nrow();
  const auto len = t.size();
  cpp11::writable::doubles_matrix<> out(nr * len, 4);

  for (int i = 0; i < nr; ++i) {
    const bezier::Bezier<3> bz({{pts(i, 0), pts(i, 1)},
                                {pts(i, 2), pts(i, 3)},
                                {pts(i, 4), pts(i, 5)},
                                {pts(i, 6), pts(i, 7)}});
    const bezier::Bezier<2> db = bz.derivative();
    for (int j = 0; j < len; ++j) {
      const auto [dx, dy] = db.valueAt(t[j]);
      out(i * len + j, 0) = i + 1;
      out(i * len + j, 1) = j + 1;
      out(i * len + j, 2) = dx;
      out(i * len + j, 3) = dy;
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::doubles_matrix<> bezier_normal_at_cpp(const cpp11::doubles_matrix<>& pts,
                                             const cpp11::doubles& t) {
  const auto nr = pts.nrow();
  const auto len = t.size();
  cpp11::writable::doubles_matrix<> out(nr * len, 4);

  for (int i = 0; i < nr; ++i) {
    const bezier::Bezier<3> bz({{pts(i, 0), pts(i, 1)},
                                {pts(i, 2), pts(i, 3)},
                                {pts(i, 4), pts(i, 5)},
                                {pts(i, 6), pts(i, 7)}});
    for (int j = 0; j < len; ++j) {
      const auto [nx, ny] = bz.normalAt(t[i]);
      out(i * len + j, 0) = i + 1;
      out(i * len + j, 1) = j + 1;
      out(i * len + j, 2) = nx;
      out(i * len + j, 3) = ny;
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::doubles_matrix<> bezier_tangent_at_cpp(
    const cpp11::doubles_matrix<>& pts, const cpp11::doubles& t) {
  const auto nr = pts.nrow();
  const auto len = t.size();
  cpp11::writable::doubles_matrix<> out(nr * len, 4);

  for (int i = 0; i < nr; ++i) {
    const bezier::Bezier<3> bz({{pts(i, 0), pts(i, 1)},
                                {pts(i, 2), pts(i, 3)},
                                {pts(i, 4), pts(i, 5)},
                                {pts(i, 6), pts(i, 7)}});
    for (int j = 0; j < len; ++j) {
      const auto [tx, ty] = bz.tangentAt(t[i]);
      out(i * len + j, 0) = i + 1;
      out(i * len + j, 1) = j + 1;
      out(i * len + j, 2) = tx;
      out(i * len + j, 3) = ty;
    }
  }
  return out;
}
