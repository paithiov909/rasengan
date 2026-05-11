#include "Voronoi.h"
#include <cpp11.hpp>

struct Vector2 {
  float x, y;
};

[[cpp11::register]]
cpp11::list delaunay_cpp(const cpp11::doubles_matrix<>& pts) {
  std::vector<Vector2> points;
  for (int i = 0; i < pts.nrow(); ++i) {
    points.emplace_back(
        Vector2{static_cast<float>(pts(i, 0)), static_cast<float>(pts(i, 1))});
  }
  auto ret = Voronoi::generate(points.cbegin(), points.cend());

  if (ret.vertices.empty()) {
    cpp11::stop("Failed to generate Delaunay triangulation");
  }

  cpp11::writable::integers idx;
  cpp11::writable::doubles cc_x, cc_y, radii;
  cpp11::writable::doubles vert_x, vert_y;
  int i = 0;

  for (auto& vertex : ret.vertices) {
    auto circ = vertex.circumcenter;
    auto radius = vertex.radius;
    auto [a, b, c] = vertex.triangle;

    idx.push_back(++i);

    cc_x.push_back(circ.x);
    cc_y.push_back(circ.y);
    radii.push_back(radius);

    vert_x.push_back(a->x);
    vert_y.push_back(a->y);
    vert_x.push_back(b->x);
    vert_y.push_back(b->y);
    vert_x.push_back(c->x);
    vert_y.push_back(c->y);
  }

  cpp11::writable::list out = {idx, cc_x, cc_y, radii, vert_x, vert_y};
  out.names() = {"idx", "cc_x", "cc_y", "radii", "vert_x", "vert_y"};
  return out;
}
