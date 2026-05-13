#pragma once
#include <cpp11.hpp>
#include <vector>

namespace sh {

#define CHECK(condition, message) \
    if (!(condition)) { \
      cpp11::stop(message); \
    }

class Vector3d {
 private:
  const std::tuple<double, double, double> xyz_;

 public:
  Vector3d(const std::tuple<double, double, double>&& d) : xyz_(d) {}

  double x() const { return std::get<0>(xyz_); }
  double y() const { return std::get<1>(xyz_); }
  double z() const { return std::get<2>(xyz_); }

  double squaredNorm() const { return x() * x() + y() * y() + z() * z(); }
};

double EvalSH(int l, int m, double phi, double theta);

double EvalSH(int l, int m, const Vector3d& dir);

double EvalSHSlow(int l, int m, double phi, double theta);

double EvalSHSlow(int l, int m, const Vector3d& dir);

}  // namespace sh
