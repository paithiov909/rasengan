#include <cpp11.hpp>
#include <vector>
#include <array>
#include <cmath>
#include <random>

using namespace cpp11;

[[cpp11::register]]
doubles_matrix<> wind_mouse_cpp(doubles start,  // 長さ2: [startX, startY]
                                doubles end,    // 長さ2: [endX, endY]
                                double gravity, double wind, double min_wait,
                                double max_wait, double max_step,
                                double target_area, double mouse_speed,
                                int seed) {
  double startX = start[0], startY = start[1];
  double endX = end[0], endY = end[1];

  if (gravity < 1.0) gravity = 1.0;
  if (max_step == 0.0) max_step = 0.01;

  double dist = std::hypot(endX - startX, endY - startY);

  std::mt19937 gen(seed);
  std::uniform_real_distribution<> rand01(0.0, 1.0);

  double windX = static_cast<int>(rand01(gen) * 10.0);
  double windY = static_cast<int>(rand01(gen) * 10.0);
  double velocityX = 0.0;
  double velocityY = 0.0;
  double veloMag = 0.0;
  double randomDist = 0.0;
  double step = 0.0;

  double newX = std::round(startX);
  double newY = std::round(startY);
  double oldX, oldY;
  double current_wait = 0.0;

  const double sqrt2 = std::sqrt(2.0);
  const double sqrt3 = std::sqrt(3.0);
  const double sqrt5 = std::sqrt(5.0);
  const double waitDiff = max_wait - min_wait;

  std::vector<std::array<double, 3>> points;

  while (dist > 1.0) {
    wind = std::min(wind, dist);

    if (dist >= target_area) {
      int w = static_cast<int>(rand01(gen) * (static_cast<int>(wind) * 2 + 1));
      windX = windX / sqrt3 + (w - wind) / sqrt5;
      windY = windY / sqrt3 + (w - wind) / sqrt5;
    } else {
      windX /= sqrt2;
      windY /= sqrt2;
      if (max_step < 3.0) {
        max_step =
            static_cast<double>(std::uniform_int_distribution<>(3, 5)(gen));
      } else {
        max_step /= sqrt5;
      }
    }

    velocityX += windX + gravity * (endX - startX) / dist;
    velocityY += windY + gravity * (endY - startY) / dist;

    veloMag = std::hypot(velocityX, velocityY);
    if (veloMag > max_step) {
      randomDist = max_step / 2.0 + rand01(gen) * (max_step / 2.0);
      velocityX = (velocityX / veloMag) * randomDist;
      velocityY = (velocityY / veloMag) * randomDist;
    }

    oldX = std::round(startX);
    oldY = std::round(startY);

    startX += velocityX;
    startY += velocityY;

    dist = std::hypot(endX - startX, endY - startY);

    newX = std::round(startX);
    newY = std::round(startY);

    step = std::hypot(startX - oldX, startY - oldY);
    double wait = std::round(waitDiff * (step / max_step) + min_wait);
    current_wait += wait;

    if (oldX != newX || oldY != newY) {
      points.push_back({newX, newY, current_wait});
    }
  }

  double finalX = std::round(endX);
  double finalY = std::round(endY);
  if (finalX != newX || finalY != newY) {
    points.push_back({finalX, finalY, current_wait});
  }

  writable::doubles_matrix<> result(points.size(), 3);
  for (size_t i = 0; i < points.size(); ++i) {
    result(i, 0) = points[i][0];
    result(i, 1) = points[i][1];
    result(i, 2) = points[i][2];
  }

  return result;
}
