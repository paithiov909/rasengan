#include <vector>

struct Point2D {
  double x;
  double y;
  double vx;
  double vy;
};

class BouncingPoints {
 public:
  std::vector<Point2D> points_;
  std::vector<Point2D> initial_points_;

  double xmin_ = 0.0;
  double xmax_ = 1.0;
  double ymin_ = 0.0;
  double ymax_ = 1.0;

  double restitution_ = 1.0;

  BouncingPoints(std::vector<Point2D> points, double xmin, double xmax,
                 double ymin, double ymax, double restitution)
      : points_(std::move(points)),
        initial_points_(points_),
        xmin_(xmin),
        xmax_(xmax),
        ymin_(ymin),
        ymax_(ymax),
        restitution_(restitution) {}

  void step(double dt) {
    for (auto& p : points_) {
      p.x += p.vx * dt;
      p.y += p.vy * dt;

      if (p.x < xmin_) {
        p.x = xmin_ + (xmin_ - p.x);
        p.vx = -p.vx * restitution_;
      } else if (p.x > xmax_) {
        p.x = xmax_ - (p.x - xmax_);
        p.vx = -p.vx * restitution_;
      }

      if (p.y < ymin_) {
        p.y = ymin_ + (ymin_ - p.y);
        p.vy = -p.vy * restitution_;
      } else if (p.y > ymax_) {
        p.y = ymax_ - (p.y - ymax_);
        p.vy = -p.vy * restitution_;
      }
    }
  }

  void proceed(double dt, int n_steps) {
    for (int i = 0; i < n_steps; ++i) {
      step(dt);
    }
  }

  void reset() { points_ = initial_points_; }

  void set_restitution(double r) { restitution_ = r; }

  std::size_t size() const { return points_.size(); }
};
