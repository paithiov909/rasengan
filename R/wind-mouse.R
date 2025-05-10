#' Generate a human-like mouse movement path
#'
#' Simulates a human-like mouse movement trajectory between two points using a
#' physics-inspired algorithm with gravity, wind, and random waiting times.
#'
#' This function generates a sequence of points resembling how a human might
#' move a mouse cursor from `start` to `end`.
#' It is a port from [arevi/wind-mouse](https://github.com/arevi/wind-mouse).
#'
#' @param start A numeric vector of length 2 giving the starting coordinates (x, y).
#' @param end A numeric vector of length 2 giving the target coordinates (x, y).
#' @param mouse_speed A base speed factor that affects the dynamics of movement.
#' @param gravity Controls the strength of "pull" toward the target.
#' @param wind Controls the random "wind-like" movement during the motion.
#' @param min_wait Minimum wait time (ms) per step, affects timestamp `t`.
#' @param max_wait Maximum wait time (ms) per step.
#' @param max_step Maximum movement length per iteration.
#' @param target_area Radius within which the movement slows down and stabilizes.
#' @param seed Random seed for reproducible paths.
#' @returns A data frame with columns:
#' * `x`: X coordinate of the cursor.
#' * `y`: Y coordinate of the cursor.
#' * `t`: Cumulative time in milliseconds since the start.
#' @export
#' @examples
#' path <- wind_mouse(start = c(0, 0), end = c(300, 200), seed = 123)
#' with(path, plot(x, y, type = "l", asp = 1, main = "WindMouse Path"))
wind_mouse <- function(
  start = c(0, 0),
  end = c(100, 100),
  mouse_speed = 3.0,
  gravity = 9.0,
  wind = 3.0,
  min_wait = 5,
  max_wait = 15,
  max_step = 10,
  target_area = 10,
  seed = sample.int(1337, 1)
) {
  stopifnot(
    length(start) == 2,
    length(end) == 2
  )
  ret <-
    wind_mouse_cpp(
      start, end,
      gravity,
      wind,
      min_wait,
      max_wait,
      max_step,
      target_area,
      mouse_speed,
      seed
    )
  colnames(ret) <- c("x", "y", "t")
  as.data.frame(ret)
}
