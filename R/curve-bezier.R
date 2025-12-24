#' Bezier curve utilities
#'
#' @description
#' Functions for evaluating cubic Bezier curves and their associated vector
#' quantities. A cubic Bezier curve is defined by four control points
#' \eqn{P_0, P_1, P_2, P_3}, where \eqn{P_0} and \eqn{P_3} are endpoints and
#' \eqn{P_1}, \eqn{P_2} act as handles controlling the tangent directions.
#'
#' All functions accept a 8 column numeric matrix or data frame
#' containing control points, where each row encodes the four points as:
#' `(x0, y0, x1, y1, x2, y2, x3, y3)`.
#'
#' @details
#' * `curve_bezier(n, control_pts)`:
#'  Evaluate cubic Bezier curves at `n` evenly spaced parameter values in
#'  \eqn{t \in [0, 1]}.
#'  Returns a table of `(x, y)` positions for each curve `id`.
#' * `bezier_derivative(n, control_pts)`:
#'  Compute first derivatives of each curve at `n` points.
#'  The result contains the velocity vectors `(dx, dy)` at each `t`.
#' * `bezier_normal(n, control_pts)`:
#'  Compute unit normal vectors `(nx, ny)` along each curve.
#'  Normals are derived from the first derivative and represent the
#'  direction perpendicular to the tangent at each point.
#' * `bezier_tangent(n, control_pts)`:
#'  Compute unit tangent vectors `(tx, ty)` along each curve.
#'  Tangents represent the direction of movement along the curve and are
#'  obtained by normalizing the first derivative.
#'
#' These functions are useful for geometric processing, constructing smooth
#' interpolations, and generating direction fields for animation or
#' stroke-based visual effects.
#' @param n An integer scalar; Number of points to sample along the curve.
#' @param control_pts A numeric matrix or data frame of control points.
#' @returns A tibble.
#'
#' @seealso [compute_handles()]
#' @family curve
#' @rdname bezier
#' @name bezier
NULL

coerce_control_pts <- function(pts) {
  if (rlang::is_empty(pts) || ncol(pts) != 8) {
    rlang::abort(
      "`control_pts` must have 8 columns.",
      call = rlang::caller_env()
    )
  }
  as.matrix(pts)
}

#' @rdname bezier
#' @export
curve_bezier <- function(n, control_pts) {
  control_pts <- coerce_control_pts(control_pts)
  t <- seq(0, 1, length.out = n)
  ret <- bezier_value_at_cpp(control_pts, t)
  colnames(ret) <- c("id", "t", "x", "y")
  entbl(ret)
}

#' @rdname bezier
#' @export
bezier_derivative <- function(n, control_pts) {
  control_pts <- coerce_control_pts(control_pts)
  t <- seq(0, 1, length.out = n)
  ret <- bezier_derivative_at_cpp(control_pts, t)
  colnames(ret) <- c("id", "t", "dx", "dy")
  entbl(ret)
}

#' @rdname bezier
#' @export
bezier_normal <- function(n, control_pts) {
  control_pts <- coerce_control_pts(control_pts)
  t <- seq(0, 1, length.out = n)
  ret <- bezier_normal_at_cpp(control_pts, t)
  colnames(ret) <- c("id", "t", "nx", "ny")
  entbl(ret)
}

#' @rdname bezier
#' @export
bezier_tangent <- function(n, control_pts) {
  control_pts <- coerce_control_pts(control_pts)
  t <- seq(0, 1, length.out = n)
  ret <- bezier_tangent_at_cpp(control_pts, t)
  colnames(ret) <- c("id", "t", "tx", "ty")
  entbl(ret)
}
