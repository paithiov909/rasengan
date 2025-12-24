#' Construct Bezier control points from a sequence of positions
#'
#' @description
#' Generates cubic Bezier control points from an ordered sequence of 2D points.
#' Given a `k * 2` matrix (or data frame) of points \eqn{P_0, P_1, \dots, P_{k-1}},
#' this function returns a tibble representing a set of Bezier segments.
#' Each output row encodes one cubic Bezier curve segment with control points
#' `(x0, y0, x1, y1, x2, y2, x3, y3)`, forming a curve from `P_i` to `P_{i+1}`.
#'
#' Different handle–generation strategies can be selected via `method`.
#'
#' @details
#' These helper functions are intended to be used together with
#' [`curve_bezier()`] and the vector–field functions
#' [`bezier_tangent()`], [`bezier_normal()`], and [`bezier_derivative()`].
#' They are useful for smoothing polyline data, constructing interpolating
#' curves, and generating stylized shapes for creative coding.
#'
#' ### `method = "catmull"`
#'  Uses Catmull–Rom spline to construct smooth Bezier segments.
#'  For each segment \eqn{P_i \to P_{i+1}}, interior points are computed as:
#'  \deqn{
#'    B_1 = P_i     + (P_{i+1} - P_{i-1}) / 6,\quad
#'    B_2 = P_{i+1} - (P_{i+2} - P_i)     / 6.
#'  }
#'  Produces \code{k - 3} segments (requires 4 or more points).
#'
#' ### `method = "linear"`
#'  Places both handles along the segment direction.
#'  Handles are spaced from the endpoints by `tension * distance(P_i, P_{i+1})`.
#'  Produces \code{k - 1} straight-style Bezier segments.
#'
#' ### `method = "normal"`
#'  Handles are placed perpendicular to the segment direction, producing
#'  decorative, "bent" or "blooming" curves.
#'  The handle offset is \eqn{\text{scale} \times \| P_{i+1} - P_i \|}.
#'  Use `side = "left"` or `"right"` to control the normal direction.
#'
#' ### `method = "tangent"`
#'  Computes tangents using centered differences, giving a smooth,
#'  spline-like interpolation.
#'  The handle offset is \eqn{\text{scale} \times \| P_{i+1} - P_i \|}.
#'  Produces \code{k - 1} \eqn{C^1}–continuous Bezier segments.
#'
#' @param points A `k * 2` matrix or data frame of points. Each row is `(x, y)`.
#' @param method Handle–generation strategy. One of
#'  `"catmull"`, `"linear"`, `"normal"`, `"tangent"`.
#' @param tension Numeric scaling factor for `"linear"` method (default `0.3`).
#' @param scale Numeric scaling factor for `"normal"` and `"tangent"` methods
#'  (default `0.3`).
#' @param side For `"normal"` method, choose `"left"` or `"right"` to determine
#'  the orientation of the normal vector.
#'
#' @returns
#' A tibble with columns
#' `x0, y0, x1, y1, x2, y2, x3, y3`,
#' containing one Bezier segment per row.
#'
#' @seealso [curve_bezier()]
#' @examples
#' pts <- matrix(rnorm(20), ncol = 2)
#'
#' # Tangent-based smoothing
#' segs <- compute_handles(pts, method = "tangent")
#' curve_bezier(n = 50, segs)
#' @export
compute_handles <- function(
  points,
  method = c("catmull", "linear", "normal", "tangent"),
  tension = 0.3,
  scale = 0.3,
  side = c("left", "right")
) {
  method <- rlang::arg_match(method)
  out <- switch(
    method,
    catmull = compute_handles_catmull(points),
    linear = compute_handles_linear(points, tension),
    normal = compute_handles_normal(points, scale, side),
    tangent = compute_handles_tangent(points, scale)
  )
  colnames(out) <- c("x0", "y0", "x1", "y1", "x2", "y2", "x3", "y3")
  entbl(out)
}

compute_handles_catmull <- function(points) {
  k <- nrow(points) %||% 0
  if (k < 4) {
    rlang::abort(
      "`points` must have at least 4 rows.",
      call = rlang::caller_env()
    )
  }
  P <- as.matrix(points)[, 1:2, drop = FALSE] # nolint

  segs <- vector("list", k - 3)

  for (i in seq_len(k - 3) + 1) {
    p0 <- P[i, ]
    p1 <- P[i, ] + (P[i + 1, ] - P[i - 1, ]) / 6
    p2 <- P[i + 1, ] - (P[i + 2, ] - P[i, ]) / 6
    p3 <- P[i + 1, ]

    segs[[i - 1]] <- c(p0, p1, p2, p3)
  }
  do.call(rbind, segs)
}

compute_handles_linear <- function(points, tension) {
  k <- nrow(points) %||% 0
  if (k < 2) {
    rlang::abort(
      "`points` must have at least 2 rows.",
      call = rlang::caller_env()
    )
  }
  P <- as.matrix(points)[, 1:2, drop = FALSE] # nolint

  segs <- vector("list", k - 1)

  for (i in seq_len(k - 1)) {
    p0 <- P[i, ]
    p3 <- P[i + 1, ]

    u <- normalise(p3 - p0)
    d <- sqrt(sum((p3 - p0)^2))

    p1 <- p0 + u * (d * tension)
    p2 <- p3 - u * (d * tension)

    segs[[i]] <- c(p0, p1, p2, p3)
  }
  do.call(rbind, segs)
}

compute_handles_normal <- function(
  points,
  scale,
  side = c("left", "right")
) {
  k <- nrow(points) %||% 0
  if (k < 2) {
    rlang::abort(
      "`points` must have at least 2 rows.",
      call = rlang::caller_env()
    )
  }
  P <- as.matrix(points)[, 1:2, drop = FALSE] # nolint

  side <- rlang::arg_match(side)

  # tangent for segment direction
  segtan <- function(i) normalise(P[i + 1, ] - P[i, ])
  # perpendicular
  perp <- function(v) c(-v[2], v[1])

  segs <- vector("list", k - 1)

  for (i in seq_len(k - 1)) {
    p0 <- P[i, ]
    p3 <- P[i + 1, ]

    t <- segtan(i)
    n <- normalize(perp(t))
    if (side == "right") {
      n <- -n
    }
    len <- sqrt(sum((p3 - p0)^2))

    p1 <- p0 + n * len * scale
    p2 <- p3 + n * len * scale

    segs[[i]] <- c(p0, p1, p2, p3)
  }
  do.call(rbind, segs)
}

compute_handles_tangent <- function(points, scale) {
  k <- nrow(points) %||% 0
  if (k < 2) {
    rlang::abort(
      "`points` must have at least 2 rows.",
      call = rlang::caller_env()
    )
  }
  P <- as.matrix(points)[, 1:2, drop = FALSE] # nolint

  # central differences for tangents
  tangent <- function(i) {
    if (i == 1) {
      normalise(P[2, ] - P[1, ])
    } else if (i == k) {
      normalise(P[k, ] - P[k - 1, ])
    } else {
      normalise(P[i + 1, ] - P[i - 1, ])
    }
  }

  segs <- vector("list", k - 1)

  for (i in seq_len(k - 1)) {
    p0 <- P[i, ]
    p3 <- P[i + 1, ]
    t0 <- tangent(i)
    t1 <- tangent(i + 1)

    len <- sqrt(sum((p3 - p0)^2))

    p1 <- p0 + t0 * len * scale
    p2 <- p3 - t1 * len * scale

    segs[[i]] <- c(p0, p1, p2, p3)
  }
  do.call(rbind, segs)
}
