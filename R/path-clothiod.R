#' Generate an Euler spiral or biarc curve path
#'
#' @description
#' Computes a path connecting two points, each with a specified orientation angle.
#' Two methods are available: a single Euler spiral, or a biarc composed of two spiral segments.
#'
#' @details
#' When `biarch = TRUE`, the path is constructed from two spiral segments computed using
#' an internal algorithm that attempts to connect the start and goal points with \( G^1 \) continuity.
#' This approach may be more robust in cases where a single spiral does not converge well.
#'
#' When `biarch = FALSE`, the function attempts to compute a single Euler spiral that smoothly
#' interpolates between the two endpoints and their respective directions. This may fail to converge
#' in some configurations.
#'
#' @param start A numeric vector of length 3
#'  specifying the starting point and heading direction:
#'  `c(x, y, theta)`, where `theta` is the orientation angle in radians.
#' @param end A numeric vector of length 3
#'  specifying the goal point and heading direction:
#'  `c(x, y, theta)`, where `theta` is the orientation angle in radians.
#' @param max_n A numric scalar;
#'  Maximum number of points to sample along the path.
#' @param max_iter_num A numric scalar;
#'  Maximum number of iterations used in the internal parameter estimation algorithm
#'  (used only when `biarch = FALSE`).
#' @param biarch A logical scalar; If `TRUE`,
#'  use a biarc approximation consisting of two Euler spiral segments.
#'  If `FALSE`, compute a single spiral curve via numerical estimation.
#' @returns
#' A data frame with columns `x`, `y`, and `theta`,
#' giving the sampled positions and orientations along the generated path.
#' If the parameter estimation fails,
#' the returned data may contain missing values, which are automatically removed.
#' @seealso
#' [CoffeeKumazaki/euler_spiral: The c++ implementation for Euler spiral (clothoid).](https://github.com/CoffeeKumazaki/euler_spiral)
#' @export
#' @family path
#' @examples
#' \dontrun{
#' path <- path_clothoid()
#' with(path, plot(x, y, type = "l", asp = 1))
#' }
path_clothoid <- function(
  start = c(0, 0, pi / 4),
  end = c(25, 10, pi),
  max_n = 1e2,
  max_iter_num = 1e3,
  biarch = TRUE
) {
  if (length(start) != 3 || length(end) != 3) {
    rlang::abort("start and end must be vectors of length 3.")
  }
  if (max_n < 0) {
    rlang::abort("max_n must be non-negative.")
  }
  if (max_iter_num < 0) {
    rlang::abort("max_iter_num must be non-negative.")
  }
  if (biarch) {
    ret <- es_biarc_cpp(start, end, max_n)
  } else {
    ret <- es_spiral_cpp(start, end, max_iter_num, max_n)
  }
  colnames(ret) <- c("x", "y", "theta")
  ret <- ret[is.finite(ret[, "x"]) & is.finite(ret[, "y"]), ]
  entbl(ret)
}
