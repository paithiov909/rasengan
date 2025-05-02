#' Generate an Euler spiral or biarc curve between two oriented points
#'
#' @description
#' Computes a planar curve connecting two points,
#' each with a specified orientation angle.
#' Two methods are available: a single Euler spiral, or a biarc composed of two spiral segments.
#'
#' @details
#' When `biarch = TRUE`, the curve is constructed from two spiral segments computed using
#' an internal algorithm that attempts to connect the start and goal points with \( G^1 \) continuity.
#' This approach may be more robust in cases where a single spiral does not converge well.
#'
#' When `biarch = FALSE`, the function attempts to compute a single Euler spiral that smoothly
#' interpolates between the two endpoints and their respective directions. This may fail to converge
#' in some configurations.
#'
#' @param start A numeric vector of length 3
#' specifying the starting point and heading direction:
#' `c(x, y, theta)`, where `theta` is the orientation angle in radians.
#' @param end A numeric vector of length 3
#' specifying the goal point and heading direction:
#' `c(x, y, theta)`, where `theta` is the orientation angle in radians.
#' @param max_n Maximum number of points to sample along the curve.
#' @param max_iter_num Maximum number of iterations
#' used in the internal parameter estimation algorithm
#' (used only when `biarch = FALSE`).
#' @param biarch Logical; if `TRUE`,
#' use a biarc approximation consisting of two Euler spiral segments.
#' If `FALSE`, compute a single spiral curve via numerical estimation.
#' @returns A data frame with columns `x`, `y`, and `theta`,
#' giving the sampled positions and orientations along the generated curve.
#' If the parameter estimation fails,
#' the returned data may contain missing values, which are automatically removed.
#' @seealso
#' [CoffeeKumazaki/euler_spiral: The c++ implementation for Euler spiral (clothoid).](https://github.com/CoffeeKumazaki/euler_spiral)
#' @export
euler_curve <- function(start = c(0, 0, pi / 4),
                        end = c(25, 10, pi),
                        max_n = 1e2,
                        max_iter_num = 1e3,
                        biarch = TRUE) {
  stopifnot(
    length(start) == 3,
    length(end) == 3,
    max_n > 0, max_iter_num > 0
  )
  if (biarch) {
    ret <- es_biarc_cpp(start, end, max_n)
  } else {
    ret <- es_spiral_cpp(start, end, max_iter_num, max_n)
  }
  colnames(ret) <- c("x", "y", "theta")
  ret <- ret[is.finite(ret[, "x"]) & is.finite(ret[, "y"]), ]
  as.data.frame(ret)
}
