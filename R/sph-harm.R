#' Evaluate real spherical harmonics
#'
#' @description
#' Evaluates the real spherical harmonic basis function of degree `l`
#' and order `m`.
#'
#' `x` must be either:
#'
#' - a matrix with 2 columns giving spherical coordinates
#'   `theta` and `phi`, or
#' - a matrix with 3 columns giving Cartesian direction vectors
#'   `x`, `y`, and `z`.
#'
#' When `x` has 3 columns, each row is interpreted as a direction vector.
#' Direction vectors should typically be normalized to unit length before
#' evaluation.
#'
#' @details
#' For spherical coordinates, the first column is interpreted as `theta`
#' and the second column as `phi`.
#'
#' For Cartesian coordinates, rows are interpreted as direction vectors
#' `(x, y, z)`.
#'
#' @param x A numeric matrix containing spherical coordinates or direction
#'  vectors.
#' @param l Integer scalar giving the degree of the spherical harmonic.
#' @param m Integer scalar giving the order of the spherical harmonic.
#' @returns
#' A numeric vector containing the evaluated real spherical harmonic values.
#' @export
sph_harm <- function(x, l, m) {
  x <- as.matrix(x)
  if (rlang::is_empty(x)) {
    cli::cli_abort("`x` must be non-empty.")
  }
  nc <- ncol(x)
  if (nc == 3L) {
    return(sph3d_cpp(as.integer(l), as.integer(m), x))
  } else if (nc == 2L) {
    return(sph2d_cpp(as.integer(l), as.integer(m), x))
  }
  # otherwise
  cli::cli_abort("`x` must have 2 or 3 columns.")
  NA_real_
}
