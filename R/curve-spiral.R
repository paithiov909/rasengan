#' Generate an Archimedean spiral
#'
#' Generates a 2D curve that is a general Archimedean spiral
#' where the raidus `r` is defined as follows:
#' \deqn{
#' r = a + b \cdot \theta^\frac{1}{c}
#' }
#'
#' According to the [Wikipedia article](https://en.wikipedia.org/wiki/Archimedean_spiral),
#' the normal Archimedean spiral occurs when `c = 1`.
#' Other spirals falling into this group include the hyperbolic spiral (`c = -1`),
#' Fermat's spiral (`c = 2`), and the lituus (`c = −2`).
#'
#' @param n An integer scalar; Number of points to sample along the curve.
#' @param a,b,c Numeric scalars; Parameters of the general Archimedean spiral.
#' @param base A numeric scalar; Base of the logarithm used to compute the spacing between points.
#' @returns A data frame with columns `x` and `y`.
#' @export
curve_archimedean <- function(n, a = 0, b = 1, c = 1, base = exp(1)) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  rd <- a + b * theta^(1 / c)
  ret <- data.frame(
    x = rd * cos(theta),
    y = rd * sin(theta)
  )
  entbl(ret)
}

#' Generate a balance spring curve
#'
#' Generates a 2D curve that is a balance spring curve.
#'
#' @param n An integer scalar; Number of points to sample along the curve.
#' @param k,m Numeric scalars; Parameters of the balance spring curve.
#' @param scale A numeric scalar; Scaling factor for the curve.
#' @param base A numeric scalar; Base of the logarithm used to compute the spacing between points.
#' @returns A data frame with columns `x` and `y`.
#' @export
curve_spring <- function(n, k = 1, m = .1, scale = 1, base = exp(1)) {
  stopifnot(
    scale >= 0
  )
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  rd <- scale / (1 + k * exp(1)^(m * theta))
  ret <- data.frame(
    x = rd * cos(theta),
    y = rd * sin(theta)
  )
  entbl(ret)
}

#' Generate an involute curve
#'
#' Generates a 2D curve that is an involute curve.
#'
#' @param n An integer scalar; Number of points to sample along the curve.
#' @param scale A numeric scalar; Scaling factor for the curve.
#' @param base A numeric scalar; Base of the logarithm used to compute the spacing between points.
#' @returns A data frame with columns `x` and `y`.
#' @export
curve_involute <- function(n, scale = 1, base = exp(1)) {
  stopifnot(
    scale >= 0
  )
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  ret <- data.frame(
    x = scale * (cos(theta) + theta * sin(theta)),
    y = scale * (sin(theta) - theta * cos(theta))
  )
  entbl(ret)
}
