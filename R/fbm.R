#' Generators for FBM and FBB
#'
#' Creates a function that generates a fractional Brownian motion (FBM) or
#' fractional Brownian bridge (FBB) time series with a given Hurst index.
#'
#' @param power Integer.
#' @param hurst_index Numeric in range `(0, 1)`.
#' @returns
#' A function that takes a function `func` as its first argument
#' and returns a time series (for `fbm_from()` and `fbbridge_from()`)
#' or a matrix (for `fbm_2d_from()`).
#' `func` is expected to take `n` as its first argument
#' and return a numeric vector of length `n`.
#' @rdname fbm
#' @name fbm
NULL

#' @rdname fbm
#' @export
fbm_from <- function(power = 4, hurst_index = 0.5) {
  function(func, ...) {
    n <- 2^power
    z <- fbm_1d_cpp(hurst_index, func(n, ...))
    stats::ts(z, start = 1 / power, frequency = power)
  }
}

#' @rdname fbm
#' @export
fbbridge_from <- function(power = 4, hurst_index = 0.5) {
  function(func, ...) {
    n <- 2^power
    z <- fbr_1d_cpp(hurst_index, func(n, ...))
    stats::ts(z, start = 1 / power, frequency = power)
  }
}

#' @rdname fbm
#' @export
fbbridge_2d_from <- function(power = 4, hurst_index = 0.5) {
  function(func, ...) {
    n <- 2^power
    z <- fbr_2d_cpp(hurst_index, func(n^2, ...))
    matrix(z, ncol = n + 1)
  }
}
