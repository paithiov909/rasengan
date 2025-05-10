#' Create a 1D fractional Brownian motion generator
#'
#' @param power Integer.
#' @param hurst_index Numeric in range `(0, 1)`.
#' @returns
#' A function that takes a function `func` as its first argument
#' and returns a time series.
#' `func` is expected to take `n` as its first argument
#' and return a numeric vector of length `n`.
#' @export
#' @examples
#' fbm_from()(runif, min = -1, max = 1)
fbm_from <- function(power = 4, hurst_index = 0.5) {
  function(func, ...) {
    n <- 2^power
    z <- fbm_1d_cpp(hurst_index, func(n, ...))
    stats::ts(z, start = 1 / power, frequency = power)
  }
}

#' Create a 1D fractional Brownian bridge generator
#'
#' @param power Integer.
#' @param hurst_index Numeric in range `(0, 1)`.
#' @returns
#' A function that takes a function `func` as its first argument
#' and returns a time series.
#' `func` is expected to take `n` as its first argument
#' and return a numeric vector of length `n`.
#' @export
#' @examples
#' fbbridge_from()(runif, min = -1, max = 1)
fbbridge_from <- function(power = 4, hurst_index = 0.5) {
  function(func, ...) {
    n <- 2^power
    z <- fbr_1d_cpp(hurst_index, func(n, ...))
    stats::ts(z, start = 1 / power, frequency = power)
  }
}

#' Create a 2D fractional Brownian bridge generator
#'
#' @param power Integer.
#' @param hurst_index Numeric in range `(0, 1)`.
#' @returns
#' A function that takes a function `func` as its first argument
#' and returns a matrix.
#' `func` is expected to take `n` as its first argument
#' and return a numeric vector of length `n`.
#' @export
#' @examples
#' fbbridge_2d_from()(runif, min = -1, max = 1)
fbbridge_2d_from <- function(power = 4, hurst_index = 0.5) {
  function(func, ...) {
    n <- 2^power
    z <- fbr_2d_cpp(hurst_index, func(n^2, ...))
    matrix(z, ncol = n + 1)
  }
}
