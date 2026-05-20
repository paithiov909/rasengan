#' Generate geometric curves
#'
#' @description
#' Generates a data frame of points along a geometric curve.
#'
#' `"curve"` in rasengan is a generic term for a set of functions
#' that take parameters `n` and generate a data frame
#' with `n` rows and columns containing `x` and `y` along the geometric curve.
#'
#' ## Archimedean spirals
#' `curve_archimedean` generates a 2D curve that is a general Archimedean spiral
#' where the raidus `r` is defined as follows:
#' \deqn{
#' r = a + b \cdot \theta^\frac{1}{c}
#' }
#'
#' According to the [Wikipedia article](https://en.wikipedia.org/wiki/Archimedean_spiral),
#' the normal Archimedean spiral occurs when `c = 1`.
#' Other spirals falling into this group include
#' the hyperbolic spiral (`c = -1`),
#' Fermat's spiral (`c = 2`), and the lituus (`c = −2`).
#'
#' @param n An integer scalar; Number of points to sample along the curve.
#' @param a,b,c,d,e,k Numeric scalars; Parameters of the curve.
#' @param or,ir Numeric scalars; Outer and inner radius.
#' @param base A numeric scalar;
#'  Base of the logarithm used to compute the spacing between points.
#' @param scale A numeric scalar; Scaling factor for the curve.
#' @returns A tibble with columns `id`, `phi` or `theta`, `x`, and `y`.
#' @family curve
#' @rdname curve-others
#' @name curve-others
NULL

#' @rdname curve-others
#' @export
curve_archimedean <- function(n, a = 0, b = 1, c = 1, base = exp(1)) {
  phi <- seq(0, by = pi / log(n, base = base), length.out = n)
  zz <- complex(modulus = a + b * phi^(1 / c), argument = phi)
  dplyr::tibble(id = seq_len(n), phi = Arg(zz), x = Re(zz), y = Im(zz))
}

#' @rdname curve-others
#' @export
curve_cyclic_harmonic <- function(
  n,
  k = 5,
  a = 1,
  b = 0.5,
  scale = 1,
  base = exp(1)
) {
  phi <- seq(0, by = pi / log(n, base = base), length.out = n)
  zz <- complex(modulus = scale * (a + b * cos(k * phi)), argument = phi)
  dplyr::tibble(id = seq_len(n), phi = Arg(zz), x = Re(zz), y = Im(zz))
}

#' @rdname curve-others
#' @export
curve_epicycloid <- function(n, or = 16, ir = 3.5, scale = 1, base = exp(1)) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * ((or + ir) * cos(theta) - ir * cos((or + ir) / ir * theta)),
    y = scale * ((or + ir) * sin(theta) - ir * sin((or + ir) / ir * theta))
  )
}

#' @rdname curve-others
#' @export
curve_epitrochoid <- function(
  n,
  or = 16,
  ir = 3.5,
  b = 2.4,
  scale = 1,
  base = exp(1)
) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * ((or + ir) * cos(theta) - b * cos((or + ir) / ir * theta)),
    y = scale * ((or + ir) * sin(theta) - b * sin((or + ir) / ir * theta))
  )
}

#' @rdname curve-others
#' @export
curve_gear <- function(n, k = 10, a = 1, b = 16, scale = 1, base = exp(1)) {
  phi <- seq(0, by = pi / log(n, base = base), length.out = n)
  zz <- complex(
    modulus = scale * (a + 1 / b * tanh(b * sin(k * phi))),
    argument = phi
  )
  dplyr::tibble(id = seq_len(n), phi = Arg(zz), x = Re(zz), y = Im(zz))
}

#' @rdname curve-others
#' @export
curve_heart <- function(n, scale = -1) {
  theta <- seq(0, 2 * pi, length.out = n)
  # fmt: skip
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * 16 * sin(theta)^3,
    y = scale * (13 * cos(theta) - 5 * cos(2 * theta) - 2 * cos(3 * theta) - cos(4 * theta)) # nolint
  )
}

#' @rdname curve-others
#' @export
curve_hypocycloid <- function(n, or = 16, ir = 3.5, scale = 1, base = exp(1)) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * ((or - ir) * cos(theta) + ir * cos((or - ir) / ir * theta)),
    y = scale * ((or - ir) * sin(theta) - ir * sin((or - ir) / ir * theta))
  )
}

#' @rdname curve-others
#' @export
curve_hypotrochoid <- function(
  n,
  or = 16,
  ir = 3.5,
  b = 2.4,
  scale = 1,
  base = exp(1)
) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * ((or - ir) * cos(theta) + b * cos((or - ir) / ir * theta)),
    y = scale * ((or - ir) * sin(theta) - b * sin((or - ir) / ir * theta))
  )
}

#' @rdname curve-others
#' @export
curve_involute <- function(n, scale = 1, base = exp(1)) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * (cos(theta) + theta * sin(theta)),
    y = scale * (sin(theta) - theta * cos(theta))
  )
}

#' @rdname curve-others
#' @export
curve_lissajous <- function(n, d = 10, e = 16, scale = 1, base = exp(1)) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * cos(d * theta),
    y = scale * sin(e * theta)
  )
}

#' @rdname curve-others
#' @export
curve_ranunculoid <- function(n, k = 6, scale = 1 / k, base = exp(1)) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * (k * cos(theta) - cos(k * theta)),
    y = scale * (k * sin(theta) - sin(k * theta))
  )
}

#' @rdname curve-others
#' @export
curve_rose <- function(n, k = 5, c = 1, scale = 1, base = exp(1)) {
  phi <- seq(0, by = pi / log(n, base = base), length.out = n)
  zz <- complex(modulus = scale * cos(k * phi / c), argument = phi)
  dplyr::tibble(id = seq_len(n), phi = Arg(zz), x = Re(zz), y = Im(zz))
}

#' @rdname curve-others
#' @export
curve_spirograph <- function(
  n,
  or = 16,
  ir = 3.5,
  b = 2.4,
  scale = 1,
  base = exp(1)
) {
  theta <- seq(0, by = pi / log(n, base = base), length.out = n)
  dplyr::tibble(
    id = seq_len(n),
    theta = theta,
    x = scale * ((or - ir) * cos(theta) + b * cos((or - ir) / ir * theta)),
    y = scale * ((or - ir) * sin(theta) - b * sin((or - ir) / ir * theta))
  )
}
