#' Smoothing functions
#'
#' @param t A numeric vector.
#' @param x1,y1,x2,y2 Numeric scalars for bezier easing.
#' @param type A string. The type of easing function to use.
#' @returns A numeric vector.
#' @rdname smoothing
#' @export
smoothstep <- function(t) {
  t * t * (3.0 - 2.0 * t)
}

#' @rdname smoothing
#' @export
smootherstep <- function(t) {
  t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
}

#' @rdname smoothing
#' @export
ease_bezier <- function(t, x1, y1, x2, y2) {
  t <- as.double(t)
  bezier_value_at_cpp(t, matrix(c(0, 0, x1, y1, x2, y2, 1, 1), ncol = 8))
}

#' @rdname smoothing
#' @export
ease_in <- function(
  t,
  type = c(
    "sine",
    "quad",
    "cubic",
    "quart",
    "quint",
    "exp",
    "circle",
    "elastic",
    "back",
    "bounce"
  )
) {
  type <- rlang::arg_match(type)
  t <- as.double(t)
  switch(
    type,
    sine = in_sine(t),
    quad = in_quad(t),
    cubic = in_cubic(t),
    quart = in_quart(t),
    quint = in_quint(t),
    exp = in_exp(t),
    circle = in_circ(t),
    elastic = in_elastic(t),
    back = in_back(t),
    bounce = in_bounce(t)
  )
}

#' @rdname smoothing
#' @export
ease_out <- function(
  t,
  type = c(
    "sine",
    "quad",
    "cubic",
    "quart",
    "quint",
    "exp",
    "circle",
    "elastic",
    "back",
    "bounce"
  )
) {
  type <- rlang::arg_match(type)
  t <- as.double(t)
  switch(
    type,
    sine = out_sine(t),
    quad = out_quad(t),
    cubic = out_cubic(t),
    quart = out_quart(t),
    quint = out_quint(t),
    exp = out_exp(t),
    circle = out_circ(t),
    elastic = out_elastic(t),
    back = out_back(t),
    bounce = out_bounce(t)
  )
}

#' @rdname smoothing
#' @export
ease_in_out <- function(
  t,
  type = c(
    "sine",
    "quad",
    "cubic",
    "quart",
    "quint",
    "exp",
    "circle",
    "elastic",
    "back",
    "bounce"
  )
) {
  type <- rlang::arg_match(type)
  t <- as.double(t)
  switch(
    type,
    sine = in_out_sine(t),
    quad = in_out_quad(t),
    cubic = in_out_cubic(t),
    quart = in_out_quart(t),
    quint = in_out_quint(t),
    exp = in_out_exp(t),
    circle = in_out_circ(t),
    elastic = in_out_elastic(t),
    back = in_out_back(t),
    bounce = in_out_bounce(t)
  )
}

#' Interpolate between two values
#'
#' Adapted from
#' [coolbutuseless/displease](https://github.com/coolbutuseless/displease).
#'
#' @param x1,x2 Numeric scalars.
#' @param col1,col2 Character scalars. The colors to interpolate between.
#' @param n `length.out` parameter for [base::seq()].
#' @param ease A function to ease the sequence.
#' @param colorspace A string. Color space in which to do the interpolation.
#'  See [farver::convert_colour()] for details.
#' @returns
#'  * For `seq_ease()`, a numeric vector.
#'  * For `seq_color()`, a character vector.
#' @rdname seq-ease
#' @export
seq_ease <- function(
  x1,
  x2,
  n = 100,
  ease = \(t) ease_in_out(t, "cubic")
) {
  t <- seq(0, 1, length.out = n)
  blend(x2, x1, ease(t))
}

#' @rdname seq-ease
#' @export
seq_color <- function(
  col1,
  col2,
  n = 100,
  ease = \(t) ease_in_out(t, "cubic"),
  colorspace = "hcl"
) {
  if (!requireNamespace("farver", quietly = TRUE)) {
    rlang::abort(glue::glue("The farver package is required."))
  }
  rgb <- farver::decode_colour(c(col1, col2))
  cs <- farver::convert_colour(rgb, from = "rgb", to = colorspace)

  columns <- lapply(
    seq_len(ncol(cs)),
    function(idx) {
      seq_ease(cs[1, idx], cs[2, idx], n = n, ease = ease)
    }
  )
  do.call(cbind, columns) |>
    farver::convert_colour(from = colorspace, to = "rgb") |>
    farver::encode_colour()
}
