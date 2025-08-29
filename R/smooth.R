#' Smoothing functions
#'
#' @param t A numeric vector.
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
