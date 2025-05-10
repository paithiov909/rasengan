#' Simply value modifications
#'
#' These functions are copied from the [ambient](https://github.com/thomasp85/ambient) package.
#'
#' @param x,y Values to modify
#' @param mask A vector of the same length as `x` and `y`. Assumed to be between
#' 0 and 1 (values outside of this range is capped). The closer to 1 the more of
#' `x` will be used and the closer to 0 the more of `y` will be used
#' @param from The range of `x` to use for normalisation
#' @param to The output domain to normalise to
#' @param lower,upper The lower and upper bounds to cap to
#'
#' @name modifications
#' @rdname modifications
#' @export
blend <- function(x, y, mask) {
  mask <- cap(mask)
  x * mask + y * (1 - mask)
}

#' @rdname modifications
#' @export
normalise <- function(x, from = range(x), to = c(0, 1)) {
  x <- (x - from[1]) / (from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}
#' @rdname modifications
#' @export
normalize <- normalise

#' @rdname modifications
#' @export
cap <- function(x, lower = 0, upper = 1) {
  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}
