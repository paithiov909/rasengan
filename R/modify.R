#' Simply value modifications
#'
#' Some of these functions are copied
#' from the [ambient](https://github.com/thomasp85/ambient) package.
#'
#' @param x,y A numeric vector.
#' @param mask A numeric scalar, typically between `0` and `1`.
#' @param from A numeric vector of length 2.
#'  The range of `x` to use for normalization.
#' @param to A numeric vector of length 2. The output domain to normalize to.
#' @param lower,upper A numeric scalar. The lower and upper bounds to cap to.
#' @returns A numeric vector.
#'
#' @rdname modifications
#' @name modifications
NULL

#' @rdname modifications
#' @export
blend <- function(x, y, mask) {
  mask <- cap(mask)
  x * mask + y * (1 - mask)
}

#' @rdname modifications
#' @export
normalize <- function(x, from = range(x), to = c(0, 1)) {
  x <- (x - from[1]) / (from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}

#' @rdname modifications
#' @export
cap <- function(x, lower = 0, upper = 1) {
  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}

#' @noRd
step <- function(x, mask) {
  as.numeric(x > mask)
}

#' @rdname modifications
#' @export
pulse <- function(x, mask) {
  x <- normalize(x)
  step(x, mask) - step(x, 1 - mask)
}
