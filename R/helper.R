#' Miscellaneous functions
#'
#' @param x A numeric vector.
#' @param origin A numeric vector of length 2.
#' @returns A numeric vector.
#' @rdname misc
#' @name misc
NULL

#' @rdname misc
#' @export
deg2rad <- function(x) {
  x * (pi / 180)
}

#' @rdname misc
#' @export
rad2deg <- function(x) {
  x * (180 / pi)
}

#' @rdname misc
#' @export
fract <- function(x) {
  x - floor(x)
}

#' @rdname misc
#' @export
mag <- function(x, origin = c(0, 0)) {
  sqrt(rowSums((x - origin)^2))
}

#' Circular shift
#'
#' @param x Any type of vector.
#' @param k An integer to shift `x` by.
#' @importFrom utils head tail
#' @export
shift <- function(x, k) {
  k <- as.integer(k %% length(x))
  if (identical(k, 0L)) {
    return(x)
  }
  c(tail(x, -k), head(x, k))
}

#' Argument matching helper
#'
#' @param x Argument to match.
#' @param arg Argument name.
#' @param values Possible values.
#' @returns An integer scalar.
#' @keywords internal
#' @noRd
int_match <- function(x, arg, values) {
  tmp <- match(x[1], values) - 1L
  if (is.na(tmp)) {
    msg <- glue::glue(
      "`{arg}` must be one of {paste0(values, collapse = ', ')}. Got '{x}'."
    )
    rlang::abort(msg)
  }
  tmp
}
