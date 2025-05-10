#' Convert degrees and radians
#'
#' @param deg Degrees.
#' @param rad Radians.
#' @returns A numeric vector.
#' @rdname deg-rad
#' @name deg-rad
NULL

#' @rdname deg-rad
#' @export
deg2rad <- function(deg) {
  deg * (pi / 180)
}

#' @rdname deg-rad
#' @export
rad2deg <- function(rad) {
  rad * (180 / pi)
}

#' Argument matching helper
#'
#' @param x Argument to match.
#' @param arg Argument name.
#' @param values Possible values.
#' @returns Integer.
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
