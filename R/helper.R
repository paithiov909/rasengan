#' Miscellaneous functions
#'
#' @param x A numeric vector.
#' @param mat A numeric matrix or a data frame.
#' @param origin A numeric vector to be subtracted from `mat`.
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
mag <- function(mat, origin = c(0, 0)) {
  sqrt(rowSums((mat - origin)^2))
}

#' @rdname misc
#' @export
pingpong <- function(x) {
  c(x, rev(x[-length(x)]))
}

#' Expand grid
#'
#' A thin wrapper for [expand.grid()] that returns a tibble.
#'
#' @param ... Arguments to be passed to [expand.grid()].
#' @returns A tibble.
#' @export
expand <- function(...) {
  entbl(expand.grid(..., stringsAsFactors = FALSE))
}

#' Argument matching helper
#'
#' @param x Argument to match.
#' @param arg Argument name.
#' @param values Possible values.
#' @returns An integer scalar.
#' @noRd
int_match <- function(x, arg, values) {
  tmp <- match(x[1], values) - 1L
  if (is.na(tmp)) {
    msg <- glue::glue(
      "`{arg}` must be one of {paste0(values, collapse = ', ')}. Got '{x}'."
    )
    rlang::abort(msg, call = rlang::caller_env())
  }
  tmp
}

#' Convert a matrix into a 'tbl_df'
#'
#' @param x A matrix.
#' @returns A data frame.
#' @noRd
entbl <- function(x) {
  x <- as.data.frame(x)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}
