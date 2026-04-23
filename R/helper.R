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

#' Ping-pong sequence
#'
#' Repeats `x` in a ping-pong fashion.
#'
#' @param x An object to repeat.
#' @param ... Arguments to be passed to methods.
#' @export
pingpong <- function(x, ...) {
  UseMethod("pingpong")
}

#' @export
pingpong.default <- function(x, ...) {
  c(x, rev(x[-length(x)]))
}

#' @export
pingpong.data.frame <- function(x, ...) {
  x[pingpong(seq_len(nrow(x))), , drop = FALSE]
}

#' Expand grid
#'
#' A thin wrapper for [expand.grid()] that returns a tibble
#' while converting numeric columns to double.
#'
#' @param ... Arguments to be passed to [expand.grid()].
#' @returns A tibble.
#' @export
expand <- function(...) {
  expand.grid(..., stringsAsFactors = FALSE) |>
    lapply(function(x) if (is.numeric(x)) as.double(x) else x) |>
    dplyr::as_tibble()
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
    cli::cli_abort(
      "`{arg}` must be one of {paste0(values, collapse = ', ')}. Got '{x}'.",
      call = rlang::caller_env()
    )
  }
  tmp
}
