#' @export
as_pattern <- function(x, width, height, ...) {
  UseMethod("as_pattern")
}

#' @export
as_pattern.numeric <- function(x, width, height, ...) {
  if (length(x) != prod(width, height, na.rm = FALSE)) {
    cli::cli_abort("Input must be of length {prod(width, height)}")
  }
  structure(
    as.integer(x),
    class = "nativeRaster",
    dim = c(height, width)
  )
}
