#' Cast a vector into a native raster
#'
#' Casts a vector of pixel values into a native raster.
#' The vector must be of length `width * height`
#'
#' @param x A vector to be cast into a native raster.
#' @param width,height Integer scalars giving the width and height of the image.
#' @param ... Additional arguments.
#' @returns A `nativeRaster` object.
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
