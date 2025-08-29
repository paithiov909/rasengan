#' @noRd
.onUnload <- function(libpath) {
  library.dynam.unload("rasengan", libpath)
}
