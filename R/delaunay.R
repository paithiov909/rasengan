#' Compute Delaunay triangulation
#'
#' Computes the Delaunay triangulation of a set of 2D points.
#'
#' @param seeds A data frame containing the input points.
#' @param x,y <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns in `seeds`
#'   that contain the x- and y-coordinates.
#'
#' @returns A list with two data frames:
#'   * `circumcenters`: A tibble containing the triangle id, the x- and
#'   y-coordinates of the circumcenter, and the circumradius.
#'   * `vertices`: A tibble containing the triangle id and the x- and
#'   y-coordinates of the three vertices of each triangle.
#'
#' @details
#' This function returns the triangulation in a form that is convenient for
#' drawing. `vertices` stores the triangle vertices in long format, with three
#' rows per triangle. `circumcenters` stores the corresponding circumcircle of
#' each triangle.
#'
#' @examples
#' theta <- seq(-pi, pi, length.out = 12)
#' seeds <- data.frame(
#'   x = c(150 * cos(theta), 0),
#'   y = c(150 * sin(theta), 0)
#' )
#'
#' tri <- delaunay(seeds)
#'
#' tri$circumcenters
#' tri$vertices
#'
#' @export
delaunay <- function(seeds, x = x, y = y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  ret <- seeds |>
    dplyr::select({{ x }}, {{ y }}) |>
    dplyr::distinct() |>
    as.matrix() |>
    delaunay_cpp()

  circumcenters <-
    dplyr::tibble(
      id = ret[["idx"]],
      x = ret[["cc_x"]],
      y = ret[["cc_y"]],
      radius = ret[["radii"]]
    )
  vertices <-
    dplyr::tibble(
      id = rep(ret[["idx"]], each = 3),
      x = ret[["vert_x"]],
      y = ret[["vert_y"]]
    )

  list(
    circumcenters = circumcenters,
    vertices = vertices
  )
}
