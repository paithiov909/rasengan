#' Create a state for orbital motion
#'
#' @description
#' Creates a pseudo-chakra state in which points revolve around a common
#' origin with constant angular velocity. Initial phases and orbital radii are
#' inferred from the input coordinates.
#'
#' @details
#' The initial positions are interpreted as points on circular orbits around
#' `origin`. For each point, the orbital radius and initial phase are computed
#' internally from the supplied coordinates.
#'
#' The resulting state stores:
#'
#' - orbital radius
#' - initial phase
#' - angular velocity
#' - current time
#'
#' Coordinates at the current time can be retrieved with [observe()].
#'
#' @param seeds A data frame containing the initial point positions.
#' @param origin Numeric vector of length 2 giving the center of rotation.
#' @param x,y <[`data-masking`][rlang::args_data_masking]> Coordinates used as
#'  the initial point positions.
#' @param omega <[`data-masking`][rlang::args_data_masking]>
#'  Angular velocity. May be either a scalar or a column evaluated
#'  in `seeds`.
#' @param id <[`data-masking`][rlang::args_data_masking]>
#'  Identifier for each orbit. Defaults to row numbers.
#' @returns A list.
#' @export
#' @seealso chakra
#' @family state
#' @examples
#' seeds <-
#'   curve_epicycloid(16) |>
#'   dplyr::mutate(
#'     omega = seq(-1, 1, length.out = dplyr::n())
#'   )
#'
#' state <-
#'   state_orbits(
#'     seeds,
#'     x = x,
#'     y = y,
#'     omega = omega
#'   )
#'
#' observe(state)
#'
#' state |>
#'   circulate(dt = pi / 4) |>
#'   observe()
state_orbits <- function(
  seeds,
  origin = c(0, 0),
  x = x,
  y = y,
  omega = 1,
  id = dplyr::row_number()
) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  omega <- rlang::enquo(omega)
  id <- rlang::enquo(id)

  seeds <- seeds |>
    dplyr::mutate(
      id = {{ id }},
      dx = {{ x }} - origin[1],
      dy = {{ y }} - origin[2],
      omega = {{ omega }}
    )

  structure(
    list(
      id = seeds[["id"]],
      radius = sqrt((seeds[["dx"]])^2 + (seeds[["dy"]])^2),
      phase = atan2(seeds[["dy"]], seeds[["dx"]]),
      omega = seeds[["omega"]],
      origin = origin[1:2],
      t = 0
    ),
    class = c("chakra_orbits", "pseudo_chakra")
  )
}

#' @export
observe.chakra_orbits <- function(x, ...) {
  angle <- x[["phase"]] + x[["omega"]] * x[["t"]]
  dplyr::tibble(
    id = .env$x[["id"]],
    x = .env$x[["origin"]][1] + .env$x[["radius"]] * cos(angle),
    y = .env$x[["origin"]][2] + .env$x[["radius"]] * sin(angle)
  )
}

#' @export
circulate.chakra_orbits <- function(x, dt = 1.0, n_steps = 1, ...) {
  purrr::modify_at(x, "t", ~ .x + dt * n_steps)
}
