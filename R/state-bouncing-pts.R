#' Generate a state for bouncing points
#'
#' Generates a chakra state for bouncing points.
#'
#' @param seeds A data frame containing the initial point positions.
#' @param bbox Numeric vector of length 4;
#'  The bounding box of the bouncing points as `c(xmin, ymin, xmax, ymax)`.
#' @param restitution Numeric scalar; The restitution of the bouncing points.
#' @param x,y,vx,vy <[`data-masking`][rlang::args_data_masking]>
#'  Expressions specifying data columns for the initial state.
#' @returns An external pointer.
#' @seealso chakra
#' @family state
#' @export
#' @examples
#' seeds <-
#'  dplyr::tibble(
#'    x = runif(10, -1, 1),
#'    y = runif(10, -1, 1),
#'    angle = runif(10, -pi, pi),
#'    vx = cos(angle),
#'    vy = sin(angle)
#'  )
#'
#' state <- state_bouncing_pts(seeds)
#'
#' observe(state)
#'
#' circulate(state) |>
#'  observe()
state_bouncing_pts <- function(
  seeds,
  bbox = c(-1, -1, 1, 1),
  restitution = 1,
  x = x,
  y = y,
  vx = vx,
  vy = vy
) {
  if (!all(is.finite(bbox))) {
    cli::cli_abort("`bbox` must be finite numerics.")
  }
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  vx <- rlang::enquo(vx)
  vy <- rlang::enquo(vy)

  seeds <- seeds |>
    dplyr::mutate(
      x = cap({{ x }}, bbox[1], bbox[3]),
      y = cap({{ y }}, bbox[2], bbox[4]),
      vx = {{ vx }},
      vy = {{ vy }}
    )

  exptr <-
    new_bouncing_pts_cpp(
      x = seeds[["x"]],
      y = seeds[["y"]],
      vx = seeds[["vx"]],
      vy = seeds[["vy"]],
      xmin = bbox[1],
      xmax = bbox[3],
      ymin = bbox[2],
      ymax = bbox[4],
      restitution = restitution
    )
  structure(exptr, class = c("chakra_bouncing_pts", "chakra", class(exptr)))
}

#' @export
observe.chakra_bouncing_pts <- function(x, ...) {
  out <- bouncing_pts_as_list_cpp(x)
  dplyr::as_tibble(out)
}

#' @export
circulate.chakra_bouncing_pts <- function(x, dt = 1.0, n_steps = 1, ...) {
  proceed_bouncing_pts_cpp(x, dt, n_steps)
  invisible(x)
}

#' @export
reset_state.chakra_bouncing_pts <- function(x, ...) {
  reset_bouncing_pts_cpp(x)
  invisible(x)
}
