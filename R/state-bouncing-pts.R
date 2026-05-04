#' Generate a state of bouncing points
#'
#' Generates a state of `n` bouncing points.
#'
#' @param n An integer scalar; Number of bouncing points.
#' @param bbox Numeric vector of length 4;
#'  The bounding box of the bouncing points as `c(xmin, ymin, xmax, ymax)`.
#' @param speed Numeric scalar; The speed factor of the bouncing points.
#' @param restitution Numeric scalar; The restitution of the bouncing points.
#' @param seed Numeric scalar; Random seed for reproducibility.
#' @returns
#'  An external pointer that represents the state of `n` bouncing points.
#' @export
state_bouncing_pts <- function(
  n,
  bbox = c(-1, -1, 1, 1),
  speed = 1,
  restitution = 1,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  x <- runif(n, bbox[1], bbox[3])
  y <- runif(n, bbox[2], bbox[4])

  angle <- runif(n, -pi, pi)
  vx <- cos(angle) * speed
  vy <- sin(angle) * speed

  exptr <-
    new_bouncing_pts_cpp(
      x = x,
      y = y,
      vx = vx,
      vy = vy,
      xmin = bbox[1],
      xmax = bbox[3],
      ymin = bbox[2],
      ymax = bbox[4],
      restitution = restitution
    )
  structure(exptr, class = c("bouncing_pts", "chakra", class(exptr)))
}

#' @export
observe.bouncing_pts <- function(x, ...) {
  out <- bouncing_pts_as_list_cpp(x)
  dplyr::as_tibble(out)
}

#' @export
circulate.bouncing_pts <- function(x, dt = 1.0, n_steps = 1, ...) {
  proceed_bouncing_pts_cpp(x, dt, n_steps)
  invisible(x)
}

#' @export
reset_state.bouncing_pts <- function(x, ...) {
  reset_bouncing_pts_cpp(x)
  invisible(x)
}
