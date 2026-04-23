#' Coerce an object to a field function
#'
#' @description
#' `as_field()` converts various representations of a vector field or
#' field-like object into a *field function* (a function of class
#' `"field_fn"`). A field function takes a seed point and tracing
#' parameters, and returns a trajectory as a data frame.
#'
#' The returned function must have the signature:
#' `function(seed, n_steps, step_size, params)`, where `seed` is a data frame
#' with columns `x` and `y`, and the return value is a data frame with
#' columns `step`, `x`, and `y`.
#'
#' @details
#' For data frame inputs, `as_field()` constructs a simple trajectory
#' generator that iteratively advances a point by looking up the nearest
#' vector `(vx, vy)` and applying a fixed step size.
#'
#' More advanced behaviors (e.g., vectorized evaluation, custom integration
#' schemes, or compiled implementations) can be achieved by supplying a
#' user-defined function directly.
#'
#' @param field An object representing a field. Supported inputs include:
#'  * A function with signature `function(seed, n_steps, step_size, params)`, returning a trajectory.
#'  * A data frame with columns `x`, `y`, `vx`, and `vy`, interpreted as a discrete vector field.
#'
#' @returns A function of class `"field_fn"` that generates trajectories
#'  from seed points.
#'
#' @seealso [trace_field()]
#' @export
#' @examples
#' # Using a custom field function
#' f <- function(seed, n_steps, step_size, params) {
#'   x <- numeric(n_steps)
#'   y <- numeric(n_steps)
#'   cur <- as.matrix(seed[, c("x", "y"), drop = FALSE])
#'   for (i in seq_len(n_steps)) {
#'     x[i] <- cur[, 1]
#'     y[i] <- cur[, 2]
#'     cur <- cur + step_size * c(1, 0)
#'   }
#'   data.frame(step = seq_len(n_steps), x = x, y = y)
#' }
#'
#' as_field(f)
#'
#' # Using a data frame as a vector field
#' df <- data.frame(
#'   x = runif(10),
#'   y = runif(10),
#'   vx = runif(10, -1, 1),
#'   vy = runif(10, -1, 1)
#' )
#'
#' as_field(df)
as_field <- function(field) {
  UseMethod("as_field")
}

#' @export
as_field.default <- function(field) {
  cli::cli_abort("There is no 'as_field' method for class {class(field)}")
}

#' @export
as_field.function <- function(field) {
  if (
    !identical(
      c("seed", "n_steps", "step_size", "params"),
      names(formals(field))
    )
  ) {
    cli::cli_abort(
      "`field` must take arguments 'seed', 'n_steps', 'step_size', and 'params'."
    )
  }
  structure(field, class = "field_fn")
}

#' @export
as_field.data.frame <- function(field) {
  if (!all(c("x", "y", "vx", "vy") %in% names(field))) {
    cli::cli_abort(
      "`field` must contain columns named 'x', 'y', 'vx', and 'vy'."
    )
  }
  f <- function(seed, n_steps, step_size, params) {
    pts <- field[, c("x", "y"), drop = FALSE]
    step <- x <- y <- numeric(n_steps)
    cur_point <- as.matrix(seed[, c("x", "y"), drop = FALSE])
    for (idx in seq_len(n_steps)) {
      step[idx] <- idx
      x[idx] <- cur_point[, 1]
      y[idx] <- cur_point[, 2]
      nn <-
        rasengan::mag(pts, origin = cur_point) |>
        which.min()
      v <- field[nn, c("vx", "vy"), drop = FALSE]
      cur_point <- cur_point + step_size * as.double(v)
    }
    data.frame(step = step, x = x, y = y)
  }
  as_field(f)
}

# To make R CMD check happy
utils::globalVariables("params")

#' Trace trajectories through a field
#'
#' Generates trajectories from a set of seed points using
#' a field function. Each seed is passed to the field function, which is
#' responsible for producing a trajectory of length `n_steps`.
#'
#' @details
#' `trace_field()` standardizes the input `field` via [as_field()] and
#' applies it to each seed independently. The field function is expected
#' to return a data frame describing the trajectory for a single seed.
#'
#' The design allows flexible implementations of field behavior, including
#' vectorized or compiled trajectory generators.
#'
#' @param seeds A data frame containing seed points.
#' @param field A field object. This can be:
#'  * A function with signature `function(seed, n_steps, step_size, params)`, or
#'  * Any object supported by [as_field()].
#' @param x,y <[`data-masking`][rlang::args_data_masking]> Expressions
#'  specifying the x and y coordinates of the seeds.
#' @param n_steps Integer scalar giving the number of steps in each
#'  trajectory.
#' @param step_size Numeric scalar controlling the step size used by the
#'  field function.
#' @param ... Additional arguments passed to the field function
#'  as a named list `params`.
#'
#' @returns A tibble with columns:
#'  * `seed`: integer identifier for each input seed
#'  * `step`: step index within each trajectory
#'  * `x`, `y`: coordinates of the trajectory
#'
#' @export
#' @examples
#' seeds <- data.frame(
#'   x = runif(5),
#'   y = runif(5)
#' )
#'
#' field <- function(seed, n_steps, step_size, params) {
#'   x <- numeric(n_steps)
#'   y <- numeric(n_steps)
#'   cur <- as.matrix(seed[, c("x", "y"), drop = FALSE])
#'   for (i in seq_len(n_steps)) {
#'     x[i] <- cur[, 1]
#'     y[i] <- cur[, 2]
#'     cur <- cur + step_size * c(0.1, 0.1)
#'   }
#'   data.frame(step = seq_len(n_steps), x = x, y = y)
#' }
#'
#' trace_field(seeds, field, x = x, y = y, n_steps = 10, step_size = 0.1)
trace_field <- function(
  seeds,
  field,
  x = x,
  y = y,
  n_steps = 10,
  step_size = 1,
  ...
) {
  if (!inherits(field, "field_fn")) {
    field <- as_field(field)
  }
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  ret <- seeds |>
    dplyr::mutate(
      x = {{ x }},
      y = {{ y }},
      .keep = "unused"
    ) |>
    rlang::as_function(~ split(., seq_len(nrow(.))))() |>
    purrr::imap(
      purrr::in_parallel(
        function(seed, .seed_idx) {
          rlang::try_fetch(
            field(seed, n_steps, step_size, params), # nolint
            error = function(e) {
              cli::cli_abort(
                "'field_fn' failed for seed at {.val .seed_idx}.",
                parent = e
              )
            }
          )
        },
        field = field,
        n_steps = n_steps,
        step_size = step_size,
        params = rlang::list2(...)
      )
    ) |>
    unname() |> # remove names, otherwise "seed" becomes characters
    purrr::list_rbind(names_to = "seed")

  dplyr::as_tibble(ret)
}
