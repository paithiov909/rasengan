# To make R CMD check happy
utils::globalVariables("params")

#' Trace trajectories through a flow
#'
#' @description
#' Generates trajectories from a set of seed points using a
#' user-supplied *flow function*. Each seed is passed to `flow_fn`, which is
#' responsible for producing a trajectory of length `n_steps`.
#'
#' @details
#' `trace_flow()` applies `flow_fn` independently to each seed group. The
#' grouping is defined by the `id` argument. For each group, `flow_fn` is
#' called once and is expected to return a full trajectory.
#'
#' The design places full control of trajectory generation in `flow_fn`,
#' allowing implementations that are vectorized, iterative, or backed by
#' compiled code. This makes it possible to express a wide range of flow-like
#' behaviors, including vector fields, noise-driven updates, and custom
#' dynamical systems.
#'
#' Evaluation of seeds is parallelized via [purrr::in_parallel()].
#'
#' @param seeds A data frame containing seed points.
#' @param flow_fn A function that generates trajectories. It must have the
#'   signature `function(seed, n_steps, step_size, params)`, where:
#'
#'   * `seed` is a data frame containing the rows corresponding to
#'      a single seed group,
#'   * `n_steps` is an integer giving the number of steps,
#'   * `step_size` is a numeric scalar controlling step size, and
#'   * `params` is a list of additional parameters.
#'
#'   The function must return a data frame with columns `step`, `x`, and `y`.
#' @param x,y <[`data-masking`][rlang::args_data_masking]> Expressions
#'   specifying the x and y coordinates of the seeds.
#' @param id <[`data-masking`][rlang::args_data_masking]> Expression
#'   specifying grouping of seeds. Each group is treated as a single seed
#'   and passed to `flow_fn`. Defaults to `row_number()`.
#' @param n_steps Integer scalar giving the number of steps in each trajectory.
#' @param step_size Numeric scalar controlling the step size passed to
#'   `flow_fn`.
#' @param ... Additional parameters passed to `flow_fn`. These are collected
#'   into a list and supplied as the `params` argument.
#'
#' @returns A tibble with columns:
#'   * `seed`: integer identifier for each seed group,
#'   * `step`: step index within each trajectory,
#'   * `x`, `y`: coordinates of the trajectory.
#'
#' @examples
#' # Simple flow: constant drift
#' flow_fn <- function(seed, n_steps, step_size, params) {
#'   x <- numeric(n_steps)
#'   y <- numeric(n_steps)
#'   cur <- as.matrix(seed[, c("x", "y"), drop = FALSE])
#'   for (i in seq_len(n_steps)) {
#'     x[i] <- cur[, 1]
#'     y[i] <- cur[, 2]
#'     cur <- cur + step_size * c(0.1, 0)
#'   }
#'   data.frame(step = seq_len(n_steps), x = x, y = y)
#' }
#'
#' seeds <- data.frame(
#'   x = runif(5),
#'   y = runif(5)
#' )
#'
#' trace_flow(seeds, flow_fn, x = x, y = y, n_steps = 20, step_size = 0.05)
#'
#' # Using additional parameters
#' flow_noise <- function(seed, n_steps, step_size, params) {
#'   x <- numeric(n_steps)
#'   y <- numeric(n_steps)
#'   cur <- as.matrix(seed[, c("x", "y"), drop = FALSE])
#'   for (i in seq_len(n_steps)) {
#'     x[i] <- cur[, 1]
#'     y[i] <- cur[, 2]
#'     cur[, 1] <- cur[, 1] +
#'       params$nx(cur[, 1], cur[, 2]) * step_size
#'     cur[, 2] <- cur[, 2] +
#'       params$ny(cur[, 1], cur[, 2]) * step_size
#'   }
#'   data.frame(step = seq_len(n_steps), x = x, y = y)
#' }
#'
#' trace_flow(
#'   seeds,
#'   flow_noise,
#'   x = x,
#'   y = y,
#'   n_steps = 50,
#'   step_size = 0.02,
#'   nx = function(x, y) sin(x + y),
#'   ny = function(x, y) cos(x - y)
#' )
#' @export
trace_flow <- function(
  seeds,
  flow_fn,
  x = x,
  y = y,
  id = dplyr::row_number(),
  n_steps = 10,
  step_size = 1,
  ...
) {
  if (
    !is.function(flow_fn) ||
      !identical(
        c("seed", "n_steps", "step_size", "params"),
        names(formals(flow_fn))
      )
  ) {
    cli::cli_abort(
      "`flow_fn` must be a function that takes 'seed', 'n_steps', 'step_size', and 'params'"
    )
  }
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  id <- rlang::enquo(id)

  ret <- seeds |>
    dplyr::mutate(
      x = {{ x }},
      y = {{ y }},
      .keep = "unused"
    ) |>
    dplyr::group_by({{ id }}) |>
    dplyr::group_split() |>
    purrr::map(
      purrr::in_parallel(
        function(seed) {
          flow_fn(seed, n_steps, step_size, params) # nolint
        },
        flow_fn = flow_fn,
        n_steps = n_steps,
        step_size = step_size,
        params = rlang::list2(...)
      )
    ) |>
    purrr::list_rbind(names_to = "seed")

  dplyr::as_tibble(ret)
}
