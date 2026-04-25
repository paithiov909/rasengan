#' Methods for chakras
#'
#' @description
#' A `chakra` in rasengan is a mutable external state (an external pointer)
#' that evolves over time and can be observed as ordinary R data.
#'
#' The methods `observe`, `circulate`, and `reset_state` are generic
#' functions that dispatch on the class of `x`.
#' `chakra` is a dummy S3 class that does nothing for actual dispatch.
#'
#' @details
#' The following generics are available:
#'
#' * `observe(x, ...)`: Returns the current state as a tibble.
#' * `circulate(x, dt, n_steps, ...)`: Evolves the chakra for `n_steps`.
#' * `reset_state(x, ...)`: Resets `x` to its initial state.
#'
#' @param x An external pointer.
#' @param dt A numeric scalar; Time step.
#' @param n_steps An integer scalar; Number of steps to simulate.
#' @param ... Additional arguments to be passed to the underlying function.
#' @returns
#'   * for `observe`: A tibble that represents the current state.
#'   * for `circulate`: Invisibly returns `x` itself.
#'   * for `reset_state`: Invisibly returns `x` itself.
#'
#' @rdname chakra-methods
#' @name chakra
NULL

#' @rdname chakra-methods
#' @export
observe <- function(x, ...) {
  UseMethod("observe")
}

#' @rdname chakra-methods
#' @export
circulate <- function(x, dt = 1.0, n_steps = 1, ...) {
  UseMethod("circulate")
}

#' @rdname chakra-methods
#' @export
reset_state <- function(x, ...) {
  UseMethod("reset_state")
}
