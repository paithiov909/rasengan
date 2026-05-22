#' Methods for chakra objects
#'
#' @description
#' A `chakra` in rasengan is an object that evolves over time and can be
#' observed as ordinary R data.
#'
#' The methods `observe()`, `circulate()`, and `reset_state()` are generic
#' functions that dispatch on the class of `x`.
#' The name `chakra` is used as a conceptual interface rather than as a
#' concrete S3 class.
#'
#' @details
#' Chakra objects may be implemented in different ways.
#'
#' Some chakras are mutable external states backed by external pointers.
#' For these objects, `circulate()` may update the underlying state in place.
#'
#' Other chakras are pseudo-states. A pseudo-state stores enough parameters to
#' compute its current observation, but does not necessarily mutate an external
#' pointer. For these objects, `circulate()` usually returns a modified copy of
#' `x`.
#'
#' Because implementations may differ, users should generally assign the return
#' value of `circulate()`:
#'
#' ```
#' x <- circulate(x, dt = 1 / 60)
#' observe(x)
#' ```
#'
#' The following generics are available:
#'
#' * `observe(x, ...)`: Returns the current state as a tibble.
#' * `circulate(x, dt, n_steps, ...)`: Evolves `x` for `n_steps`.
#' * `reset_state(x, ...)`: Resets `x` to an initial or specified state, when
#'   supported.
#'
#' Not all chakra implementations need to support meaningful reset behavior.
#' For pseudo-states whose state is represented directly by an R object, it may
#' be simpler to recreate the object instead of calling `reset_state()`.
#'
#' @param x A chakra-like object.
#' @param dt A numeric scalar giving the time step.
#' @param n_steps An integer scalar giving the number of steps to advance.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' * `observe()` returns a tibble representing the current state.
#' * `circulate()` returns the evolved object. Some methods may also mutate the
#'  underlying state in place.
#' * `reset_state()` returns the reset object, when supported.
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
