#' Reorder objects by cyclic shifts or scanning patterns
#'
#' @description
#' A collection of functions for reordering one-dimensional vectors or the rows
#' of data frames by simple index manipulations.
#'
#' These operations are designed for creative coding workflows,
#' where controlling the traversal order of grids or sequences is useful
#' for producing structured, semi-regular visual variations.
#'
#' @details
#' The following methods are available:
#'
#' * `rings(x, nrow, ncol)`:
#'  Reorders `x` or the rows of a data frame by scanning the grid in
#'  *concentric rings*: the outer border first, followed by progressively
#'  inner borders, until the center is reached.
#' * `rings_index(nrow, ncol)`:
#'  Returns the index vector that enumerates the cells of a grid in
#'  concentric rectangular rings from the outside inward.
#' * `shift(x, k)`:
#'  Performs a circular shift of the vector `x` by `k` positions.
#'  For data frames, rows are shifted instead of elements.
#' * `snake(x, nrow, ncol)`:
#'  Reorders `x` (or rows of a data frame) according to a *snake* (serpentine)
#'  traversal of a conceptual grid of size `nrow * ncol`.
#'  Odd-numbered rows are read left-to-right, even rows right-to-left.
#' * `snake_index(nrow, ncol)`:
#'  Returns only the index vector representing the snake traversal.
#' * `spiral(x, nrow, ncol)`:
#'  Reorders `x` or a data frame by following a spiral scan of a grid,
#'  starting from the outermost ring toward the center.
#' * `spiral_index(nrow, ncol)`:
#'  Computes the spiral traversal indices.
#' * `stride(x, step)`:
#'  Reorders `x` by interleaving elements with a stride of length `step`.
#' * `stride_index(n, step)`:
#'  Returns the index vector used by `stride()`.
#' * `zigzag(x, nrow, ncol)`:
#'  Reorders `x` (or rows of a data frame) according to a diagonal zigzag
#'  traversal of a conceptual `nrow * ncol` grid.
#'  Each diagonal where `i + j` is constant is processed together, with the
#'  direction alternating between forward and reverse.
#' * `zigzag_index(nrow, ncol)`:
#'  Returns the index vector for a zigzag traversal of a grid, following
#'  alternating diagonal scanlines (similar to JPEG DCT block ordering).
#'
#' These functions are lightweight and composable, and are intended for
#' preprocessing sequences or point grids in creative visualization workflows.
#'
#' @param x A vector or data frame to be reordered.
#' @param k An integer shift amount for `shift()`.
#' @param nrow,ncol Integers giving the conceptual number of rows and columns
#'  for grid-based scanning patterns.
#'  If one of them is `NULL`, it is inferred from the length of `x`.
#' @param byrow Logical; whether to interpret the implicit grid in row-major
#'  (`TRUE`) or column-major (`FALSE`) order.
#' @param step Integer stride size for `stride()` and `stride_index()`.
#' @param n An integer scalar; the length of the index vector to generate.
#' @returns
#' A reordered object of the same type as the input (`x`), or an integer vector
#' of indices for functions ending in `_index`.
#'
#' @rdname reorder
#' @name reorder
NULL

#' @rdname reorder
#' @export
rings_index <- function(nrow, ncol, byrow = FALSE) {
  mat <- matrix(seq_len(nrow * ncol), nrow, ncol, byrow = byrow)
  genidx_rings_cpp(mat)
}

#' @rdname reorder
#' @export
rings <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  UseMethod("rings")
}

#' @rdname reorder
#' @export
rings.default <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- length(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- rings_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len]]
}

#' @rdname reorder
#' @export
rings.data.frame <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- nrow(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- rings_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len], , drop = FALSE]
}

#' @rdname reorder
#' @export
shift <- function(x, k) {
  UseMethod("shift")
}

#' @rdname reorder
#' @export
shift.default <- function(x, k) {
  k <- as.integer(k %% length(x))
  if (identical(k, 0L)) {
    return(x)
  }
  c(tail(x, -k), head(x, k))
}

#' @rdname reorder
#' @export
shift.data.frame <- function(x, k) {
  k <- as.integer(k %% nrow(x))
  if (identical(k, 0L)) {
    return(x)
  }
  temp <- x
  x <- seq_len(nrow(temp))
  idx <- NextMethod()
  temp[idx, , drop = FALSE]
}

#' @rdname reorder
#' @export
snake_index <- function(nrow, ncol, byrow = FALSE) {
  base <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol, byrow = byrow)
  idx <- integer(nrow * ncol)
  k <- 1L
  for (i in seq_len(nrow)) {
    if (i %% 2L == 1L) {
      idx[k:(k + ncol - 1L)] <- base[i, ]
    } else {
      idx[k:(k + ncol - 1L)] <- rev(base[i, ])
    }
    k <- k + ncol
  }
  idx
}

#' @rdname reorder
#' @export
snake <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  UseMethod("snake")
}

#' @rdname reorder
#' @export
snake.default <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- length(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- snake_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len]]
}

#' @rdname reorder
#' @export
snake.data.frame <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- nrow(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- snake_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len], , drop = FALSE]
}

#' @rdname reorder
#' @export
spiral_index <- function(nrow, ncol, byrow = FALSE) {
  mat <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol, byrow = byrow)
  genidx_spiral_cpp(mat)
}

#' @rdname reorder
#' @export
spiral <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  UseMethod("spiral")
}

#' @rdname reorder
#' @export
spiral.default <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- length(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- spiral_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len]]
}

#' @rdname reorder
#' @export
spiral.data.frame <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- nrow(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- spiral_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len], , drop = FALSE]
}

#' @rdname reorder
#' @export
stride_index <- function(n, step = 2L) {
  if (step <= 1 || step >= n) {
    return(seq_len(n))
  }
  genidx_stride_cpp(n, step)
}

#' @rdname reorder
#' @export
stride <- function(x, step = 2L) {
  UseMethod("stride")
}

#' @rdname reorder
#' @export
stride.default <- function(x, step = 2L) {
  x[stride_index(length(x), step = step)]
}

#' @rdname reorder
#' @export
stride.data.frame <- function(x, step = 2L) {
  x[stride_index(nrow(x), step = step), , drop = FALSE]
}

#' @rdname reorder
#' @export
zigzag_index <- function(nrow, ncol, byrow = FALSE) {
  mat <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol, byrow = byrow)
  genidx_zigzag_cpp(mat)
}

#' @rdname reorder
#' @export
zigzag <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  UseMethod("zigzag")
}

#' @rdname reorder
#' @export
zigzag.default <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- length(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- zigzag_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len]]
}

#' @rdname reorder
#' @export
zigzag.data.frame <- function(x, nrow = NULL, ncol = NULL, byrow = FALSE) {
  if (rlang::is_empty(x)) {
    return(x)
  }
  len <- nrow(x)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- floor(sqrt(len))
    ncol <- ceiling(len / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(len / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(len / nrow)
  }
  idx <- zigzag_index(nrow = nrow, ncol = ncol, byrow = byrow)
  x[idx[idx <= len], , drop = FALSE]
}
