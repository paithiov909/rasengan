#' @noRd
vec3_normalize <- function(v) {
  mag <- sqrt(sum(v^2))
  v / mag
}

#' @noRd
vec3_cross <- function(a, b) {
  c(
    a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1]
  )
}

#' @noRd
as_trans3d <- function(m) {
  class(m) <- c("transform3d", "at_matrix", class(m))
  m
}

#' @keywords internal
#' @export
as_ndc_mul <- function(lhs, rhs) UseMethod("as_ndc_mul")

#' @keywords internal
#' @export
as_ndc_mul.default <- function(lhs, rhs) {
  if (all(lhs[, 4] != 0)) {
    lhs[, 1:3] <- lhs[, 1:3] / lhs[, 4]
  } else {
    rlang::warn(
      "Skipping division by w component because `lhs` has zero values."
    )
  }
  lhs %*% rhs
}

#' @keywords internal
#' @export
as_ndc_mul.transform3d <- function(lhs, rhs) {
  NextMethod()
}

#' NDC multiplication
#'
#' @param lhs A `transform3d` object.
#' @param rhs A `transform3d` object.
#' @returns A `transform3d` object.
#' @keywords internal
#' @export
`%!*%` <- as_ndc_mul

#' 3D world to camera transformation
#'
#' @param eye A numeric vector of length 3 giving the position of the camera.
#' @param center A numeric vector of length 3 giving the position of the target.
#' @param up A numeric vector of length 3 giving the direction of the "up" vector.
#' @param fovy A numeric scalar giving the field of view in radians.
#' @param aspect A numeric scalar giving the aspect ratio.
#' @param near A numeric scalar giving the distance to the near plane.
#' @param far A numeric scalar giving the distance to the far plane.
#' @param width,height A numeric scalar giving the width and height of the viewport.
#' @returns A `transform3d` object.
#' @rdname camera
#' @name camera
NULL

#' @rdname camera
#' @export
lookat3d <- function(eye, center, up = c(0, 1, 0)) {
  if (!all(eye != center)) {
    msg <- glue::glue(
      "`center` must be different from `eye` for all dimensions. Found same values at positions: {paste0(which(eye == center), collapse = ', ')}"
    )
    rlang::abort(msg)
  }
  z <- vec3_normalize(center - eye)
  x <- vec3_normalize(vec3_cross(z, up))
  y <- vec3_normalize(vec3_cross(x, z))
  # fmt: skip
  out <- matrix(
    c(
      x[1], y[1], z[1], -x %*% eye,
      x[2], y[2], z[2], -y %*% eye,
      x[3], y[3], z[3], -z %*% eye,
      0, 0, 0, 1
    ),
    ncol = 4,
    byrow = TRUE
  )
  as_trans3d(t(out))
}

#' @rdname camera
#' @export
persp3d <- function(fovy, aspect, near = .1, far = 10) {
  if (far <= near) {
    rlang::abort("`far` must be greater than `near`")
  }
  if (near <= 0) {
    rlang::abort("`near` must be greater than 0")
  }
  f <- 1 / tan(fovy / 2)
  # fmt: skip
  out <- matrix(
    c(
      f / aspect, 0, 0, 0,
      0, f, 0, 0,
      0, 0, -1 * (far + near) / (far - near), -1,
      0, 0, -2 * near * far / (far - near), 0
    ),
    nrow = 4
  )
  as_trans3d(t(out))
}

#' @rdname camera
#' @export
viewport3d <- function(width, height) {
  # fmt: skip
  out <-
    matrix(
      c(
        width / 2, 0, 0, width / 2,
        0, -height / 2, 0, height / 2,
        0, 0, 0.5, 0.5,
        0, 0, 0, 1
      ),
      nrow = 4,
      byrow = TRUE
    )
  as_trans3d(t(out))
}
