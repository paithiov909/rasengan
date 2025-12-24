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
  class(m) <- unique(c("transform3d", "at_matrix", class(m)))
  m
}

#' Perspective division
#'
#' Multiplication of two matrices after normalizing the first one.
#'
#' @param lhs,rhs A numeric matrix.
#' @returns A numeric matrix.
#' @export
#' @family camera
#' @rdname ndc_mul
ndc_mul <- function(lhs, rhs) UseMethod("ndc_mul")

#' @export
ndc_mul.default <- function(lhs, rhs) {
  w <- lhs[, 4]
  ok <- w != 0 & is.finite(w)
  if (!all(ok)) {
    rlang::warn("Some points had invalid w (0, NA, or Inf). They were dropped.")
  }
  lhs[ok, 1:3] <- lhs[ok, 1:3] / w[ok]
  lhs[ok, 4] <- 1
  lhs[ok, ] %*% rhs
}

#' @keywords internal
#' @export
ndc_mul.transform3d <- function(lhs, rhs) {
  rlang::abort("`lhs` must be a numeric matrix")
}

#' @rdname ndc_mul
#' @export
`%!*%` <- ndc_mul

#' 3D world to camera transformation
#'
#' @param eye A numeric vector of length 3 giving the position of the camera.
#' @param center A numeric vector of length 3 giving the position where the camera is looking at.
#' @param up A numeric vector of length 3 giving the direction of the "up" vector for the camera.
#' @param fovy A numeric scalar giving the field of view in radians.
#' @param aspect A numeric scalar giving the aspect ratio.
#' @param near A numeric scalar giving the distance to the near plane.
#' @param far A numeric scalar giving the distance to the far plane.
#' @param width,height A numeric scalar giving the width and height of the viewport.
#' @param ox,oy A numeric scalar giving the offset of the viewport in pixels.
#' @returns A `transform3d` object.
#' @family camera
#'
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
persp3d <- function(fovy, aspect, near = 0.1, far = 10) {
  if (far <= near) {
    rlang::abort("`far` must be greater than `near`")
  }
  if (near <= 0) {
    rlang::abort("`near` must be greater than 0")
  }
  f <- 1 / tan(fovy * .5)
  # fmt: skip
  out <- matrix(
    c(
      f / aspect, 0, 0, 0,
      0, f, 0, 0,
      0, 0, (far + near) / (near - far), 2 * far * near / (near - far),
      0, 0, -1, 0
    ),
    ncol = 4,
    byrow = TRUE
  )
  as_trans3d(t(out))
}

#' @rdname camera
#' @export
viewport3d <- function(width, height, ox = 0, oy = 0) {
  # fmt: skip
  out <-
    matrix(
      c(
        width / 2, 0, 0, width / 2 + ox,
        0, -height / 2, 0, height / 2 + oy,
        0, 0, .5, .5,
        0, 0, 0, 1
      ),
      ncol = 4,
      byrow = TRUE
    )
  as_trans3d(t(out))
}
