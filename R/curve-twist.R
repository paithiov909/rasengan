#' Generate a smooth twisted curve in 2D from a 3D spiral projection
#'
#' This function creates a smooth, looped 2D curve
#' by generating a 3D twisted curve and projecting it onto 2D using PCA.
#' The resulting shape resembles a twisted loop, and can be oriented
#' and scaled freely.
#'
#' @note
#' The `origin` and `direction` parameters
#' do not directly constrain the endpoints of the curve.
#' Instead, after generating a 3D spiral,
#' the function performs PCA to obtain a best-fit 2D projection plane.
#' The projected curve is then transformed
#' so that the direction from its first point to its last point
#' aligns approximately with the vector from `origin` to `direction`.
#' Due to the curvature of the original spiral
#' and the non-linear nature of the projection,
#' the endpoints may not exactly match.
#'
#' @param n Integer.
#' Number of points to generate along the curve. Default is `300`.
#' @param amplitude Numeric.
#' Amplitude of the radial modulation in the 3D spiral.
#' Controls how tightly the loop twists.
#' @param frequency Numeric.
#' Frequency of the radial modulation (i.e., number of bumps per revolution).
#' @param origin Numeric vector of length 2.
#' The desired starting position of the curve in 2D space.
#' @param direction Numeric vector of length 2.
#' A target point indicating the overall orientation and extent of the curve.
#' The curve will be transformed to point
#' approximately from `origin` toward this vector.
#' @param t_range Numeric vector of length 2.
#' The range of the parameter `t`, which controls how far along the spiral to travel.
#' Larger ranges yield longer, more twisted curves.
#' @returns A data frame with two columns ("x", "y") and `n` rows,
#' representing the 2D coordinates of the projected curve.
#' @export
#' @examples
#' curve <- twist_curve()
#' plot(curve, type = "l", asp = 1)
#'
#' # Change orientation
#' curve2 <- twist_curve(origin = c(0, 0), direction = c(1, 1))
#' lines(curve2, col = "red")
#'
#' # More dramatic twist
#' curve3 <- twist_curve(amplitude = 0.5, frequency = 3)
#' lines(curve3, col = "blue")
twist_curve <- function(n = 300,
                        amplitude = 0.2,
                        frequency = 2,
                        origin = c(0, 0),
                        direction = c(1, 0),
                        t_range = c(0, 2 * pi)) {
  t <- seq(t_range[1], t_range[2], length.out = n)
  r <- 1 - amplitude * sin(frequency * t)
  d <- cbind(
    r * cos(t),
    r * sin(t),
    t
  )
  m <- stats::prcomp(d, center = TRUE, scale. = FALSE)
  path <- m$x[, 2:3]

  from <- path[1, ]
  to <- path[n, ]
  path_centered <- sweep(path, 2, from)

  target_vec <- direction - origin
  direction <- to - from
  scale_factor <- sqrt(sum(target_vec^2)) / sqrt(sum(direction^2))

  angle <-
    atan2(target_vec[2], target_vec[1]) - atan2(direction[2], direction[1])
  rot <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2)

  path_transformed <- t(rot %*% t(path_centered)) * scale_factor
  path_final <- sweep(path_transformed, 2, -origin, "+")

  colnames(path_final) <- c("x", "y")
  as.data.frame(path_final)
}
