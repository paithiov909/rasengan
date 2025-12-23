#' Create a noise generator
#'
#' Creates a noise generator function that wraps
#' [FastNoiseLite](https://github.com/Auburn/FastNoiseLite).
#'
#' @seealso
#' [Documentation · Auburn/FastNoiseLite Wiki](https://github.com/Auburn/FastNoiseLite/wiki/Documentation)
#'
#' @param noise_type A string; Noise type to use.
#' @param frequency A numeric scalar; Frequency.
#' @param fractal_type A string; Fractal type.
#' @param octaves A numeric scalar; Number of octaves.
#' @param lacunarity A numeric scalar;
#'  Lacunarity (the frequency multiplier between each octave).
#' @param gain A numeric scalar;
#'  Gain (the relative strength of noise from each layer when compared to the last).
#' @param weighted_strength A numeric scalar;
#'  Weighted strength for fractal noise.
#'  Keep between `0` and `1` to maintain `[-1, 1]` output bounding.
#' @param ping_pong_strength A numeric scalar;
#'  Ping-pong strength for 'PingPong' fractal noise.
#' @param distance_function A string; Distance function for cellular noise.
#' @param return_type A string; Return type for cellular noise.
#' @param jitter A numeric scalar; Jitter for cellular noise.
#' @param rotation_type A string; Rotation type for 3D noise.
#' @returns
#' A function that takes arguments:
#'
#' * `x`, `y`, (or just `data`) and `seed` for 2D noise
#' * `x`, `y`, `z`, (or just `data`) and `seed` for 3D noise
#'
#' and returns noise values.
#' Note that `seed` is set to a random value by default,
#' so if you want to use the same seed for multiple calls,
#' you need to explicitly set it.
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   nz <- expand(
#'     id = seq(0, 1, length.out = 16),
#'     x = seq(0, 32, by = 2),
#'     y = seq(0, 32, by = 2)
#'   ) |>
#'     dplyr::mutate(
#'       val = noise_3d()(data = cbind(id, x, y)),
#'       id = dplyr::consecutive_id(id)
#'     )
#' }
#' if (require("ggplot2", quietly = TRUE)) {
#'   ggplot(nz) +
#'     geom_tile(aes(x = x, y = y, fill = val))
#' }
#' @rdname noise
#' @name noise
NULL

#' @rdname noise
#' @export
noise_2d <- function(
  noise_type = c(
    "OpenSimplex2",
    "OpenSimplex2S",
    "Cellular",
    "Perlin",
    "ValueCubic",
    "Value"
  ),
  frequency = 0.01,
  fractal_type = c(
    "None",
    "FBm",
    "Rigid",
    "PingPong"
  ),
  octaves = 3,
  lacunarity = 2.0,
  gain = 0.5,
  weighted_strength = 0.0,
  ping_pong_strength = 2.0,
  distance_function = c(
    "EuclideanSq",
    "Euclidean",
    "Manhattan",
    "Hybrid"
  ),
  return_type = c(
    "Distance",
    "CellValue",
    "Distance2",
    "Distance2Add",
    "Distance2Sub",
    "Distance2Mul",
    "Distance2Div"
  ),
  jitter = 1.0
) {
  function(x, y, data = NULL, seed = sample.int(1337, 1)) {
    noise_type <- int_match(
      noise_type,
      "noise_type",
      c(
        "OpenSimplex2",
        "OpenSimplex2S",
        "Cellular",
        "Perlin",
        "ValueCubic",
        "Value"
      )
    )
    fractal_type <- int_match(
      fractal_type,
      "fractal_type",
      c(
        "None",
        "FBm",
        "Rigid",
        "PingPong"
      )
    )
    distance_function <- int_match(
      distance_function,
      "distance_function",
      c(
        "Euclidean",
        "EuclideanSq",
        "Manhattan",
        "Hybrid"
      )
    )
    return_type <- int_match(
      return_type,
      "return_type",
      c(
        "CellValue",
        "Distance",
        "Distance2",
        "Distance2Add",
        "Distance2Sub",
        "Distance2Mul",
        "Distance2Div"
      )
    )
    d <-
      if (!is.null(data)) {
        data
      } else {
        as.double(as.matrix(expand.grid(x, y)))
      }
    noise_2d_cpp(
      noise_type,
      as.integer(seed),
      as.double(frequency),
      fractal_type,
      as.integer(octaves),
      as.double(lacunarity),
      as.double(gain),
      weighted_strength,
      ping_pong_strength,
      distance_function,
      return_type,
      as.double(jitter),
      d
    )
  }
}

#' @rdname noise
#' @export
noise_3d <- function(
  noise_type = c(
    "OpenSimplex2",
    "OpenSimplex2S",
    "Cellular",
    "Perlin",
    "ValueCubic",
    "Value"
  ),
  frequency = 0.01,
  fractal_type = c(
    "None",
    "FBm",
    "Rigid",
    "PingPong"
  ),
  octaves = 3,
  lacunarity = 2.0,
  gain = 0.5,
  weighted_strength = 0.0,
  ping_pong_strength = 2.0,
  distance_function = c(
    "EuclideanSq",
    "Euclidean",
    "Manhattan",
    "Hybrid"
  ),
  return_type = c(
    "Distance",
    "CellValue",
    "Distance2",
    "Distance2Add",
    "Distance2Sub",
    "Distance2Mul",
    "Distance2Div"
  ),
  jitter = 1.0,
  rotation_type = c(
    "None",
    "ImproveXYPlanes",
    "ImproveXZPlanes"
  )
) {
  function(x, y, z, data = NULL, seed = sample.int(1337, 1)) {
    noise_type <- int_match(
      noise_type,
      "noise_type",
      c(
        "OpenSimplex2",
        "OpenSimplex2S",
        "Cellular",
        "Perlin",
        "ValueCubic",
        "Value"
      )
    )
    fractal_type <- int_match(
      fractal_type,
      "fractal_type",
      c(
        "None",
        "FBm",
        "Rigid",
        "PingPong"
      )
    )
    distance_function <- int_match(
      distance_function,
      "distance_function",
      c(
        "Euclidean",
        "EuclideanSq",
        "Manhattan",
        "Hybrid"
      )
    )
    return_type <- int_match(
      return_type,
      "return_type",
      c(
        "CellValue",
        "Distance",
        "Distance2",
        "Distance2Add",
        "Distance2Sub",
        "Distance2Mul",
        "Distance2Div"
      )
    )
    rotation_type <- int_match(
      rotation_type,
      "rotation_type",
      c(
        "None",
        "ImproveXYPlanes",
        "ImproveXZPlanes"
      )
    )
    d <-
      if (!is.null(data)) {
        data
      } else {
        as.double(as.matrix(expand.grid(x, y, z)))
      }
    noise_3d_cpp(
      noise_type,
      as.integer(seed),
      as.double(frequency),
      fractal_type,
      as.integer(octaves),
      as.double(lacunarity),
      as.double(gain),
      weighted_strength,
      ping_pong_strength,
      distance_function,
      return_type,
      as.double(jitter),
      rotation_type,
      d
    )
  }
}

#' Generate data with domain warping
#'
#' Warps data with domain warping.
#'
#' @param data A numeric matrix that has just 2 or 3 columns.
#' @param seed An integer scalar; Random seed.
#' @param warp_type A string; Warp type.
#' @param amplitude A numeric scalar;
#'  Amplitude (the maximum warp distance from original position).
#' @param fractal_type A string; Fractal type.
#' @param octaves A numeric scalar; Number of octaves.
#' @param lacunarity A numeric scalar;
#'  Lacunarity (the frequency multiplier between each octave).
#' @param gain A numeric scalar;
#'  Gain (the relative strength of noise from each layer when compared to the last).
#' @param rotation_type A string; Rotation type for 3D noise.
#' @returns A double matrix.
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'  nz <- expand(
#'    id = seq(0, 1, length.out = 16),
#'    x = seq(0, 32, by = 2),
#'    y = seq(0, 32, by = 2)
#'  ) |>
#'    dplyr::mutate(
#'      val = noise_3d()(data = cbind(id, x, y) |> domain_warp()),
#'      id = dplyr::consecutive_id(id)
#'    )
#' }
#' if (require("ggplot2", quietly = TRUE)) {
#'   ggplot(nz) +
#'     geom_tile(aes(x = x, y = y, fill = val))
#' }
#' @export
domain_warp <- function(
  data,
  seed = sample.int(1337, 1),
  warp_type = c(
    "OpenSimplex2",
    "OpenSimplex2Reduced",
    "BasicGrid"
  ),
  amplitude = 1.0,
  fractal_type = c(
    "None",
    "DomainWarpProgressive",
    "DomainWarpIndependent"
  ),
  octaves = 3,
  lacunarity = 2.0,
  gain = 0.5,
  rotation_type = c(
    "None",
    "ImproveXYPlanes",
    "ImproveXZPlanes"
  )
) {
  warp_type <- int_match(
    warp_type,
    "warp_type",
    c(
      "OpenSimplex2",
      "OpenSimplex2Reduced",
      "BasicGrid"
    )
  )
  fractal_type <- rlang::arg_match(
    fractal_type,
    c(
      "None",
      "DomainWarpProgressive",
      "DomainWarpIndependent"
    )
  )
  fractal_type <-
    if (fractal_type == "DomainWarpProgressive") {
      4L
    } else if (fractal_type == "DomainWarpProgressive") {
      5L
    } else {
      0L
    }
  rotation_type <- int_match(
    rotation_type,
    "rotation_type",
    c(
      "None",
      "ImproveXYPlanes",
      "ImproveXZPlanes"
    )
  )

  if (ncol(data) == 2) {
    domain_warp_2d_cpp(
      warp_type,
      as.integer(seed),
      as.double(amplitude),
      fractal_type,
      as.integer(octaves),
      as.double(lacunarity),
      as.double(gain),
      as.matrix(data)
    )
  } else if (ncol(data) == 3) {
    domain_warp_3d_cpp(
      warp_type,
      as.integer(seed),
      as.double(amplitude),
      fractal_type,
      as.integer(octaves),
      as.double(lacunarity),
      as.double(gain),
      rotation_type,
      as.matrix(data)
    )
  } else {
    rlang::abort("`data` must have 2 or 3 columns")
  }
}
