#' Create a noise generator
#'
#' Creates a noise generator function that wraps 'FastNoiseLite'.
#'
#' @seealso
#' [Documentation · Auburn/FastNoiseLite Wiki](https://github.com/Auburn/FastNoiseLite/wiki/Documentation)
#'
#' @param noise_type Noise type to use.
#' @param freq Frequency.
#' @param fractal_type Fractal type.
#' @param octaves Number of octaves.
#' @param lacunarity Lacunarity (the frequency multiplier between each octave).
#' @param gain Gain (the relative strength of noise from each layer when compared to the last).
#' @param weighted_strength Weighted strength for fractal noise.
#' Keep between `0` and `1` to maintain `[-1, 1]` output bounding.
#' @param ping_pong_strength Ping-pong strength for 'PingPong' fractal noise.
#' @param distance_function Distance function for cellular noise.
#' @param return_type Return type for cellular noise.
#' @param jitter Jitter for cellular noise.
#' @param rotation_type Rotation type for 3D noise.
#' @returns
#' A function that takes arguments:
#' * `x`, `y`, (or just `data`) and `seed` for 2D noise
#' * `x`, `y`, `z`, (or just `data`) and `seed` for 3D noise
#' and returns noise values.
#' Note that `seed` is set to a random value by default,
#' so if you want to use the same seed for multiple calls, you need to explicitly set it.
#' @examples
#' noise_2d()(1:10, 1)
#' noise_3d()(1:16, 1:16, 1)
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
    freq = 0.01,
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
    jitter = 1.0) {
  function(x, y, data = NULL, seed = sample.int(1337, 1)) {
    noise_type <- int_match(noise_type, "noise_type", c(
      "OpenSimplex2",
      "OpenSimplex2S",
      "Cellular",
      "Perlin",
      "ValueCubic",
      "Value"
    ))
    fractal_type <- int_match(fractal_type, "fractal_type", c(
      "None",
      "FBm",
      "Rigid",
      "PingPong"
    ))
    distance_function <- int_match(distance_function, "distance_function", c(
      "Euclidean",
      "EuclideanSq",
      "Manhattan",
      "Hybrid"
    ))
    return_type <- int_match(return_type, "return_type", c(
      "CellValue",
      "Distance",
      "Distance2",
      "Distance2Add",
      "Distance2Sub",
      "Distance2Mul",
      "Distance2Div"
    ))
    d <-
      if (!is.null(data)) {
        data
      } else {
        as.matrix(expand.grid(x, y))
      }
    noise_2d_cpp(
      noise_type, seed, freq,
      fractal_type, octaves, lacunarity, gain,
      weighted_strength, ping_pong_strength,
      distance_function, return_type, jitter,
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
    freq = 0.01,
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
    )) {
  function(x, y, z, data = NULL, seed = sample.int(1337, 1)) {
    noise_type <- int_match(noise_type, "noise_type", c(
      "OpenSimplex2",
      "OpenSimplex2S",
      "Cellular",
      "Perlin",
      "ValueCubic",
      "Value"
    ))
    fractal_type <- int_match(fractal_type, "fractal_type", c(
      "None",
      "FBm",
      "Rigid",
      "PingPong"
    ))
    distance_function <- int_match(distance_function, "distance_function", c(
      "Euclidean",
      "EuclideanSq",
      "Manhattan",
      "Hybrid"
    ))
    return_type <- int_match(return_type, "return_type", c(
      "CellValue",
      "Distance",
      "Distance2",
      "Distance2Add",
      "Distance2Sub",
      "Distance2Mul",
      "Distance2Div"
    ))
    rotation_type <- int_match(rotation_type, "rotation_type", c(
      "None",
      "ImproveXYPlanes",
      "ImproveXZPlanes"
    ))
    d <-
      if (!is.null(data)) {
        data
      } else {
        as.matrix(expand.grid(x, y, z))
      }
    noise_3d_cpp(
      noise_type, seed, freq,
      fractal_type, octaves, lacunarity, gain,
      weighted_strength, ping_pong_strength,
      distance_function, return_type, jitter,
      rotation_type,
      d
    )
  }
}

#' Generate data with domain warping
#'
#' Warps the position passed in with the x, y(, z) parameters.
#'
#' @param x,y,z Passed to `expand.grid()`.
#' @param seed Random seed.
#' @param warp_type Warp type.
#' @param amplitude Amplitude (the maximum warp distance from original position).
#' @param fractal_type Fractal type.
#' @param octaves Number of octaves.
#' @param lacunarity Lacunarity (the frequency multiplier between each octave).
#' @param gain Gain (the relative strength of noise from each layer when compared to the last).
#' @param rotation_type Rotation type for 3D noise.
#' @returns A double matrix.
#' @examples
#' noise_3d()(data = domain_warp(1:16, 1:16, 1))
#' @export
domain_warp <- function(
    x, y, z = NULL,
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
    )) {
  warp_type <- int_match(warp_type, "warp_type", c(
    "OpenSimplex2",
    "OpenSimplex2Reduced",
    "BasicGrid"
  ))
  fractal_type <- rlang::arg_match(fractal_type, c(
    "None",
    "DomainWarpProgressive",
    "DomainWarpIndependent"
  ))
  fractal_type <-
    if (fractal_type == "DomainWarpProgressive") {
      4L
    } else if (fractal_type == "DomainWarpProgressive") {
      5L
    } else {
      0L
    }
  rotation_type <- int_match(rotation_type, "rotation_type", c(
    "None",
    "ImproveXYPlanes",
    "ImproveXZPlanes"
  ))

  if (is.null(z)) {
    d <- as.matrix(expand.grid(x, y))
    domain_warp_2d_cpp(
      warp_type,
      seed,
      amplitude,
      fractal_type,
      octaves,
      lacunarity,
      gain,
      d
    )
  } else {
    d <- as.matrix(expand.grid(x, y, z))
    domain_warp_3d_cpp(
      warp_type,
      seed,
      amplitude,
      fractal_type,
      octaves,
      lacunarity,
      gain,
      rotation_type,
      d
    )
  }
}
