#include "FastNoiseLite.h"
#include <cpp11.hpp>

[[cpp11::register]]
cpp11::doubles noise_2d_cpp(int type, int seed, float freq, int fractals,
                            int octaves, float lacunarity, float gain,
                            float weighted_strength, float ping_pong_strength,
                            int distance_function, int return_type,
                            float jitter, cpp11::doubles_matrix<> d) {
  FastNoiseLite gen(seed);
  gen.SetNoiseType(static_cast<FastNoiseLite::NoiseType>(type));
  gen.SetFrequency(freq);
  gen.SetCellularDistanceFunction(
      static_cast<FastNoiseLite::CellularDistanceFunction>(distance_function));
  gen.SetCellularReturnType(
      static_cast<FastNoiseLite::CellularReturnType>(return_type));
  gen.SetCellularJitter(jitter);

  if (fractals != 0) {
    gen.SetFractalType(static_cast<FastNoiseLite::FractalType>(fractals));
    gen.SetFractalOctaves(octaves);
    gen.SetFractalLacunarity(lacunarity);
    gen.SetFractalGain(gain);
    gen.SetFractalWeightedStrength(weighted_strength);
    gen.SetFractalPingPongStrength(ping_pong_strength);
  }

  cpp11::writable::doubles result(d.nrow());
  for (int i = 0; i < d.nrow(); i++) {
    result[i] = gen.GetNoise(d(i, 0), d(i, 1));
  }
  return result;
}

[[cpp11::register]]
cpp11::doubles noise_3d_cpp(int type, int seed, float freq, int fractals,
                            int octaves, float lacunarity, float gain,
                            float weighted_strength, float ping_pong_strength,
                            int distance_function, int return_type,
                            float jitter, int rotation_type,
                            cpp11::doubles_matrix<> d) {
  FastNoiseLite gen(seed);
  gen.SetNoiseType(static_cast<FastNoiseLite::NoiseType>(type));
  gen.SetRotationType3D(
      static_cast<FastNoiseLite::RotationType3D>(rotation_type));
  gen.SetFrequency(freq);
  gen.SetCellularDistanceFunction(
      static_cast<FastNoiseLite::CellularDistanceFunction>(distance_function));
  gen.SetCellularReturnType(
      static_cast<FastNoiseLite::CellularReturnType>(return_type));
  gen.SetCellularJitter(jitter);

  if (fractals != 0) {
    gen.SetFractalType(static_cast<FastNoiseLite::FractalType>(fractals));
    gen.SetFractalOctaves(octaves);
    gen.SetFractalLacunarity(lacunarity);
    gen.SetFractalGain(gain);
    gen.SetFractalWeightedStrength(weighted_strength);
    gen.SetFractalPingPongStrength(ping_pong_strength);
  }

  cpp11::writable::doubles result(d.nrow());
  for (int i = 0; i < d.nrow(); i++) {
    result[i] = gen.GetNoise(d(i, 0), d(i, 1), d(i, 2));
  }
  return result;
}

[[cpp11::register]]
cpp11::doubles_matrix<> domain_warp_2d_cpp(int dtype, int seed, float amplitude,
                                           int fractals, int octaves,
                                           float lacunarity, float gain,
                                           cpp11::doubles_matrix<> d) {
  FastNoiseLite gen(seed);
  gen.SetDomainWarpType(static_cast<FastNoiseLite::DomainWarpType>(dtype));
  gen.SetDomainWarpAmp(amplitude);

  if (fractals != 0) {
    gen.SetFractalType(static_cast<FastNoiseLite::FractalType>(fractals));
    gen.SetFractalOctaves(octaves);
    gen.SetFractalLacunarity(lacunarity);
    gen.SetFractalGain(gain);
  }

  cpp11::writable::doubles_matrix<> result(d.nrow(), 2);
  double x, y;
  for (int i = 0; i < d.nrow(); i++) {
    x = d(i, 0);
    y = d(i, 1);
    gen.DomainWarp(x, y);
    result(i, 0) = x;
    result(i, 1) = y;
  }
  return result;
}

[[cpp11::register]]
cpp11::doubles_matrix<> domain_warp_3d_cpp(int dtype, int seed, float amplitude,
                                           int fractals, int octaves,
                                           float lacunarity, float gain,
                                           int rotation_type,
                                           cpp11::doubles_matrix<> d) {
  FastNoiseLite gen(seed);
  gen.SetDomainWarpType(static_cast<FastNoiseLite::DomainWarpType>(dtype));
  gen.SetDomainWarpAmp(amplitude);
  gen.SetRotationType3D(
      static_cast<FastNoiseLite::RotationType3D>(rotation_type));

  if (fractals != 0) {
    gen.SetFractalType(static_cast<FastNoiseLite::FractalType>(fractals));
    gen.SetFractalOctaves(octaves);
    gen.SetFractalLacunarity(lacunarity);
    gen.SetFractalGain(gain);
  }

  cpp11::writable::doubles_matrix<> result(d.nrow(), 3);
  double x, y, z;
  for (int i = 0; i < d.nrow(); i++) {
    x = d(i, 0);
    y = d(i, 1);
    z = d(i, 2);
    gen.DomainWarp(x, y, z);
    result(i, 0) = x;
    result(i, 1) = y;
    result(i, 2) = z;
  }
  return result;
}
