// This file contains modified excerpts from
// [google/spherical-harmonics](https://github.com/google/spherical-harmonics).
//
// Changes were made to remove the Eigen dependency
// and integrate the code into the package.
//
// The original code is under the Apache 2.0 license.
//
// ---
//
// Copyright 2015 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
#include "SphericalHarmonics.h"

// The general spherical harmonic functions and fitting methods are from:
// 1. R. Green, "Spherical Harmonic Lighting: The Gritty Details", GDC 2003,
//    http://www.research.scea.com/gdc2003/spherical-harmonic-lighting.pdf
//
// The environment map related functions are based on the methods in:
// 2. R. Ramamoorthi and P. Hanrahan, "An Efficient Representation for
//    Irradiance Environment Maps",. , P., SIGGRAPH 2001, 497-500
// 3. R. Ramamoorthi and P. Hanrahan, “On the Relationship between Radiance and
//    Irradiance: Determining the Illumination from Images of a Convex
//    Lambertian Object,” J. Optical Soc. Am. A, vol. 18, no. 10, pp. 2448-2459,
//    2001.
//
// Spherical harmonic rotations are implemented using the recurrence relations
// described by:
// 4. J. Ivanic and K. Ruedenberg, "Rotation Matrices for Real Spherical
//    Harmonics. Direct Determination by Recursion", J. Phys. Chem., vol. 100,
//    no. 15, pp. 6342-6347, 1996.
//    http://pubs.acs.org/doi/pdf/10.1021/jp953350u
// 4b. Corrections to initial publication:
//    http://pubs.acs.org/doi/pdf/10.1021/jp9833350
namespace sh {

namespace {

// Number of precomputed factorials and double-factorials that can be
// returned in constant time.
const int kCacheSize = 16;

const int kHardCodedOrderLimit = 4;

const int kIrradianceOrder = 2;

// Get the total number of coefficients for a function represented by
// all spherical harmonic basis of degree <= @order (it is a point of
// confusion that the order of an SH refers to its degree and not the order).
constexpr int GetCoefficientCount(int order) {
  return (order + 1) * (order + 1);
}

const int kIrradianceCoeffCount = GetCoefficientCount(kIrradianceOrder);

// Clamp the first argument to be greater than or equal to the second
// and less than or equal to the third.
double Clamp(double val, double min, double max) {
  return std::clamp(val, min, max);
}

// Return true if the first value is within epsilon of the second value.
bool NearByMargin(double actual, double expected) {
  double diff = actual - expected;
  if (diff < 0.0) {
    diff = -diff;
  }
  // 5 bits of error in mantissa (source of '32 *')
  return diff < 32 * std::numeric_limits<double>::epsilon();
}

// Hardcoded spherical harmonic functions for low orders (l is first number
// and m is second number (sign encoded as preceeding 'p' or 'n')).
//
// As polynomials they are evaluated more efficiently in cartesian coordinates,
// assuming that @d is unit. This is not verified for efficiency.
double HardcodedSH00(const Vector3d& d) {
  // 0.5 * sqrt(1/pi)
  return 0.282095;
}

double HardcodedSH1n1(const Vector3d& d) {
  // -sqrt(3/(4pi)) * y
  return -0.488603 * d.y();
}

double HardcodedSH10(const Vector3d& d) {
  // sqrt(3/(4pi)) * z
  return 0.488603 * d.z();
}

double HardcodedSH1p1(const Vector3d& d) {
  // -sqrt(3/(4pi)) * x
  return -0.488603 * d.x();
}

double HardcodedSH2n2(const Vector3d& d) {
  // 0.5 * sqrt(15/pi) * x * y
  return 1.092548 * d.x() * d.y();
}

double HardcodedSH2n1(const Vector3d& d) {
  // -0.5 * sqrt(15/pi) * y * z
  return -1.092548 * d.y() * d.z();
}

double HardcodedSH20(const Vector3d& d) {
  // 0.25 * sqrt(5/pi) * (-x^2-y^2+2z^2)
  return 0.315392 * (-d.x() * d.x() - d.y() * d.y() + 2.0 * d.z() * d.z());
}

double HardcodedSH2p1(const Vector3d& d) {
  // -0.5 * sqrt(15/pi) * x * z
  return -1.092548 * d.x() * d.z();
}

double HardcodedSH2p2(const Vector3d& d) {
  // 0.25 * sqrt(15/pi) * (x^2 - y^2)
  return 0.546274 * (d.x() * d.x() - d.y() * d.y());
}

double HardcodedSH3n3(const Vector3d& d) {
  // -0.25 * sqrt(35/(2pi)) * y * (3x^2 - y^2)
  return -0.590044 * d.y() * (3.0 * d.x() * d.x() - d.y() * d.y());
}

double HardcodedSH3n2(const Vector3d& d) {
  // 0.5 * sqrt(105/pi) * x * y * z
  return 2.890611 * d.x() * d.y() * d.z();
}

double HardcodedSH3n1(const Vector3d& d) {
  // -0.25 * sqrt(21/(2pi)) * y * (4z^2-x^2-y^2)
  return -0.457046 * d.y() *
         (4.0 * d.z() * d.z() - d.x() * d.x() - d.y() * d.y());
}

double HardcodedSH30(const Vector3d& d) {
  // 0.25 * sqrt(7/pi) * z * (2z^2 - 3x^2 - 3y^2)
  return 0.373176 * d.z() *
         (2.0 * d.z() * d.z() - 3.0 * d.x() * d.x() - 3.0 * d.y() * d.y());
}

double HardcodedSH3p1(const Vector3d& d) {
  // -0.25 * sqrt(21/(2pi)) * x * (4z^2-x^2-y^2)
  return -0.457046 * d.x() *
         (4.0 * d.z() * d.z() - d.x() * d.x() - d.y() * d.y());
}

double HardcodedSH3p2(const Vector3d& d) {
  // 0.25 * sqrt(105/pi) * z * (x^2 - y^2)
  return 1.445306 * d.z() * (d.x() * d.x() - d.y() * d.y());
}

double HardcodedSH3p3(const Vector3d& d) {
  // -0.25 * sqrt(35/(2pi)) * x * (x^2-3y^2)
  return -0.590044 * d.x() * (d.x() * d.x() - 3.0 * d.y() * d.y());
}

double HardcodedSH4n4(const Vector3d& d) {
  // 0.75 * sqrt(35/pi) * x * y * (x^2-y^2)
  return 2.503343 * d.x() * d.y() * (d.x() * d.x() - d.y() * d.y());
}

double HardcodedSH4n3(const Vector3d& d) {
  // -0.75 * sqrt(35/(2pi)) * y * z * (3x^2-y^2)
  return -1.770131 * d.y() * d.z() * (3.0 * d.x() * d.x() - d.y() * d.y());
}

double HardcodedSH4n2(const Vector3d& d) {
  // 0.75 * sqrt(5/pi) * x * y * (7z^2-1)
  return 0.946175 * d.x() * d.y() * (7.0 * d.z() * d.z() - 1.0);
}

double HardcodedSH4n1(const Vector3d& d) {
  // -0.75 * sqrt(5/(2pi)) * y * z * (7z^2-3)
  return -0.669047 * d.y() * d.z() * (7.0 * d.z() * d.z() - 3.0);
}

double HardcodedSH40(const Vector3d& d) {
  // 3/16 * sqrt(1/pi) * (35z^4-30z^2+3)
  double z2 = d.z() * d.z();
  return 0.105786 * (35.0 * z2 * z2 - 30.0 * z2 + 3.0);
}

double HardcodedSH4p1(const Vector3d& d) {
  // -0.75 * sqrt(5/(2pi)) * x * z * (7z^2-3)
  return -0.669047 * d.x() * d.z() * (7.0 * d.z() * d.z() - 3.0);
}

double HardcodedSH4p2(const Vector3d& d) {
  // 3/8 * sqrt(5/pi) * (x^2 - y^2) * (7z^2 - 1)
  return 0.473087 * (d.x() * d.x() - d.y() * d.y()) *
         (7.0 * d.z() * d.z() - 1.0);
}

double HardcodedSH4p3(const Vector3d& d) {
  // -0.75 * sqrt(35/(2pi)) * x * z * (x^2 - 3y^2)
  return -1.770131 * d.x() * d.z() * (d.x() * d.x() - 3.0 * d.y() * d.y());
}

double HardcodedSH4p4(const Vector3d& d) {
  // 3/16*sqrt(35/pi) * (x^2 * (x^2 - 3y^2) - y^2 * (3x^2 - y^2))
  double x2 = d.x() * d.x();
  double y2 = d.y() * d.y();
  return 0.625836 * (x2 * (x2 - 3.0 * y2) - y2 * (3.0 * x2 - y2));
}
// Compute the factorial for an integer @x. It is assumed x is at least 0.
// This implementation precomputes the results for low values of x, in which
// case this is a constant time lookup.
//
// The vast majority of SH evaluations will hit these precomputed values.
double Factorial(int x) {
  const double factorial_cache[kCacheSize] = {
      1,         1,          2,           6,
      24,        120,        720,         5040,
      40320,     362880,     3628800,     39916800,
      479001600, 6227020800, 87178291200, 1307674368000};

  if (x < kCacheSize) {
    return factorial_cache[x];
  } else {
    double s = factorial_cache[kCacheSize - 1];
    for (int n = kCacheSize; n <= x; n++) {
      s *= n;
    }
    return s;
  }
}

// Compute the double factorial for an integer @x. This assumes x is at least
// 0.  This implementation precomputes the results for low values of x, in
// which case this is a constant time lookup.
//
// The vast majority of SH evaluations will hit these precomputed values.
// See http://mathworld.wolfram.com/DoubleFactorial.html
double DoubleFactorial(int x) {
  const double dbl_factorial_cache[kCacheSize] = {
      1,   1,   2,    3,     8,     15,     48,     105,
      384, 945, 3840, 10395, 46080, 135135, 645120, 2027025};

  if (x < kCacheSize) {
    return dbl_factorial_cache[x];
  } else {
    double s = dbl_factorial_cache[kCacheSize - (x % 2 == 0 ? 2 : 1)];
    double n = x;
    while (n >= kCacheSize) {
      s *= n;
      n -= 2.0;
    }
    return s;
  }
}

// Evaluate the associated Legendre polynomial of degree @l and order @m at
// coordinate @x. The inputs must satisfy:
// 1. l >= 0
// 2. 0 <= m <= l
// 3. -1 <= x <= 1
// See http://en.wikipedia.org/wiki/Associated_Legendre_polynomials
//
// This implementation is based off the approach described in [1],
// instead of computing Pml(x) directly, Pmm(x) is computed. Pmm can be
// lifted to Pmm+1 recursively until Pml is found
double EvalLegendrePolynomial(int l, int m, double x) {
  // Compute Pmm(x) = (-1)^m(2m - 1)!!(1 - x^2)^(m/2), where !! is the double
  // factorial.
  double pmm = 1.0;
  // P00 is defined as 1.0, do don't evaluate Pmm unless we know m > 0
  if (m > 0) {
    double sign = (m % 2 == 0 ? 1 : -1);
    pmm = sign * DoubleFactorial(2 * m - 1) * pow(1 - x * x, m / 2.0);
  }

  if (l == m) {
    // Pml is the same as Pmm so there's no lifting to higher bands needed
    return pmm;
  }

  // Compute Pmm+1(x) = x(2m + 1)Pmm(x)
  double pmm1 = x * (2 * m + 1) * pmm;
  if (l == m + 1) {
    // Pml is the same as Pmm+1 so we are done as well
    return pmm1;
  }

  // Use the last two computed bands to lift up to the next band until l is
  // reached, using the recurrence relationship:
  // Pml(x) = (x(2l - 1)Pml-1 - (l + m - 1)Pml-2) / (l - m)
  for (int n = m + 2; n <= l; n++) {
    double pmn = (x * (2 * n - 1) * pmm1 - (n + m - 1) * pmm) / (n - m);
    pmm = pmm1;
    pmm1 = pmn;
  }
  // Pmm1 at the end of the above loop is equal to Pml
  return pmm1;
}

}  // namespace

Vector3d ToVector(double phi, double theta) {
  double r = sin(theta);
  return std::tuple{r * cos(phi), r * sin(phi), cos(theta)};
}

void ToSphericalCoords(const Vector3d& dir, double* phi, double* theta) {
  CHECK(NearByMargin(dir.squaredNorm(), 1.0), "dir is not unit");
  // Explicitly clamp the z coordinate so that numeric errors don't cause it
  // to fall just outside of acos' domain.
  *theta = acos(Clamp(dir.z(), -1.0, 1.0));
  // We don't need to divide dir.y() or dir.x() by sin(theta) since they are
  // both scaled by it and atan2 will handle it appropriately.
  *phi = atan2(dir.y(), dir.x());
}

double EvalSHSlow(int l, int m, double phi, double theta) {
  CHECK(l >= 0, "l must be at least 0.");
  CHECK(-l <= m && m <= l, "m must be between -l and l.");

  double kml = sqrt((2.0 * l + 1) * Factorial(l - abs(m)) /
                    (4.0 * M_PI * Factorial(l + abs(m))));
  if (m > 0) {
    return sqrt(2.0) * kml * cos(m * phi) *
           EvalLegendrePolynomial(l, m, cos(theta));
  } else if (m < 0) {
    return sqrt(2.0) * kml * sin(-m * phi) *
           EvalLegendrePolynomial(l, -m, cos(theta));
  } else {
    return kml * EvalLegendrePolynomial(l, 0, cos(theta));
  }
}

double EvalSHSlow(int l, int m, const Vector3d& dir) {
  double phi, theta;
  ToSphericalCoords(dir, &phi, &theta);
  return EvalSH(l, m, phi, theta);
}

double EvalSH(int l, int m, double phi, double theta) {
  // If using the hardcoded functions, switch to cartesian
  if (l <= kHardCodedOrderLimit) {
    return EvalSH(l, m, ToVector(phi, theta));
  } else {
    // Stay in spherical coordinates since that's what the recurrence
    // version is implemented in
    return EvalSHSlow(l, m, phi, theta);
  }
}

double EvalSH(int l, int m, const Vector3d& dir) {
  if (l <= kHardCodedOrderLimit) {
    // Validate l and m here (don't do it generally since EvalSHSlow also
    // checks it if we delegate to that function).
    CHECK(l >= 0, "l must be at least 0.");
    CHECK(-l <= m && m <= l, "m must be between -l and l.");
    CHECK(NearByMargin(dir.squaredNorm(), 1.0), "dir is not unit.");

    switch (l) {
      case 0:
        return HardcodedSH00(dir);
      case 1:
        switch (m) {
          case -1:
            return HardcodedSH1n1(dir);
          case 0:
            return HardcodedSH10(dir);
          case 1:
            return HardcodedSH1p1(dir);
        }
      case 2:
        switch (m) {
          case -2:
            return HardcodedSH2n2(dir);
          case -1:
            return HardcodedSH2n1(dir);
          case 0:
            return HardcodedSH20(dir);
          case 1:
            return HardcodedSH2p1(dir);
          case 2:
            return HardcodedSH2p2(dir);
        }
      case 3:
        switch (m) {
          case -3:
            return HardcodedSH3n3(dir);
          case -2:
            return HardcodedSH3n2(dir);
          case -1:
            return HardcodedSH3n1(dir);
          case 0:
            return HardcodedSH30(dir);
          case 1:
            return HardcodedSH3p1(dir);
          case 2:
            return HardcodedSH3p2(dir);
          case 3:
            return HardcodedSH3p3(dir);
        }
      case 4:
        switch (m) {
          case -4:
            return HardcodedSH4n4(dir);
          case -3:
            return HardcodedSH4n3(dir);
          case -2:
            return HardcodedSH4n2(dir);
          case -1:
            return HardcodedSH4n1(dir);
          case 0:
            return HardcodedSH40(dir);
          case 1:
            return HardcodedSH4p1(dir);
          case 2:
            return HardcodedSH4p2(dir);
          case 3:
            return HardcodedSH4p3(dir);
          case 4:
            return HardcodedSH4p4(dir);
        }
    }

    // This is unreachable given the CHECK's above but the compiler can't tell.
    return 0.0;
  } else {
    // Not hard-coded so use the recurrence relation (which will convert this
    // to spherical coordinates).
    return EvalSHSlow(l, m, dir);
  }
}

}  // namespace sh

[[cpp11::register]]
cpp11::doubles sph2d_cpp(int l, int m, const cpp11::doubles_matrix<>& d) {
  cpp11::writable::doubles ret(d.nrow());
  for (int i = 0; i < d.nrow(); i++) {
    ret[i] = sh::EvalSH(l, m, d(i, 1), d(i, 0)); // phi, theta
  }
  return ret;
}

[[cpp11::register]]
cpp11::doubles sph3d_cpp(int l, int m, const cpp11::doubles_matrix<>& d) {
  cpp11::writable::doubles ret(d.nrow());
  for (int i = 0; i < d.nrow(); i++) {
    ret[i] = sh::EvalSH(l, m, std::tuple{d(i, 0), d(i, 1), d(i, 2)}); // x, y, z
  }
  return ret;
}
