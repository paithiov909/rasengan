#include <cpp11.hpp>
#include "Easing.h"

using namespace cpp11;

[[cpp11::register]]
doubles in_quad(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InQuad(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_quad(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutQuad(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_quad(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutQuad(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_cubic(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InCubic(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_cubic(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutCubic(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_cubic(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutCubic(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_quart(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InQuart(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_quart(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutQuart(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_quart(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutQuart(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_quint(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InQuint(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_quint(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutQuint(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_quint(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutQuint(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_sine(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InSine(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_sine(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutSine(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_sine(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutSine(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_exp(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InExp(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_exp(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutExp(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_exp(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutExp(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_circ(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InCirc(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_circ(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutCirc(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_circ(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutCirc(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_elastic(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InElastic(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_elastic(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutElastic(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_elastic(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutElastic(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_back(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InBack(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_back(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutBack(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_back(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutBack(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_bounce(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InBounce(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles out_bounce(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::OutBounce(time[i]);
  }
  return result;
}

[[cpp11::register]]
doubles in_out_bounce(const doubles time) {
  writable::doubles result(time.size());
  for (R_xlen_t i = 0; i < time.size(); i++) {
    result[i] = Easing::InOutBounce(time[i]);
  }
  return result;
}
