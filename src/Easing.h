// Modified from https://qiita.com/HnniTns/items/9e3799d3b414dccbbd7d
struct Easing {
 private:
  template <class Ty = double>
  static constexpr Ty Pai{static_cast<Ty>(3.141592653589793)};

  template <typename Ty>
  static constexpr Ty Pai_c4 = static_cast<Ty>(2 * Pai<Ty> / 3);
  template <typename Ty>
  static constexpr Ty Pai_c5 = static_cast<Ty>(2 * Pai<Ty> / 4.5);

  template <typename Ty>
  static constexpr Ty _0_5 = static_cast<Ty>(0.5);
  template <typename Ty>
  static constexpr Ty _0_75 = static_cast<Ty>(0.75);
  template <typename Ty>
  static constexpr Ty _0_9375 = static_cast<Ty>(0.9375);
  template <typename Ty>
  static constexpr Ty _0_984375 = static_cast<Ty>(0.984375);

  template <typename Ty>
  static constexpr Ty _1_5 = static_cast<Ty>(1.5);
  template <typename Ty>
  static constexpr Ty _1_525 = static_cast<Ty>(1.525);
  template <typename Ty>
  static constexpr Ty _1_70158 = static_cast<Ty>(1.70158);

  template <typename Ty>
  static constexpr Ty _2_25 = static_cast<Ty>(2.25);
  template <typename Ty>
  static constexpr Ty _2_5 = static_cast<Ty>(2.5);
  template <typename Ty>
  static constexpr Ty _2_625 = static_cast<Ty>(2.625);
  template <typename Ty>
  static constexpr Ty _2_75 = static_cast<Ty>(2.75);

  template <typename Ty>
  static constexpr Ty _7_5625 = static_cast<Ty>(7.5625);

  template <typename Ty>
  static constexpr Ty _10_75 = static_cast<Ty>(10.75);
  template <typename Ty>
  static constexpr Ty _11_125 = static_cast<Ty>(11.125);

 private:
  template <typename Ty>
  [[nodiscard]] static inline bool AdjEqual(const Ty epsilon_num,
                                            const Ty num) {
    constexpr auto Fabs{[](const Ty num) constexpr {
      if (num > 0)
        return num;
      else
        return -num;
    }};

    static constexpr auto Epsilon{std::numeric_limits<Ty>::epsilon()};
    auto dis{Fabs(epsilon_num - num)};

    return (dis <= Epsilon);
  }

 public:
  template <typename Ty = float>
  [[nodiscard]] static inline Ty InQuad(Ty time) {
    return std::pow(time, 2);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutQuad(Ty time) {
    return 1 - std::pow(1 - time, 2);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutQuad(Ty time) {
    return time < _0_5<Ty> ? 2 * std::pow(time, 2)
                           : 1 - std::pow(-2 * time + 2, 2) * _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InCubic(Ty time) {
    return std::pow(time, 3);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutCubic(Ty time) {
    return 1 - std::pow(1 - time, 3);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutCubic(Ty time) {
    return time < _0_5<Ty> ? 4 * std::pow(time, 3)
                           : 1 - std::pow(-2 * time + 2, 3) * _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InQuart(Ty time) {
    return std::pow(time, 4);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutQuart(Ty time) {
    return 1 - std::pow(1 - time, 4);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutQuart(Ty time) {
    return time < _0_5<Ty> ? 8 * std::pow(time, 4)
                           : 1 - std::pow(-2 * time + 2, 4) * _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InQuint(Ty time) {
    return std::pow(time, 5);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutQuint(Ty time) {
    return 1 - std::pow(1 - time, 5);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutQuint(Ty time) {
    return time < _0_5<Ty> ? 16 * std::pow(time, 5)
                           : 1 - std::pow(-2 * time + 2, 5) * _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InSine(Ty time) {
    return 1 - std::cos(time * Pai<Ty> * _0_5<Ty>);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutSine(Ty time) {
    return std::sin(time * Pai<Ty> * _0_5<Ty>);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutSine(Ty time) {
    return -(std::cos(Pai<Ty> * time) - 1) * _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InExp(Ty time) {
    return AdjEqual<Ty>(time, 0.0) ? 0 : std::pow(2, 10 * time - 10);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutExp(Ty time) {
    return AdjEqual<Ty>(time, 1.0) ? 1 : 1 - std::pow(2, -10 * time);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutExp(Ty time) {
    if (AdjEqual<Ty>(time, 0.0)) return 0;
    if (AdjEqual<Ty>(time, 1.0)) return 1;
    return time < _0_5<Ty> ? std::pow(2, 20 * time - 10) * _0_5<Ty>
                           : (2 - std::pow(2, -20 * time + 10)) * _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InCirc(Ty time) {
    return 1 - std::sqrt(1 - std::pow(time, 2));
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutCirc(Ty time) {
    return std::sqrt(1 - std::pow(time - 1, 2));
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutCirc(Ty time) {
    return time < _0_5<Ty>
               ? (1 - std::sqrt(1 - std::pow(2 * time, 2))) * _0_5<Ty>
               : (std::sqrt(1 - std::pow(-2 * time + 2, 2)) + 1) * _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InElastic(Ty time) {
    if (AdjEqual<Ty>(time, 0.0)) return 0;
    if (AdjEqual<Ty>(time, 1.0)) return 1;
    return -std::pow(2, 10 * time - 10) *
           std::sin((time * 10 - _10_75<Ty>)*Pai_c4<Ty>);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutElastic(Ty time) {
    if (AdjEqual<Ty>(time, 0.0)) return 0;
    if (AdjEqual<Ty>(time, 1.0)) return 1;
    return std::pow(2, -10 * time) *
               std::sin((time * 10 - _0_75<Ty>)*Pai_c4<Ty>) +
           1;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutElastic(Ty time) {
    if (AdjEqual<Ty>(time, 0.0)) return 0;
    if (AdjEqual<Ty>(time, 1.0)) return 1;
    return time < _0_5<Ty>
               ? -std::pow(2, 20 * time - 10) *
                     std::sin((20 * time - _11_125<Ty>)*Pai_c5<Ty>) * _0_5<Ty>
               : std::pow(2, -20 * time + 10) *
                         std::sin((20 * time - _11_125<Ty>)*Pai_c5<Ty>) *
                         _0_5<Ty> +
                     1;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InBack(Ty time, Ty back = _1_70158<Ty>) {
    return (back + 1) * std::pow(time, 3) - back * std::pow(time, 2);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutBack(Ty time, Ty back = _1_70158<Ty>) {
    return 1 + (1 + back) * std::pow(time - 1, 3) +
           back * std::pow(time - 1, 2);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutBack(Ty time, Ty back = _1_70158<Ty>) {
    return time < _0_5<Ty>
               ? (std::pow(2 * time, 2) * ((back + 1) * 2 * time - back)) *
                     _0_5<Ty>
               : (std::pow(2 * time - 2, 2) *
                      ((back + 1) * (2 * time - 2) + back) +
                  2) *
                     _0_5<Ty>;
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty OutBounce(Ty time) {
    if (time < (1 / _2_75<Ty>)) {
      return _7_5625<Ty> * time * time;
    } else if (time < (2 / _2_75<Ty>)) {
      time -= _1_5<Ty> / _2_75<Ty>;
      return _7_5625<Ty> * time * time + _0_75<Ty>;
    } else if (time < (_2_5<Ty> / _2_75<Ty>)) {
      time -= _2_25<Ty> / _2_75<Ty>;
      return _7_5625<Ty> * time * time + _0_9375<Ty>;
    } else {
      time -= _2_625<Ty> / _2_75<Ty>;
      return _7_5625<Ty> * time * time + _0_984375<Ty>;
    }
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InBounce(Ty time) {
    return 1 - OutBounce<Ty>(1 - time);
  }

  template <typename Ty = float>
  [[nodiscard]] static inline Ty InOutBounce(Ty time) {
    return time < _0_5<Ty> ? (1 - OutBounce<Ty>(1 - time * 2)) * _0_5<Ty>
                           : (1 + OutBounce<Ty>(time * 2 - 1)) * _0_5<Ty>;
  }
};
