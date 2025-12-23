#include <cpp11.hpp>
#include <vector>

[[cpp11::register]]
cpp11::integers genidx_rings_cpp(const cpp11::integers_matrix<>& mat) {
  const int nr = mat.nrow();
  const int nc = mat.ncol();
  const int num_rings = (std::min(nr, nc) + 1) / 2;

  cpp11::writable::integers out(nr * nc);
  int k = 0;

  for (int r = 1; r <= num_rings; ++r) {
    int top = r;
    int bottom = nr - r + 1;
    int left = r;
    int right = nc - r + 1;

    // Single row
    if (top == bottom) {
      for (int j = left; j <= right; ++j) {
        out[k++] = mat(top - 1, j - 1);
      }
      continue;
    }
    // Single column
    if (left == right) {
      for (int i = top; i <= bottom; ++i) {
        out[k++] = mat(i - 1, left - 1);
      }
      continue;
    }
    // 1. Top (left → right)
    for (int j = left; j <= right; ++j) {
      out[k++] = mat(top - 1, j - 1);
    }
    // 2. Right (top+1 → bottom-1)
    for (int i = top + 1; i <= bottom - 1; ++i) {
      out[k++] = mat(i - 1, right - 1);
    }
    // 3. Bottom (right → left)
    for (int j = right; j >= left; --j) {
      out[k++] = mat(bottom - 1, j - 1);
    }
    // 4. Left (bottom-1 → top+1)
    for (int i = bottom - 1; i >= top + 1; --i) {
      out[k++] = mat(i - 1, left - 1);
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::integers genidx_spiral_cpp(const cpp11::integers_matrix<>& mat) {
  const int nr = mat.nrow();
  const int nc = mat.ncol();
  int top = 0;
  int bottom = nr - 1;
  int left = 0;
  int right = nc - 1;

  cpp11::writable::integers out(nr * nc);
  int k = 0;

  while (top <= bottom && left <= right) {
    for (int i = left; i <= right && top <= bottom; ++i) {
      out[k++] = mat(top, i);
    }
    ++top;

    for (int i = top; i <= bottom && left <= right; ++i) {
      out[k++] = mat(i, right);
    }
    --right;

    for (int i = right; i >= left && top <= bottom; --i) {
      out[k++] = mat(bottom, i);
    }
    --bottom;

    for (int i = bottom; i >= top && left <= right; --i) {
      out[k++] = mat(i, left);
    }
    ++left;
  }
  return out;
}

[[cpp11::register]]
cpp11::integers genidx_stride_cpp(int n, int step) {
  cpp11::writable::integers out(n);
  int k = 0;

  for (int s = 1; s <= step; s++) {
    for (int num = s; num <= n; num += step) {
      out[k++] = num;
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::integers genidx_zigzag_cpp(const cpp11::integers_matrix<> mat) {
  const int nr = mat.nrow();
  const int nc = mat.ncol();

  cpp11::writable::integers out(nr * nc);
  int k = 0;

  // s = i+j  (1-indexed)
  // s ranges from 2 to nr+nc
  for (int s = 2; s <= nr + nc; ++s) {

    // i ranges over possible rows for this diagonal
    int i_min = std::max(1, s - nc);
    int i_max = std::min(nr, s - 1);

    if ((s % 2) == 0) {
      // even -> reverse direction
      for (int i = i_max; i >= i_min; --i) {
        int j = s - i;
        out[k++] = mat(i - 1, j - 1);
      }
    } else {
      // odd -> forward direction
      for (int i = i_min; i <= i_max; ++i) {
        int j = s - i;
        out[k++] = mat(i - 1, j - 1);
      }
    }
  }
  return out;
}
