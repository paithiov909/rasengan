skip_on_cran()
skip_on_ci()

test_that("smoothstep seems to be correct", {
  vdiffr::expect_doppelganger("smoothstep", {
    op <- par(mfrow = c(1, 2), mar = c(2, 2, 2, 2))
    s <- seq(0, 1, by = 0.01)
    plot(smoothstep(s), type = "l")
    plot(smootherstep(s), type = "l")
    par(op)
    invisible(grDevices::recordPlot())
  })
})

test_that("ease-in seems to be correct", {
  vdiffr::expect_doppelganger("ease-in", {
    op <- par(mfrow = c(2, 5), mar = c(2, 2, 2, 2))
    s <- seq(0, 1, by = 0.01)
    plot(ease_in(s, "sine"), type = "l")
    plot(ease_in(s, "quad"), type = "l")
    plot(ease_in(s, "cubic"), type = "l")
    plot(ease_in(s, "quart"), type = "l")
    plot(ease_in(s, "quint"), type = "l")
    plot(ease_in(s, "exp"), type = "l")
    plot(ease_in(s, "circle"), type = "l")
    plot(ease_in(s, "elastic"), type = "l")
    plot(ease_in(s, "back"), type = "l")
    plot(ease_in(s, "bounce"), type = "l")
    par(op)
    invisible(grDevices::recordPlot())
  })
})

test_that("ease-out seems to be correct", {
  vdiffr::expect_doppelganger("ease-out", {
    op <- par(mfrow = c(2, 5), mar = c(2, 2, 2, 2))
    s <- seq(0, 1, by = 0.01)
    plot(ease_out(s, "sine"), type = "l")
    plot(ease_out(s, "quad"), type = "l")
    plot(ease_out(s, "cubic"), type = "l")
    plot(ease_out(s, "quart"), type = "l")
    plot(ease_out(s, "quint"), type = "l")
    plot(ease_out(s, "exp"), type = "l")
    plot(ease_out(s, "circle"), type = "l")
    plot(ease_out(s, "elastic"), type = "l")
    plot(ease_out(s, "back"), type = "l")
    plot(ease_out(s, "bounce"), type = "l")
    par(op)
    invisible(grDevices::recordPlot())
  })
})

test_that("ease-in-out seems to be correct", {
  vdiffr::expect_doppelganger("ease-in-out", {
    op <- par(mfrow = c(2, 5), mar = c(2, 2, 2, 2))
    s <- seq(0, 1, by = 0.01)
    plot(ease_in_out(s, "sine"), type = "l")
    plot(ease_in_out(s, "quad"), type = "l")
    plot(ease_in_out(s, "cubic"), type = "l")
    plot(ease_in_out(s, "quart"), type = "l")
    plot(ease_in_out(s, "quint"), type = "l")
    plot(ease_in_out(s, "exp"), type = "l")
    plot(ease_in_out(s, "circle"), type = "l")
    plot(ease_in_out(s, "elastic"), type = "l")
    plot(ease_in_out(s, "back"), type = "l")
    plot(ease_in_out(s, "bounce"), type = "l")
    par(op)
    invisible(grDevices::recordPlot())
  })
})
