test_that("trace_flow works", {
  dragon_curve_fn <- \(seed, n_steps, step_size, params) {
    cur <- matrix(c(0, 0, seed$x, seed$y), ncol = 2)
    for (i in seq_len(n_steps)) {
      aa <- cur %*%
        matrix(
          c(
            cos(pi / 4),
            sin(pi / 4),
            -sin(pi / 4),
            cos(pi / 4)
          ),
          ncol = 2
        )
      bb <- cur %*%
        matrix(
          c(
            cos(pi / 4 * 3),
            sin(pi / 4 * 3),
            -sin(pi / 4 * 3),
            cos(pi / 4 * 3)
          ),
          ncol = 2
        )
      cur <- (1 / sqrt(2)) * rbind(aa, bb + c(step_size, 0))
    }
    data.frame(step = n_steps, x = cur[, 1], y = cur[, 2])
  }
  expect_s3_class(
    {
      trace_flow(
        data.frame(
          x = c(0, -1),
          y = c(1, 0)
        ),
        dragon_curve_fn,
        n_steps = 10,
        step_size = 1
      )
    },
    "tbl_df"
  )
})

test_that("trace_flow fails as expected", {
  ## 'flow_fn' signature is wrong
  expect_snapshot_error({
    trace_flow(
      data.frame(x = 1:8, y = 1:8),
      flow_fn = \(seed, n_steps, step_size, ...) {
        data.frame(step = 1, x = seed$x, y = seed$y)
      }
    )
  })
  ## 'flow_fn' is not a function
  expect_snapshot_error({
    trace_flow(
      data.frame(x = 1:8, y = 1:8),
      flow_fn = NA
    )
  })

  test_fn1 <- \(seed, n_steps, step_size, params) {
    if (nrow(seed) != 1) {
      cli::cli_abort("test_fn1 only accepts a single seed")
    }
    data.frame(step = 1, x = seed$x, y = seed$y)
  }
  expect_s3_class(
    {
      trace_flow(
        data.frame(x = 1:8, y = 1:8),
        test_fn1,
        n_steps = 1,
        step_size = 1
      )
    },
    "tbl_df"
  )
  expect_error({
    trace_flow(
      data.frame(x = 1:8, y = 1:8),
      test_fn1,
      id = "same_group_idx",
      n_steps = 1,
      step_size = 1
    )
  })
})

test_that("trace_flow works in parallel", {
  skip_on_cran()
  skip_on_ci()

  mirai::daemons(1)
  on.exit(mirai::daemons(0), add = TRUE)

  noise_fn <- \(seed, n_steps, step_size, params) {
    x <- y <- step <- numeric(n_steps)
    cur <- as.matrix(seed[, c("x", "y"), drop = FALSE])
    for (idx in seq_len(n_steps)) {
      step[idx] <- idx
      x[idx] <- cur[, 1]
      y[idx] <- cur[, 2]
      cur[, 1] <- cur[, 1] +
        rasengan::noise_2d()(cur[, 1], cur[, 2], seed = 11) * step_size
      cur[, 2] <- cur[, 2] +
        rasengan::noise_2d()(cur[, 1], cur[, 2], seed = 22) * step_size
    }
    data.frame(step = step, x = x, y = y)
  }
  expect_s3_class(
    {
      trace_flow(
        path_clothoid(),
        noise_fn,
        n_steps = 3,
        step_size = 8
      )
    },
    "tbl_df"
  )

  test_fn2 <- function(seed, n_steps, step_size, params) {
    pts <- params[["field_df"]][, c("x", "y"), drop = FALSE]
    step <- x <- y <- numeric(n_steps)
    cur_point <- as.matrix(seed[, c("x", "y"), drop = FALSE])
    for (idx in seq_len(n_steps)) {
      step[idx] <- idx
      x[idx] <- cur_point[, 1]
      y[idx] <- cur_point[, 2]
      nn <-
        rasengan::mag(pts, origin = cur_point) |>
        which.min()
      v <- params[["field_df"]][nn, c("vx", "vy"), drop = FALSE]
      cur_point <- cur_point + step_size * as.double(v)
    }
    data.frame(step = step, x = x, y = y)
  }
  expect_s3_class(
    {
      trace_flow(
        data.frame(x = 1:8, y = 1:8),
        test_fn2,
        n_steps = 1,
        step_size = 1,
        field_df = data.frame(
          x = 1:8,
          y = 1:8,
          vx = runif(8, -1, 1),
          vy = runif(8, -1, 1)
        )
      )
    },
    "tbl_df"
  )
})
