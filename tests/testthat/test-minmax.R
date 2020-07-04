test_that("moving minmax", {
  n = 100000
  w = 120

  # moving min
  x <- runif(n, max = 1000)
  f <- make_moving_min(w)
  y <- f(x)
  ym <- roll::roll_min(x, w)
  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # moving max
  x <- runif(n, max = 1000)
  f <- make_moving_max(w)
  y <- f(x)
  ym <- roll::roll_max(x, w)
  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # moving argmin
  x <- runif(n, max = 1000)
  f <- make_moving_argmin(w, arg = "index")
  y <- f(x)
  ym <- roll::roll_idxmin(x, w)
  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # moving argmax
  x <- runif(n, max = 1000)
  f <- make_moving_argmax(w, arg = "index")
  y <- f(x)
  ym <- roll::roll_idxmax(x, w)
  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)
})
