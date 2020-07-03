test_that("moving and cumulative mean and sd", {

  n = 10000
  w = 100

  # moving
  x <- runif(n)
  f <- make_moving_mean(window = w)

  y <- f(x)
  ym <- roll::roll_mean(x, width = w)

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  x <- runif(n)
  f <- make_moving_sd(window = w)

  y <- f(x)
  ym1 <- roll::roll_mean(x, width = w)
  ym2 <- roll::roll_sd(x, width = w)
  ym <- matrix(c(ym1, ym2), ncol = 2)

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # cumulative
  x <- runif(n)
  f <- make_cumulative_mean()

  y <- f(x)
  ym <- cumsum(x) / seq_along(x)

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  x <- runif(n)
  f <- make_cumulative_sd()

  y <- f(x)
  ym1 <- cumsum(x) / seq_along(x)
  for (i in seq_len(n)) {
    ym2[i] <- sd(x[seq_len(i)])
  }
  ym <- matrix(c(ym1, ym2), ncol = 2)

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)
})

test_that("moving and cumulative moments", {

  n = 10000
  w = 100

  # moving
  x <- runif(n)
  f <- make_moving_moment(window = w, order = 4)

  y <- f(x)
  ym1 <- zoo::rollapply(x, w, mean, fill = NA, align = "right")
  ym2 <- zoo::rollapply(x, w, moments::moment, order = 2, central = TRUE, fill = NA, align = "right")
  ym3 <- zoo::rollapply(x, w, moments::moment, order = 3, central = TRUE, fill = NA, align = "right")
  ym4 <- zoo::rollapply(x, w, moments::moment, order = 4, central = TRUE, fill = NA, align = "right")
  ym <- matrix(c(ym1, ym2, ym3, ym4), ncol = 4)

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # cumulative
  x <- runif(n)
  f <- make_cumulative_moment(order = 4)

  y <- f(x)
  for (i in seq_len(n)) {
    ym1[i] <- mean(x[seq_len(i)])
    ym2[i] <- moments::moment(x[seq_len(i)], order = 2, central = TRUE)
    ym3[i] <- moments::moment(x[seq_len(i)], order = 3, central = TRUE)
    ym4[i] <- moments::moment(x[seq_len(i)], order = 4, central = TRUE)
  }
  ym <- matrix(c(ym1, ym2, ym3, ym4), ncol = 4)

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)
})

test_that("moving and cumulative cov", {

  n = 10000
  w = 100

  # moving
  x <- runif(n)
  y <- runif(n)
  f <- make_moving_cov(window = w)

  z <- f(x, y)
  zm <- roll::roll_cov(x, y, width = w)

  delta <- sum(abs(z - zm), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # cumulative
  x <- runif(n)
  y <- runif(n)
  f <- make_cumulative_cov()

  z <- f(x, y)
  for (i in seq_len(n)) {
    zm[i] <- cov(x[seq_len(i)], y[seq_len(i)])
  }

  delta <- sum(abs(z - zm), na.rm = TRUE)
  expect_equal(delta, 0.0)
})
