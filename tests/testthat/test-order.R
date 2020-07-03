test_that("moving median", {

  n = 10000
  w = 300

  # moving median, odd window
  w = 301
  x = runif(n)
  f = make_moving_median(window = w)

  y = f(x)
  ym = roll::roll_median(x, width = w)

  delta = sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # moving median, even window
  w = 300
  x = runif(n)
  f = make_moving_median(window = w)

  y = f(x)
  ym = roll::roll_median(x, width = w)

  delta = sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)
})

test_that("moving sort", {

  n = 10000
  w = 2000

  x <- runif(n)
  f <- make_moving_sorted(window = w)
  for (i in seq_len(5)) {
    # flush window
    xx <- sample(x, size = w)
    y <- f$insert(xx)
    ym <- sort(xx)
    delta <- sum(abs(y - ym), na.rm = TRUE)
    expect_equal(delta, 0.0)
  }

  x <- runif(n)
  for (i in seq_len(5)) {
    xx <- sample(x, size = 3000)
    y <- f$insert(xx)
    expect_equal(is.unsorted(y), FALSE)
  }
})

test_that("moving quantile", {

  n = 10000
  w = 120

  # moving quantile
  x <- runif(n)
  probs <- seq(0, 1, 0.25)
  f <- make_moving_quantile(window = w, probs = probs)

  y <- f(x)
  ym <- zoo::rollapply(x, 120, quantile, probs = probs, type = 3, fill = NA, align = "right")

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)

  # moving gastwirth
  gastwirth <- function(x) {
    w <- stats::quantile(x, probs = c(1/3, 1/2, 2/3), type = 3)
    sum(w * c(0.3, 0.4, 0.3))
  }
  x <- runif(n)
  f <- make_moving_gastwirth(window = w)

  y <- f(x)
  ym <- zoo::rollapply(x, 120, gastwirth, fill = NA, align = "right")

  delta <- sum(abs(y - ym), na.rm = TRUE)
  expect_equal(delta, 0.0)
})
