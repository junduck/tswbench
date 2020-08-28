#' Create a moving sort object.
#'
#' @param window moving window size
#'
#' @return an object of ocls_moving_sort class
#' @export
#'
make_moving_sorted <- function(window) {

  window <- as.integer(window)
  stopifnot(window > 1L)

  x = new(ocls_moving_sort, window);
  x
}

#' Online moving median
#'
#' @param window moving window size
#'
#' @return a stateful online function
#' @export
#'
make_moving_median <- function(window) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  calc <- new(ocls_moving_median, window)
  function(x) {
    calc$update(x)
  }
}

#' Online moving quantile
#'
#' Only type 3 is supported
#'
#' @param window moving window size
#' @param probs numeric vector of probabilities with values in [0,1], passed to stats::quantile()
#'
#' @return
#' @export
#'
make_moving_quantile <- function(window, probs) {

  window <- as.integer(window)
  stopifnot(window > length(probs))

  idx <- stats::quantile(seq_len(window), probs = probs, type = 3)
  idx <- as.integer(idx) - 1L;

  calc <- new(ocls_moving_quantile, window, idx)
  function(x) {
    calc$update(x)
  }
}

#' Online moving Gastwirth estimator
#'
#' @param window moving window size
#'
#' @return a stateful online function
#' @export
#'
make_moving_gastwirth <- function(window) {

  window <- as.integer(window)
  stopifnot(window > 3L)

  f <- make_moving_quantile(window = window, probs = c(1/3, 1/2, 2/3))
  function(x) {
    y <- f(x)
    0.3 * y[, 1] + 0.4 * y[, 2] + 0.3 * y[, 3]
  }
}

#' Online quantile estimation based on P-Square algorithm
#'
#' @param probs
#'
#' @return a stateful online function
#' @export
#'
make_cumulative_psquare <- function(probs) {

  calc <- new(ocls_cumulative_psquare, probs)
  function(x) {
    calc$update(x)
  }
}

#' Online quantile estimation based on KLL algorithm
#'
#'
#' @return a stateful online function
#' @export
#'
make_cumulative_quantile <- function(k = 128, c = 2.0 / 3.0, lazy = TRUE) {

  calc <- new(ocls_cumulative_quantile, k, c, lazy)
  function(x) {
    calc$update(x)
  }
}
