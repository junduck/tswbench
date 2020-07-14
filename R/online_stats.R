#' Online moving/cumulative mean and sd
#'
#' @param window moving window size
#' @param ... not used
#'
#' @return a stateful online function
#' @export
#'
make_moving_mean <- function(window, ...) {

  window <- as.integer(window)
  stopifnot(window > 1L)

  calc <- new(ocls_moving_mean, window)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_moving_mean
#' @export
#'
make_cumulative_mean <- function(...) {

  calc <- new(ocls_cumulative_mean)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_moving_mean
#' @export
#'
make_moving_sd <- function(window, ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  calc <- new(ocls_moving_sd, window)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_moving_mean
#' @export
#'
make_cumulative_sd <- function(...) {

  calc <- new(ocls_cumulative_sd)
  function(x) {
    calc$update(x)
  }
}

#' Moving mean absolute error
#'
#' @param window moving window size
#' @param ... not used
#'
#' @return a stateful online function
#' @export
#'
make_moving_mae <- function(window, ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  calc <- new(ocls_moving_mae, window)
  function(x) {
    calc$update(x)
  }
}

#' Online moving/cumulative statistics
#'
#' Support up to 4th order central moments.
#'
#' @param window moving window size
#' @param order order of moments
#' @param ... not used
#'
#' @return a stateful online function
#' @export
#'
make_moving_moment <- function(window, order, ...) {

  window <- as.integer(window)
  order <- as.integer(order)
  stopifnot(window >= 3L, 0L < order && order <= 4L)

  calc <- new(ocls_moving_moment, window, order)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_moving_moment
#' @export
#'
make_cumulative_moment <- function(order, ...) {

  order <- as.integer(order)
  stopifnot(0L < order && order <= 4L)

  calc <- new(ocls_cumulative_moment, order)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_moving_moment
#' @export
#'
make_moving_cov <- function(window, ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  calc <- new(ocls_moving_cov, window)
  function(x, y) {
    stopifnot(length(x) == length(y))
    calc$update(x, y)
  }
}

#' @rdname make_moving_moment
#' @export
#'
make_cumulative_cov <- function(...) {

  calc <- new(ocls_cumulative_cov)
  function(x, y) {
    stopifnot(length(x) == length(y))
    calc$update(x, y)
  }
}

#' Moving z-score algorithm for anomaly detection
#'
#' @param window moving window size
#' @param zscore z-score threshold for signal
#' @param attenu attenuation for signal
#' @param ... not used
#'
#' @return a stateful online function
#' @export
#'
make_moving_zscore <- function(window, zscore, attenu, ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L, zscore > 0.0, 0.0 <= attenu && attenu <= 1.0)

  calc <- new(ocls_moving_zscore, window, zscore, attenu)
  function(x) {
    calc$update(x)
  }
}
