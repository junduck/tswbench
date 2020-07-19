#' Calculate lagged element/delta/ratio
#'
#' @param lag lag period
#' @param na_fill filled value when lagged data is not available
#' @param window moving window size
#'
#' @return a stateful online function
#' @export
#'
make_lag <- function(lag, na_fill = NA) {

  lag <- as.integer(lag)
  stopifnot(lag > 0L)

  calc <- new(ocls_lag, lag, na_fill)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_lag
#' @export
#'
make_lag_delta <- function(lag, na_fill = NA) {

  lag <- as.integer(lag)
  stopifnot(lag > 0L)

  calc <- new(ocls_lag_delta, lag, na_fill)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_lag
#' @export
#'
make_lag_ratio <- function(lag, na_fill = NA) {

  lag <- as.integer(lag)
  stopifnot(lag > 0L)

  calc <- new(ocls_lag_ratio, lag, na_fill)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_lag
#' @export
#'
make_lag_delta_moving_sum <- function(window, lag) {

  window <- as.integer(window)
  stopifnot(window > 0L)

  lag <- as.integer(lag)
  stopifnot(lag > 0L)

  calc <- new(ocls_lag_delta_moving_sum, window, lag)
  function(x) {
    calc$update(x)
  }
}

#' Calculate lagged element/delta/ratio
#'
#' @param x measured variable
#' @param lag lag period
#' @param na_fill filled value when lagged data is not available
#' @param window moving window size
#'
#' @return
#' @export
#'
#' @examples
lag_delta <- function(x, lag, na_fill = NA) {

  f <- make_lag_delta(lag = lag, na_fill = na_fill)
  f(x)
}

#' @rdname lag_delta
#' @export
#'
lag_ratio <- function(x, lag, na_fill = NA) {

  f <- make_lag_ratio(lag = lag, na_fill = na_fill)
  f(x)
}

#' @rdname lag_delta
#' @export
#'
lag_delta_moving_sum <- function(x, window, lag) {

  f <- make_lag_delta_moving_sum(window = window, lag = lag)
  f(x)
}
