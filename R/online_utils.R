new_lag <- function(lag) new(ocls_lag, as.integer(lag))
new_lag_delta <- function(lag) new(ocls_lag_delta, as.integer(lag))
new_lag_ratio <- function(lag) new(ocls_lag_ratio, as.integer(lag))

#' Calculate lagged element/delta/ratio
#'
#' @param lag lag period
#'
#' @return a stateful online function
#' @export
#'
make_lag <- function(lag) {

  lag <- as.integer(lag)
  stopifnot(lag > 0L)

  calc <- new(ocls_lag, lag)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_lag
#' @export
#'
make_lag_delta <- function(lag) {

  lag <- as.integer(lag)
  stopifnot(lag > 0L)

  calc <- new(ocls_lag_delta, lag)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_lag
#' @export
#'
make_lag_ratio <- function(lag) {

  lag <- as.integer(lag)
  stopifnot(lag > 0L)

  calc <- new(ocls_lag_ratio)
  function(x) {
    calc$update(x)
  }
}

lag_delta <- function(x, lag) {

  f <- make_lag_delta(lag)
  f(x)
}

lag_ratio <- function(x, lag) {

  f <- make_lag_ratio(lag)
  f(x)
}
