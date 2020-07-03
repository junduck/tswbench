#' Online exponential moving average
#'
#' Avalable variances are:
#' make_ema: normal EMA
#' make_dema: double EMA
#' make_tema: triple EMA
#' make_zlema: zero lag EMA
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_ema <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_ema, period)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_ema
#' @export
#'
make_dema <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_dema, period)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_ema
#' @export
#'
make_tema <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_tema, period)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_ema
#' @export
#'
make_zlema <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 1L)

  calc <- new(ocls_zlema, period)
  function(x) {
    calc$update(x)
  }
}

#' Wilders smoothing average
#'
#' @param period smoothing period
#' @param ... not used
#'
#' @return a stateful online function
#' @export
#'
make_wilders <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_wilders, period)
  function(x) {
    calc$update(x)
  }
}

#' Simple moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_sma <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_sma, period)
  function(x) {
    calc$update(x)
  }
}

#' Weighted moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_wma <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_wma, period)
  function(x) {
    calc$update(x)
  }
}

#' Kaufman adaptive moving average
#'
#' @param period MA period
#' @param period_short short period for efficiency ratio
#' @param period_long long period for efficiency ratio
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_kama <- function(period, period_short = 2, period_long = 30, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L, period_short > 0L, period_long > 0L, period_short < period_long, )

  calc <- new(ocls_kama, period, period_short, period_long)
  function(x) {
    calc$update(x)
  }
}

#' Hull moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_hma <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 1L)

  calc <- new(ocls_hma, period)
  function(x) {
    calc$update(x)
  }
}

#' Volume weighted moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_vwma <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_vwma, period)
  function(price, volume) {
    stopifnot(length(price) == length(volume))
    calc$update(price, volume)
  }
}
