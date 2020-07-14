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
#' @param x variable to average
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
ema <- function(x, period) {

  f <- make_ema(period)
  f(x)
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
dema <- function(x, period) {

  f <- make_dema(period)
  f(x)
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
tema <- function(x, period) {

  f <- make_tema(period)
  f(x)
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

#' @rdname make_ema
#' @export
#'
zlema <- function(x, period) {

  f <- make_zlema(period)
  f(x)
}

#' Wilders smoothing average
#'
#' @param period smoothing period
#' @param ... not used
#' @param x variable to average
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

#' @rdname make_wilders
#' @export
#'
wilders <- function(x, period) {

  f <- make_wilders(period = period)
  f(x)
}

#' Simple moving average
#'
#' @param period MA period
#' @param ... not used
#' @param x variable to average
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

#' @rdname make_sma
#' @export
#'
sma <- function(x, period) {

  f <- make_sma(period = period)
  f(x)
}

#' Weighted moving average
#'
#' @param period MA period
#' @param ... not used
#' @param x variable to average
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

#' @rdname make_wma
#' @export
#'
wma <- function(x, period) {

  f <- make_wma(period = period)
  f(x)
}

#' Kaufman adaptive moving average
#'
#' @param period MA period
#' @param period_short short period for efficiency ratio
#' @param period_long long period for efficiency ratio
#' @param ... not used
#' @param x variable to average
#'
#' @return an online function
#' @export
#'
make_kama <- function(period, period_short = 2L, period_long = 30L, ...) {

  period <- as.integer(period)
  period_short <- as.integer(period_short)
  period_long <- as.integer(period_long)
  stopifnot(period > 0L, period_short > 0L, period_short < period_long)

  calc <- new(ocls_kama, period, period_short, period_long)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_kama
#' @export
#'
kama <- function(x, period, period_short = 2L, period_long = 30L) {

  f <- make_kama(period = period,
                 period_short = period_short,
                 period_long = period_long)
  f(x)
}

#' Hull moving average
#'
#' @param period MA period
#' @param ... not used
#' @param x variable to average
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

#' @rdname make_hma
#' @export
#'
hma <- function(x, period) {

  f <- make_hma(period = period)
  f(x)
}

#' Volume weighted moving average
#'
#' @param period MA period
#' @param ... not used
#' @param price price
#' @param volume volume
#'
#' @return an online function
#' @export
#'
make_vwma <- function(period, ...) {

  period <- as.integer(period)
  stopifnot(period > 0L)

  calc <- new(ocls_vwma, period)
  function(price, volume) {
    calc$update(price, volume)
  }
}

#' @rdname make_vwma
#' @export
#'
vwma <- function(price, volume, period) {

  f <- make_vwma(period = period)
  f(price, volume)
}

#' Variable index dynamic average
#'
#' @param period_short short period for efficiency ratio
#' @param period_long long period for efficiency ratio
#' @param alpha smoothing factor
#' @param x variable to average
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_vidya <- function(period_short = 5, period_long = 20, alpha = 0.2, ...) {

  period_short <- as.integer(period_short)
  period_long <- as.integer(period_long)
  stopifnot(period_short > 0L, period_short < period_long)

  calc <- new(ocls_vidya, period_short, period_long, alpha)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_vidya
#' @export
#'
vidya <- function(x, period_short = 5L, period_long = 20L, alpha = 0.2) {

  f <- make_vidya(period_short = period_short,
                  period_long = period_long,
                  alpha = alpha)
  f(x)
}
