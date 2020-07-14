#' Technical indicators, online version
#'
#' @param period indicator period
#' @param period_short short smoothing period
#' @param period_long long smoothing period
#' @param period_signal signal period
#' @param exp_period exponential smoothing period
#' @param return return value type
#' @param lot volume per lot
#' @param lag lag window
#' @param ma moving average method
#'
#' @return a stateful online function
#'
#' @name online
NULL

#' @rdname online
#' @export
#'
make_macd <- function(period_short = 12L, period_long = 26L, period_signal = 9L, return = c("l", "m")) {

  return <- match.arg(return)
  stopifnot(period_short < period_long)

  n <- as.integer(period_short)
  m <- as.integer(period_long)
  s <- as.integer(period_signal)

  init <- 1L

  ema_short  <- make_ema(n)
  ema_long   <- make_ema(m)
  ema_signal <- make_ema(s)

  switch(return,
         l = function(x) {
           macd   <- ema_short(x) - ema_long(x)
           #wait for long period to generate signals
           if (init < m) {
             signal <- macd
             npt <- length(x)
             gap <- m - init
             if (npt >= gap) {
               init <<- m
               range <- seq.int(gap, npt)
               signal[range] <- ema_signal(macd[range])
             } else {
               init <<- init + npt
             }
           } else {
             signal <- ema_signal(macd)
           }
           list(
             macd   = macd,
             signal = signal,
             histo  = macd - signal
           )
         },
         m = function(x) {
           macd   <- ema_short(x) - ema_long(x)
           #wait for long period to generate signals
           if (init < m) {
             signal <- macd
             npt <- length(x)
             gap <- m - init
             if (npt >= gap) {
               init <<- m
               range <- seq.int(gap, npt)
               signal[range] <- ema_signal(macd[range])
             } else {
               init <<- init + npt
             }
           } else {
             signal <- ema_signal(macd)
           }
           cbind(macd = macd, signal = signal, histo = macd - signal)
         })
}

#' @rdname online
#' @export
#'
make_ad <- function() {

  ad <- 0.0

  function(high, low, close, volume) {

    nh <- length(high)
    nl <- length(low)
    nc <- length(close)
    nv <- length(volume)
    stopifnot(nh == nl && nl == nc && nc == nv)

    hl <- high - low
    delta <- volume * ((close - low) - (high - close)) / hl
    delta[hl == 0.0] <- 0.0
    ans <- ad + cumsum(delta)
    ad <<- ans[nh]

    ans
  }
}

#' @rdname online
#' @export
#'
make_adosc <- function(period_short, period_long) {

  stopifnot(period_short < period_long)

  ema_short <- make_ema(period_short)
  ema_long <- make_ema(period_long)
  ad <- make_ad()

  function(high, low, close, volume) {

    val_ad <- ad(high, low, close, volume)
    val_ema_short <- ema_short(val_ad)
    val_ema_long <- ema_long(val_ad)

    val_ema_short - val_ema_long
  }
}

#' @rdname online
#' @export
#'
make_ao <- function(short_period = 5L, long_period = 34L) {

  short_period <- as.integer(short_period)
  long_period <- as.integer(long_period)
  stopifnot(short_period > 0, short_period < long_period)

  mshort <- make_sma(period = short_period)
  mlong <- make_sma(period = long_period)

  function(high, low) {

    hl <- (high + low) / 2
    mshort(hl) - mlong(hl)
  }
}

#' @rdname online
#' @export
#'
make_arosc <- function(period) {

  period <- as.integer(period)
  stopifnot(period >= 3L)

  imax <- make_moving_argmax(window = period, arg = "distance")
  imin <- make_moving_argmin(window = period, arg = "distance")

  a <- 100.0 / period

  function(high, low) {

    arup   <- a * (period - imax(high))
    ardown <- a * (period - imin(low))

    arup - ardown
  }
}

#' @rdname online
#' @export
#'
make_atr <- function(period) {

  last_close <- NA
  ma <- make_wilders(period = period)

  function(high, low, close) {
    close_shift <- data.table::shift(close, fill = last_close)
    last_close <<- close[length(close)]
    tr <- pmax(high - low, abs(high - close), abs(low - close), na.rm = TRUE)
    ma(tr)
  }
}

#' @rdname online
#' @export
#'
make_cmo <- function(period) {

  period <- as.integer(period)
  stopifnot(period >= 3L)

  mup   <- make_sma(period = period)
  mdown <- make_sma(period = period)
  delta <- make_lag_delta(1L)

  function(x) {
    up <- delta(x)
    if (anyNA(up)) {
      up[is.na(up)] <- 0.0
    }
    down <- -up

    up[up < 0.0]     <- 0.0
    down[down < 0.0] <- 0.0
    up               <- mup(up)
    down             <- mdown(down)

    100 * (up - down) / (up + down)
  }
}

#' @rdname online
#' @export
#'
make_dpo <- function(period) {

  period <- as.integer(period)
  stopifnot(period >= 3L)

  m <- period %/% 2L + 1L
  lag <- make_lag(lag = m)
  ma <- make_sma(period = period)

  function(x) {
    ma(x) - lag(x)
  }
}

#' @rdname online
#' @export
#'
make_emv <- function(lot = 100, lag = 1L) {

  lag <- as.integer(lag)
  stopifnot(lag > 0L, lot > 0)
  delta <- make_lag_delta(lag = lag)

  function(high, low, volume) {
    mid <- (high + low) / 2
    d <- delta(mid)
    r <- volume / (lot * (high - low))
    d / r
  }
}

#' @rdname online
#' @export
#'
make_mass <- function(period, exp_period = 9L) {

  period <- as.integer(period)
  exp_period <- as.integer(exp_period)
  stopifnot(period > 0, exp_period > 0)

  ema1 <- make_ema(period = exp_period)
  ema2 <- make_dema(period = exp_period)
  ma   <- make_sma(period = period)

  function(high, low) {
    d <- high - low
    e <- ema1(d) / ema2(d);
    ma(e)
  }
}

#' @rdname online
#' @export
#'
make_ppo <- function(short_period, long_period, ma = c("ema", "sma")) {

  ma <- match.arg(ma)

  short_period <- as.integer(short_period)
  long_period <- as.integer(long_period)
  stopifnot(short_period > 0, short_period < long_period)

  if (ma == "ema") {
    mshort <- make_ema(period = short_period)
    mlong <- make_ema(period = long_period)
  } else {
    mshort <- make_sma(period = short_period)
    mlong <- make_sma(period = long_period)
  }

  function(x) {
    short <- mshort(x)
    long <- mlong(x)
    100 * (short / long - 1.0)
  }

}

#' @rdname online
#' @export
#'
make_rsi <- function(period) {

  period <- as.integer(period)
  stopifnot(period >= 3L)

  last_x <- NA

  mup   <- make_wilders(period = period)
  mdown <- make_wilders(period = period)

  function(x) {
    up <- x - data.table::shift(x, fill = last_x)
    if (anyNA(up)) {
      up[is.na(up)] <- 0.0
    }
    down <- -up

    up[up < 0.0]     <- 0.0
    down[down < 0.0] <- 0.0
    last_x          <<- x[length(x)]

    100 - 100 / (1 + mup(up) /  mdown(down))
  }
}

#' @rdname online
#' @export
#'
make_willr <- function(period) {

  mmin <- make_moving_min(window = period)
  mmax <- make_moving_max(window = period)

  function(high, low, close) {

    nh <- length(high)
    nl <- length(low)
    nc <- length(close)
    stopifnot(nh == nl && nl == nc)

    min <- mmin(low)
    max <- mmax(high)
    ans <- -100.0 * (max - close) / (max - min)
    ans
  }
}
