#' Technical indicators
#'
#' Various technical indicators and their online versions are provided:
#'
#' macd: 	Moving Average Convergence/Divergence
#'
#' ad: Accumulation/Distribution Line
#'
#' adosc: Accumulation/Distribution Oscillator
#'
#' ao: Awesome Oscillator
#'
#' arosc: Aroon Oscillator
#'
#' atr: Average True Range
#'
#' cmo: Chande Momentum Oscillator
#'
#' dpo: Detrended Price Oscillator
#'
#' emv: Ease of Movement
#'
#' mass: Mass Index
#'
#' ppo: Percentage Price Oscillator
#'
#' rsi: Relative Strength Index
#'
#' willr: Williams %R
#'
#'
#' @name indicators
NULL

#' Moving average convergence/divergence
#'
#' NOTICE: Traditionally, 0.15 and 0.075 are used as 12-day and 26-day EMA factor,
#' however, this is convention is not used in make_macd. 2 / (period + 1) is used
#' instead for consistency.
#'
#' @param x measured variable
#' @param period_short short EMA period
#' @param period_long long EMA period
#' @param period_signal signal EMA period
#' @param return l for a named list, m for matrix
#'
#' @return numeric vector
#' @export
#'
macd <- function(x, period_short = 12L, period_long = 26L, period_signal = 9L, return = c("l", "m")) {

  f <- make_macd(period_short = period_short,
                 period_long = period_long,
                 period_signal = period_signal,
                 return = return)
  f(x)
}

#' Accumulation/Distribution Line
#'
#' @param high high
#' @param low low
#' @param close close
#' @param volume volume
#'
#' @return numeric vector
#' @export
#'
ad <- function(high, low, close, volume) {

  f <- make_ad()
  f(high, low, close, volume)
}

#' Accumulation/Distribution Oscillator
#'
#' @param high high
#' @param low low
#' @param close close
#' @param volume volume
#' @param period_short short smoothing period
#' @param period_long long smoothing period
#'
#' @return numeric vector
#' @export
#'
adosc <- function(high, low, close, volume, period_short, period_long) {

  f <- make_adosc(period_short = period_short, period_long = period_long)
  f(high, low, close, volume)
}

#' Awesome Oscillator
#'
#' @param high high
#' @param low low
#' @param short_period short smoothing period
#' @param long_period long smoothing period
#'
#' @return numeric vector
#' @export
#'
ao <- function(high, low, short_period = 5L, long_period = 34L) {

  f <- make_ao(short_period = short_period, long_period = long_period)
  f(high, low)
}

#' Aroon Oscillator
#'
#' @param high high
#' @param low low
#' @param period period
#'
#' @return numeric vector
#' @export
#'
arosc <- function(high, low, period) {

  f <- make_arosc(period = period)
  f(high, low)
}

#' Average True Range
#'
#' @param high high
#' @param low low
#' @param close close
#' @param period period
#'
#' @return numeric vector
#' @export
#'
atr <- function(high, low, close, period) {

  f <- make_atr(period = period)
  f(high, low, close)
}

#' Chande Momentum Oscillator
#'
#' @param x measured variable
#' @param period period
#'
#' @return numeric vector
#' @export
#'
cmo <- function(x, period) {

  f <- make_cmo(period = period)
  f(x)
}

#' Detrended Price Oscillator
#'
#' @param x measured variable
#' @param period period
#'
#' @return numeric vector
#' @export
#'
dpo <- function(x, period) {

  f <- make_dpo(period = period)
  f(x)
}

#' Ease of Movement
#'
#' @param high high
#' @param low low
#' @param volume volume
#' @param lot volume per lot
#' @param lag lag window
#'
#' @return numeric vector
#' @export
#'
emv <- function(high, low, volume, lot = 100, lag = 1L) {

  f <- make_emv(lot = lot, lag = lag)
  f(high, low, volume)
}

#' Mass Index
#'
#' @param high high
#' @param low low
#' @param period period
#' @param exp_period exponential smoothing period
#'
#' @return numeric vector
#' @export
#'
mass <- function(high, low, period, exp_period = 9L) {

  f <- make_mass(period = period, exp_period = exp_period)
  f(high, low)
}

#' Percentage Price Oscillator
#'
#' @param x measured variable
#' @param short_period short smoothing period
#' @param long_period long smoothing period
#' @param ma moving average method
#'
#' @return numeric vector
#' @export
#'
ppo <- function(x, short_period, long_period, ma = c("ema", "sma")) {

  f <- make_ppo(short_period = short_period, long_period = long_period, ma = ma)
  f(x)
}

#' Relative Strength Index
#'
#' @param x measured variable
#' @param period period
#'
#' @return numeric vector
#' @export
#'
rsi <- function(x, period) {

  f <- make_rsi(period = period)
  f(x)
}

#' Williams %R
#'
#' @param high high
#' @param low low
#' @param close close
#' @param period period
#'
#' @return numeric vector
#' @export
#'
willr <- function(high, low, close, period) {

  f <- make_willr(period)
  f(high, low, close)
}
