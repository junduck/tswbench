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
#' cci: Commodity Channel Index
#'
#' cmo: Chande Momentum Oscillator
#'
#' dpo: Detrended Price Oscillator
#'
#' emv: Ease of Movement
#'
#' mass: Mass Index
#'
#' mfi: Money Flow Index
#'
#' obv: On Balance Volume
#'
#' pnvi: Positive/Negative Volume Index
#'
#' ppo: Percentage Price Oscillator
#'
#' rsi: Relative Strength Index
#'
#' trix: Trix Momentum Indicator
#'
#' willr: Williams R
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

#' Commodity Channel Index
#'
#' @param x measured variable / typical price
#' @param period period
#' @param mdr a ratio to adjust ±100 region
#'
#' @return numeric vector
#' @export
#'
cci <- function(x, period, mdr = 0.015) {

  f <- make_cci(period = period, mdr = mdr)
  f(x)
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

#' Money Flow Index
#'
#' @param price price
#' @param volume volume
#' @param period period
#'
#' @return numeric vector
#' @export
#'
mfi <- function(price, volume, period) {

  f <- make_mfi(period)
  f(price, volume)
}

#' On Balance Volume
#'
#' @param close close
#' @param volume volume
#'
#' @return numeric vector
#' @export
#'
obv <- function(close, volume) {

  f <- make_obv()
  f(close, volume)
}

#' Positive/Negative Volume Index
#'
#' @param close close
#' @param volume volume
#' @param base_index base index of the first bar
#' @param return return data type, l for list, m for matrix
#'
#' @return a list or matrix
#' @export
#'
pnvi <- function(close, volume, base_index = 1000.0, return = c("l", "m")) {

  f <- make_pnvi(base_index = base_index, return = return)
  f(close, volume)
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

#' Trix Momentum Indicator
#'
#' @param x measured variable
#' @param period period
#'
#' @return numeric vector
#' @export
#'
trix <- function(x, period) {
  f <- make_trix(period = period)
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
