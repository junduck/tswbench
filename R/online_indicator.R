#' Moving average convergence/divergence
#'
#' NOTICE: Traditionally, 0.15 and 0.075 are used as 12-day and 26-day EMA factor,
#' however, this is convention is not used in make_macd. 2 / (period + 1) is used
#' instead for consistency.
#'
#' @param period_short short EMA period
#' @param period_long long EMA period
#' @param period_signal signal EMA period
#' @param return l for a named list, m for matrix
#'
#' @return a stateful online function
#' @export
#'
make_macd <- function(period_short = 12L, period_long = 26L, period_signal = 9L, return = c("l", "m")) {

  return <- match.arg(return)

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
             npt <- length(x)
             signal <- macd
             for (i in seq_len(npt)) {
               if (init < m) {
                 init <<- init + 1L
               } else {
                 signal[i] <- ema_signal(macd[i])
               }
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
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 3L)
           colnames(ans) <- c("macd", "signal", "histo")
           ans[, 1L] <- ema_short(x) - ema_long(x)
           #wait for long period to generate signals
           if (init < m) {
             npt <- length(x)
             ans[, 2L] <- ans[, 1L]
             for (i in seq_len(npt)) {
               if (init < m) {
                 init <<- init + 1L
               } else {
                 ans[i, 2L] <- ema_signal(ans[i, 1L])
               }
             }
           } else {
             ans[, 2L] <- ema_signal(ans[, 1L])
           }
           ans[, 3L] <- ans[, 1L] - ans[, 2L]
           ans
         })
}

make_willr <- function(period) {

  mmin <- make_moving_min(window = period)
  mmax <- make_moving_max(window = period)

  function(high, low, close) {
    nh <- length(high)
    nl <- length(low)
    nc <- length(close)
    if (nh == nl && nl == nc) {
      min <- mmin(low)
      max <- mmax(high)
      ans <- -100.0 * (max - close) / (max - min)
    } else {
      stop("Arguments are of different lengths", call. = FALSE)
    }
    ans
  }
}

make_ad <- function() {

  ad <- 0.0

  function(high, low, close, volume) {
    nh <- length(high)
    nl <- length(low)
    nc <- length(close)
    nv <- length(volume)
    if (nh == nl && nl == nc && nc == nv) {
      delta <- volume * ((close - low) - (high - close)) / (high - low)
      delta[is.nan(delta) | is.infinite(delta)] <- 0.0
      ans <- vector(mode = "numeric", length = nh)
      for (i in seq_len(nh)) {
        ad <<- ad + delta[i]
        ans[i] <- ad
      }
    } else {
      stop("Arguments are of different lengths", call. = FALSE)
    }
    ans
  }
}

make_adosc <- function(period_short, period_long) {

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
