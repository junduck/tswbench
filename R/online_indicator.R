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
