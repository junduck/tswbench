adjust_ohlc <- function(ohlc, adj_factors = NULL, api = NULL,
                        adjust = c("forward", "qfq", "backward", "hfq")) {

  adjust <- match.arg(adjust)

  if (is.null(adj_factors)) {
    if (is.null(api)) {
      stop("Either adj_factors or api is required.")
    }
    cols <- colnames(ohlc)
    test <- c("trade_date", "trade_time", "date", "time")
    col <- NULL
    for (elm in test) {
      if (elm %in% cols) {
        col <- elm
        break
      }
    }
    if (is.null(col)) {
      stop("No date/time column found in OHLC data.")
    }
    f <- api$adj_factor(ts_code    = ohlc$ts_code[1],
                        start_date = min(ohlc[[col]]),
                        end_date   = max(ohlc[[col]]))
  } else {
    f <- data.table::copy(adj_factors)
  }

  if (adjust == "forward" || adjust == "qfq") {
    f[, adj_factor := adj_factor / adj_factor[.N]]
  }

  dt <- f[ohlc, roll = TRUE]
  dt[, open       := open  * adj_factor]
  dt[, high       := high  * adj_factor]
  dt[, low        := low   * adj_factor]
  dt[, close      := close * adj_factor]
  dt[, adj_factor := NULL]
  dt[, i.ts_code  := NULL]

  dt
}
