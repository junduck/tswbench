#' Query intraday OHLC data
#'
#' @param ts_code Tushare code
#' @param start_date OPTIONAL, start time of OHLC
#' @param end_date OPTIONAL, end time of OHLC
#' @param freq frequency of OHLC
#' @param api a tsapi object
#'
#' @return a data.table
#' @export
#'
intraday <- function(ts_code, start_date = "", end_date = "", freq = c("1", "5", "15", "60"), api = TushareApi()) {

  freq <- as.character(freq)
  freq <- match.arg(freq)
  freq <- paste0(freq, "min")

  #intraday OHLC
  ohlc <- api$stk_mins(ts_code = ts_code, start_date = start_date, end_date = end_date, freq = freq)
  data.table::setkeyv(ohlc, "trade_time")

  #daily indicator
  n <- nrow(ohlc)
  date_1 <- lubridate::as_date(ohlc$trade_time[1])
  date_n <- lubridate::as_date(ohlc$trade_time[n])
  indi <- api$daily_basic(ts_code = ts_code, start_date = date_1, end_date = date_n,
                          fields = c("trade_date", "close", "pe", "pe_ttm", "pb", "ps", "ps_ttm"))
  indi[, trade_date_posix := as_POSIXct(trade_date, tz = get_tz(api))]
  data.table::setkeyv(indi, "trade_date_posix")

  indi[, pe_p     := pe     * close]
  indi[, pe_ttm_p := pe_ttm * close]
  indi[, pb_p     := pb     * close]
  indi[, ps_p     := ps     * close]
  indi[, ps_ttm_p := ps_ttm * close]

  dt <- indi[ohlc, roll = TRUE]
  dt[, pe     := pe_p     / i.close]
  dt[, pe_ttm := pe_ttm_p / i.close]
  dt[, pb     := pb_p     / i.close]
  dt[, ps     := ps_p     / i.close]
  dt[, ps_ttm := ps_ttm_p / i.close]

  dt[, close := i.close]

  dt <- data.table::setnames(dt[, c("ts_code", "trade_date_posix",
                                    "open", "high", "low", "close", "vol", "amount",
                                    "pe", "pe_ttm", "pb", "ps", "ps_ttm")],
                             old = "trade_date_posix", new = "trade_time")
  data.table::setkeyv(dt, "trade_time")
}

#' Query daily OHLC data
#'
#' @param ts_code Tushare code
#' @param start_date OPTIONAL, start date of OHLC
#' @param end_date OPTIONAL, end date of OHLC
#' @param adjust Dividen adjustment method
#' @param api a tsapi object
#'
#' @return a data.table
#' @export
#'
daily <- function(ts_code, start_date = "", end_date = "", adjust = c("none", "forward", "qfq", "backward", "hfq"), api = TushareApi()) {

  adjust <- match.arg(adjust)

  ohlc <- api$daily(ts_code = ts_code, start_date = start_date, end_date = end_date)
  indi <- api$daily_basic(ts_code = ts_code, start_date = start_date, end_date = end_date)

  if (adjust != "none") {
    f <- api$adj_factor(ts_code = ts_code, start_date = start_date, end_date = end_date)
    if (adjust == "forward" || adjust == "qfq") {
      f[, adj_factor := adj_factor / last(adj_factor)]
    }
    ohlc <- f[ohlc, roll = TRUE, on = "trade_date"]
    ohlc[, open       := open  * adj_factor]
    ohlc[, high       := high  * adj_factor]
    ohlc[, low        := low   * adj_factor]
    ohlc[, close      := close * adj_factor]
    ohlc[, c("i.ts_code") := NULL]
  }

  ohlc[indi, on = "trade_date"][, c("i.ts_code", "i.close") := NULL]
}

#' Query whole-market daily OHLC data
#'
#' @param trade_date trade date to query
#' @param api a tsapi object
#'
#' @return a data.table
#' @export
#'
mdaily <- function(trade_date = Sys.Date(), api = TushareApi()) {

  ohlc <- api$daily(trade_date = trade_date)
  indi <- api$daily_basic(trade_date = trade_date)

  ohlc[indi, on = "ts_code"][, c("i.trade_date", "i.close") := NULL]
}

#' Perform dividen adjustment on OHLC data.
#'
#' Only open, high, low, close columns are ajusted.
#'
#' @param ohlc a data.table of OHLC data
#' @param adjust Adjustment method
#' @param api a tsapi object
#'
#' @return a data.table
#' @export
#'
adjust_ohlc <- function(ohlc, adjust = c("forward", "qfq", "backward", "hfq"), api = TushareApi()) {

  adjust <- match.arg(adjust)

  cols <- names(ohlc)
  test <- stringr::str_detect(cols, "date$|time$")
  if (!any(test)) {
    stop("No date/time column found in OHLC data.", call. = FALSE)
  }
  col <- which(test)[1L]

  ts_code <- unique(ohlc$ts_code)
  if (!length(ts_code)) {
    stop("No ts_code column found in OHLC data.", call. = FALSE)
  }
  if (length(ts_code) > 1L) {
    stop("Non-unique ts_code found in OHLC data.", call. = FALSE)
  }

  f <- api$adj_factor(ts_code    = ts_code,
                      start_date = data.table::first(ohlc[[col]]),
                      end_date   = data.table::last(ohlc[[col]]))

  if (adjust == "forward" || adjust == "qfq") {
    f[, adj_factor := adj_factor / last(adj_factor)]
  }

  dt <- f[ohlc, roll = TRUE]
  dt[, open       := open  * adj_factor]
  dt[, high       := high  * adj_factor]
  dt[, low        := low   * adj_factor]
  dt[, close      := close * adj_factor]
  dt[, c("i.ts_code") := NULL]

  dt
}

#' Query whole-market end-of-day data by date
#'
#' @param func Tushare function to call. Use intraday for intraday data.
#' @param date date to query
#' @param freq only used when func == "intraday", intraday data frequency
#' @param ... other arguments passed to the query
#' @param api a tsapi object
#'
#' @return a data.table object, keyed by ts_code (and trade_time if the query is intraday)
#' @export
#'
market_eod <- function(func, date = Sys.Date(), freq = c("60", "15", "5", "1"), ..., api = TushareApi()) {

  #deal with special case
  if (func == "index_daily") {

    q_index <- c("000001.SH", "000005.SH", "000006.SH", "000016.SH", "000300.SH", "000905.SH",
                 "399001.SZ", "399005.SZ", "399006.SZ", "399016.SZ", "399300.SZ", "399905.SZ")
    #to maintain consistency with index_dailybasic
    tmp <- api$index_dailybasic(trade_date = date, fields = "ts_code")
    q_index <- tmp$ts_code
    dt <- list()
    for (i in seq_along(q_index)) {
      dt[[i]] <- api$index_daily(ts_code = q_index[i], trade_date = date, ...)
    }

    dt <- data.table::rbindlist(dt)
    data.table::setkeyv(dt, "ts_code")

  } else if (func == "intraday") {

    freq     <- as.character(freq)
    freq     <- match.arg(freq)
    freq_sec <- as.integer(freq) * 60L
    freq     <- paste0(freq, "min")

    tl1 <- tl2 <- as.POSIXlt(as_POSIXct(date, tz = get_tz(api)))
    #morning section
    tl1$hour <- 9 ; tl1$min <- 30; tl1$sec <- 0
    tl2$hour <- 11; tl2$min <- 30; tl2$sec <- 0
    t <- seq(unclass(as.POSIXct(tl1)), unclass(as.POSIXct(tl2)), by = freq_sec)
    #shift 12600 (3.5 hr) seconds to get afternoon section
    t <- lubridate::as_datetime(c(t, t + 12600), tz = get_tz(api))

    dt <- list()
    for (i in seq_along(t)) {
      dt[[i]] <- api$stk_mins(start_date = t[i], end_date = t[i] + 1.0, freq = freq, ...)
    }

    dt_idx <- as.logical(sapply(dt, nrow))
    dt <- dt[dt_idx]

    dt <- data.table::rbindlist(dt)
    data.table::setkeyv(dt, c("ts_code", "trade_time"))

  } else {
    dt_arg_name <- switch(func,
                          suspend = "suspend_date",
                          income_vip = ,
                          balancesheet_vip = ,
                          cashflow_vip = ,
                          forecast_vip = ,
                          express_vip = ,
                          dividend = ,
                          fina_indicator_vip = ,
                          fina_mainbz_vip = ,
                          repurchase = ,
                          share_float = ,
                          stk_holdertrade = ,
                          fund_div = ,
                          cb_issue = "ann_date",
                          fund_nav = "end_date",
                          "trade_date")
    #construct args
    args <- list(...)
    args[[dt_arg_name]] <- date
    #construct query function
    f  <- do.call("$", list(api, func))
    dt <- do.call(f, args)
    data.table::setkeyv(dt, "ts_code")
  }

  dt
}
