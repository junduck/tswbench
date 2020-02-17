#' Query intraday OHLC data
#'
#' @param api a tsapi object
#' @param ts_code Tushare code
#' @param start_date OPTIONAL, start time of OHLC
#' @param end_date OPTIONAL, end time of OHLC
#' @param freq frequency of OHLC
#'
#' @return a data.table
#' @export
#'
intraday <- function(api, ts_code, start_date = "", end_date = "", freq = c("1", "5", "15", "60")) {

  freq <- as.character(freq)
  freq <- match.arg(freq)
  freq <- paste0(freq, "min")

  #intraday OHLC
  ohlc <- api$stk_mins(ts_code = ts_code, start_date = start_date, end_date = end_date,
               freq = freq)

  #daily indicator
  n <- nrow(ohlc)
  date <- as.Date(ohlc$trade_time[n])
  indi <- api$daily_basic(ts_code = ts_code, end_date = date,
                          fields = c("trade_date", "close", "pe_ttm", "pb", "ps_ttm"))
  indi[, td_posix := as.POSIXct(trade_date)]
  data.table::setkeyv(indi, "td_posix")

  indi[, pe_p := pe_ttm * close]
  indi[, pb_p := pb     * close]
  indi[, ps_p := ps_ttm * close]

  dt <- indi[ohlc, roll = TRUE]
  dt[, pe := pe_p / i.close]
  dt[, pb := pb_p / i.close]
  dt[, ps := ps_p / i.close]

  dt[, close := i.close]

  data.table::setnames(dt[, c("ts_code", "td_posix", "open", "high", "low", "close", "vol", "amount", "pe", "pb", "ps")],
                       old = "td_posix",
                       new = "trade_time")
}

#' Query daily OHLC data
#'
#' @param api a tsapi object
#' @param ts_code Tushare code
#' @param start_date OPTIONAL, start date of OHLC
#' @param end_date OPTIONAL, end date of OHLC
#' @param adjust Dividen adjustment method
#'
#' @return a data.table
#' @export
#'
daily <- function(api, ts_code, start_date = "", end_date = "", adjust = c("none", "forward", "qfq", "backward", "hfq")) {

  adjust <- match.arg(adjust)

  ohlc <- api$daily(ts_code = ts_code, start_date = start_date, end_date = end_date)
  indi <- api$daily_basic(ts_code = ts_code, start_date = start_date, end_date = end_date)

  if (adjust != "none") {
    f <- api$adj_factor(ts_code = ts_code, start_date = start_date, end_date = end_date)
    if (adjust == "forward" || adjust == "qfq") {
      f[, adj_factor := adj_factor / adj_factor[.N]]
    }
    ohlc <- f[ohlc, roll = TRUE]
    ohlc[, open       := open  * adj_factor]
    ohlc[, high       := high  * adj_factor]
    ohlc[, low        := low   * adj_factor]
    ohlc[, close      := close * adj_factor]
    ohlc[, c("adj_factor", "i.ts_code") := NULL]
  }

  ohlc[indi, ][, c("i.ts_code", "i.close") := NULL]
}

#' Query whole-market daily OHLC data
#'
#' @param api a tsapi object
#' @param trade_date trade date to query
#'
#' @return a data.table
#' @export
#'
mdaily <- function(api, trade_date = Sys.Date()) {

  ohlc <- api$daily(trade_date = trade_date)
  indi <- api$daily_basic(trade_date = trade_date)

  data.table::setkeyv(ohlc, "ts_code")
  data.table::setkeyv(indi, "ts_code")

  ohlc[indi, ][, c("i.trade_date", "i.close") := NULL]
}

#' Perform dividen adjustment on OHLC data
#'
#' @param api a tsapi object
#' @param ohlc a data.table of OHLC data
#' @param adjust Adjustment method
#'
#' @return a data.table
#' @export
#'
adjust_ohlc <- function(api, ohlc, adjust = c("forward", "qfq", "backward", "hfq")) {

  adjust <- match.arg(adjust)

  datetime <- c("trade_date", "trade_time", "date", "time")
  cols <- colnames(ohlc)
  test <- datetime %in% cols
  if (!any(test)) {
    stop("No date/time column found in OHLC data.")
  }
  col <- datetime[test][1]

  f <- api$adj_factor(ts_code    = ohlc$ts_code[1],
                      start_date = ohlc[ 1, ..col][[1]],
                      end_date   = ohlc[.N, ..col][[1]])

  if (adjust == "forward" || adjust == "qfq") {
    f[, adj_factor := adj_factor / adj_factor[.N]]
  }

  dt <- f[ohlc, roll = TRUE]
  dt[, open       := open  * adj_factor]
  dt[, high       := high  * adj_factor]
  dt[, low        := low   * adj_factor]
  dt[, close      := close * adj_factor]
  dt[, c("adj_factor", "i.ts_code") := NULL]

  dt
}

#' Query whole-market end-of-day data by date
#'
#' @param api a tsapi object
#' @param func Tushare function to call, with special function intraday for intraday OHLC
#' @param date date to query
#' @param freq only used when func == "intraday", intraday data frequency
#' @param ... other arguments passed to the query
#'
#' @return a data.table object, keyed by ts_code (and trade_time if the query is intraday)
#' @export
#'
market_eod <- function(api, func, date, freq = c("60", "15", "5", "1"), ...) {

  #deal with special case
  if (func == "index_daily") {
    q_index <- c("000001.SH", "000005.SH", "000006.SH", "000016.SH", "000300.SH", "000905.SH",
                 "399001.SZ", "399005.SZ", "399006.SZ", "399016.SZ", "399300.SZ", "399905.SZ")
    #to maintain consistentcy with index_dailybasic
    tmp <- api$index_dailybasic(trade_date = date, fields = "ts_code")
    q_index <- tmp$ts_code
    dt <- list()
    for (i in seq_along(q_index)) {
      dt[[i]] <- api$index_daily(ts_code = q_index[i], trade_date = date)
    }

    dt <- data.table::rbindlist(dt)
    data.table::setkeyv(dt, "ts_code")

  } else if (func == "intraday") {

    #intraday is tricky
    freq <- as.character(freq)
    freq <- match.arg(freq)
    freq_min <- as.integer(freq)
    freq <- paste0(freq, "min")

    #workaround for tzone issue of Date object when converting to POSIXct
    if (lubridate::is.Date(date)) {
      date <- as.character(date)
    }
    t_date <- lubridate::as_datetime(date, tz = attr(api, "tz"))
    break_noon <- t_date + lubridate::hours(11L) + lubridate::minutes(30L)
    break_day <- t_date + lubridate::hours(15L)

    i <- 0
    dt <- list()

    t <- t_date + lubridate::hours(9L) + lubridate::minutes(30L)
    while (1) {
      i <- i + 1
      dt[[i]] <- api$stk_mins(start_date = t - lubridate::seconds(),
                              end_date   = t + lubridate::seconds(), freq = freq)
      t <- t + lubridate::minutes(freq_min)
      if (t > break_noon) {
        break
      }
    }

    #no data on 13:00:00
    t <- t_date + lubridate::hours(13L) + lubridate::minutes(freq_min)
    while (1) {
      i <- i + 1
      dt[[i]] <- api$stk_mins(start_date = t - lubridate::seconds(),
                              end_date   = t + lubridate::seconds(), freq = freq)
      t <- t + lubridate::minutes(freq_min)
      if (t > break_day) {
        break
      }
    }

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
    f <- do.call("$", list(api, func))
    dt <- do.call(f, args)
    data.table::setkeyv(dt, "ts_code")
  }

  dt
}
