tus.globals <- new.env()
tus.globals$api_token <- NULL
tus.globals$date_fmt <- "%Y%m%d"
tus.globals$time_fmt <- "%Y-%m-%d %H:%M:%S"

#fixes for R CMD check
utils::globalVariables(c(
  "update_flag",
  "end_date",
  "..cidx",
  "adj_factor",
  "i.ts_code",
  "open",
  "high",
  "low",
  "close",
  #dapdv
  "roe",
  "pb",
  "pe_ttm",
  #adjust_ohlc
  "..col",
  #intraday
  "td_posix",
  "trade_date",
  "pe_p",
  "pb_p",
  "ps_p",
  "ps_ttm",
  "pe",
  "i.close",
  "ps",
  "trade_time"
))
