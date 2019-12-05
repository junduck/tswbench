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
  #get_intraday
  "trade_date",
  "trade_time",
  "pe_ttm",
  "pb",
  "ps_ttm",
  "pe",
  "ps",
  #get_assets
  "rinc",
  "rroe",
  "n_income",
  "total_assets",
  "total_liab",
  "total_equity"
))
