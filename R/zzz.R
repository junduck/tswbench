tus.globals <- new.env()
tus.globals$api_token <- NULL

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

.onUnload <- function(libpath) {

  if (!is.null(tus.globals$sina_handle)) {
    curl::handle_reset(tus.globals$sina_hanlde)
  }
}
