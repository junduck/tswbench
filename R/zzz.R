tus.globals <- new.env()
tus.globals$api_token <- NULL

#fixes for R CMD check
utils::globalVariables(c(
  ".", "Date", "Time", "adj_factor", "date_str", "high", "i.close", "low", "pb",
  "pb_p", "pe", "pe_p", "pe_ttm", "pe_ttm_p", "ps", "ps_p", "ps_ttm", "ps_ttm_p",
  "time_str", "token", "trade_date", "trade_date_posix", "val"
))
