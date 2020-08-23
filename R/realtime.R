curl_get_plaintext <- function(url, handle, encoding) {

  req <- curl::curl_fetch_memory(url, handle)
  if (req$status_code != 200) stop("Failed to fetch data.", call. = FALSE)

  stringr::str_conv(req$content, encoding = encoding)
}

#' Query function generator
#'
#' Query values using HTTP GET. Expected returned values are Javascript assignments
#' of var_prefix_var_name = "string value"
#'
#' string values are split and parsed as tabular data.
#'
#' @param baseurl base URL
#' @param rand_var random variable name
#' @param code_var code variable name
#' @param encode content encoding
#' @param max_batch max number of codes per batch
#' @param var_prefix Javascript assignment variable prefix
#' @param var_name variable column name
#' @param split Javascript assignment value delimiter
#' @param cols column names of the returned data.table
#'
#' @return a function
realtime_js_str_var <- function(baseurl, rand_var, code_var, encode, max_batch, var_prefix, var_name, split, cols) {

  if (endsWith(baseurl, "/")) {
    url <- paste0(baseurl, rand_var, "=%.0f&", code_var, "=%s")
  } else {
    url <- paste0(baseurl, "/", rand_var, "=%.0f&", code_var, "=%s")
  }

  # Match js str var assignment: [var_prefix]var_name = "string value"
  ptn <- sprintf('%s(.*?)\\s*=\\s*"(.*?)"', var_prefix)

  keep_cols <- seq_along(cols)

  function(code) {
    handle = curl::new_handle()
    code_part <- split(code, seq_along(code) %/% max_batch)
    req_ans <- sapply(code_part, function(codes) {
      req_url <- sprintf(url, unclass(Sys.time()) * 1000, paste0(codes, collapse = ","))
      curl_get_plaintext(req_url, handle, encode)
    }, USE.NAMES = FALSE)
    data <- req_ans %>%
      stringr::str_match_all(., pattern = ptn) %>%
      do.call(rbind, .)
    dt <- data.table::tstrsplit(data[, 3L], split = split, fixed = TRUE, keep = keep_cols)
    data.table::setDT(dt)
    data.table::setnames(dt, cols)
    data.table::set(dt, j = var_name, value = data[, 2L])
  }
}

sina_realtime_quote_cols <- c(
  # meta
  "Name",
  # ohlc
  "Open", "PreClose", "Price", "High", "Low", "Bid", "Ask", "Vol", "Tnvr",
  # bid and ask
  "Bid_V1", "Bid_P1", "Bid_V2", "Bid_P2", "Bid_V3", "Bid_P3", "Bid_V4", "Bid_P4", "Bid_V5", "Bid_P5",
  "Ask_V1", "Ask_P1", "Ask_V2", "Ask_P2", "Ask_V3", "Ask_P3", "Ask_V4", "Ask_P4", "Ask_V5", "Ask_P5",
  # timestamps
  "date_str", "time_str"
)

sina_realtime_quote_num_cols <- c(
  # ohlc
  "Open", "PreClose", "Price", "High", "Low", "Bid", "Ask", "Vol", "Tnvr",
  # bid and ask
  "Bid_V1", "Bid_P1", "Bid_V2", "Bid_P2", "Bid_V3", "Bid_P3", "Bid_V4", "Bid_P4", "Bid_V5", "Bid_P5",
  "Ask_V1", "Ask_P1", "Ask_V2", "Ask_P2", "Ask_V3", "Ask_P3", "Ask_V4", "Ask_P4", "Ask_V5", "Ask_P5"
)

sina_realtime_quote_data <- realtime_js_str_var(baseurl = "http://hq.sinajs.cn/",
                                                rand_var = "rn",
                                                code_var = "list",
                                                encode = "GB18030",
                                                max_batch = 750L,
                                                var_prefix = "hq_str_",
                                                var_name = "sina_code",
                                                split = ",",
                                                cols = sina_realtime_quote_cols)

#' Query realtime quotes from Sina
#'
#' @param sina_code Sina codes to query
#' @param api an tsapi object
#'
#' @return data.table
#' @export
#'
sina_realtime_quote <- function(sina_code, api = TushareApi()) {

  dt <- sina_realtime_quote_data(sina_code)
  dt[, (sina_realtime_quote_num_cols) := lapply(.SD, as.numeric), .SDcols = sina_realtime_quote_num_cols]
  parse_datetime <- datetime_parser(api)
  dt[, Time := parse_datetime(paste0(date_str, time_str))]
  dt[, c("date_str", "time_str") := NULL]

  dt
}

tencent_realtime_quote_cols_all <- c(
  "MktId", "Name", "Code", "Price", "PreClose", "Open", "Vol", "Bid_Vol", "Ask_Vol",
  "Bid_P1", "Bid_V1", "Bid_P2", "Bid_V2", "Bid_P3", "Bid_V3", "Bid_P4", "Bid_V4", "Bid_P5", "Bid_V5",
  "Ask_P1", "Ask_V1", "Ask_P2", "Ask_V2", "Ask_P3", "Ask_V3", "Ask_P4", "Ask_V4", "Ask_P5", "Ask_V5",
  "Detail", "Time", "Change", "ChangePct", "High", "Low",
  "PriceVolTnvr", "Vol_Duplicate", "TnvrDisp", "TnvrPct", "PE", "V41_Unknow_Status", "High_Duplicate",
  "Low_Duplicate", "AmpPct", "FreeFloat", "MktCap", "PB", "HighLimit", "LowLimit",
  "OrderVolRatio", "OrderValSpread", "VWAP", "PE_Dynamic", "PE_Static", "V55_Unknow", "V56_Unknow",
  "V57_Unknow", "Tnvr", "V59_Unknow", "V60_Unknow", "V61_Unknow", "MktTag", "V63_Unknow",
  "V64_Unknow", "V65_Unknow", "V66_Unknow", "V67_Unknow"
)

# Keep till MktTag
tencent_realtime_quote_cols <- tencent_realtime_quote_cols_all[seq_len(62L)]

tencent_realtime_quote_parsed <- c(
  # Meta
  "Name", "Code",
  # Quote
  "Open", "PreClose", "Price", "High", "Low", "Vol", "Bid_Vol", "Ask_Vol", "Tnvr", "TnvrPct",
  "HighLimit", "LowLimit", "Change", "ChangePct", "AmpPct", "VWAP",
  # Bid/Ask
  "Bid_P1", "Bid_V1", "Bid_P2", "Bid_V2", "Bid_P3", "Bid_V3", "Bid_P4", "Bid_V4", "Bid_P5", "Bid_V5",
  "Ask_P1", "Ask_V1", "Ask_P2", "Ask_V2", "Ask_P3", "Ask_V3", "Ask_P4", "Ask_V4", "Ask_P5", "Ask_V5",
  # Fundamental
  "MktCap", "FreeFloat", "PE", "PE_Dynamic", "PE_Static", "PB",
  # Timestamp
  "Time",
  # misc
  "MktTag", "tencent_code",
  # Placeholder
  NULL
)

# Regarding to number unit:
# Vol: lot (100 shares)
# Tnvr: 100k
# *Pct: %
tencent_realtime_quote_num <- c(
  # Quote
  "Open", "PreClose", "Price", "High", "Low", "Vol", "Bid_Vol", "Ask_Vol", "Tnvr", "TnvrPct",
  "HighLimit", "LowLimit", "Change", "ChangePct", "AmpPct", "VWAP",
  # Bid/Ask
  "Bid_P1", "Bid_V1", "Bid_P2", "Bid_V2", "Bid_P3", "Bid_V3", "Bid_P4", "Bid_V4", "Bid_P5", "Bid_V5",
  "Ask_P1", "Ask_V1", "Ask_P2", "Ask_V2", "Ask_P3", "Ask_V3", "Ask_P4", "Ask_V4", "Ask_P5", "Ask_V5",
  # Fundamental
  "MktCap", "FreeFloat", "PE", "PE_Dynamic", "PE_Static", "PB",
  # Placeholder
  NULL
)

tencent_realtime_quote_data <- realtime_js_str_var(baseurl = "http://qt.gtimg.cn/utf8",
                                                   rand_var = "_",
                                                   code_var = "q",
                                                   encode = "UTF-8",
                                                   max_batch = 750L,
                                                   var_prefix = "v_",
                                                   var_name = "tencent_code",
                                                   split = "~",
                                                   cols = tencent_realtime_quote_cols)

#' Query realtime quotes from Tencent
#'
#' Tencent quotes format is not full understood yet. Do not use.
#'
#' @param tencent_code Tencent codes to query
#' @param api as tsapi object
#'
#' @return data.table
#' @export
#'
tencent_realtime_quote <- function(tencent_code, api = TushareApi()) {

  dt <- tencent_realtime_quote_data(tencent_code)
  dt[, (tencent_realtime_quote_num) := lapply(.SD, as.numeric), .SDcols = tencent_realtime_quote_num]
  parse_datetime <- datetime_parser(api)
  dt[, Time := parse_datetime(Time)]

  # Only return parsed columns
  dt[, tencent_realtime_quote_parsed, with = FALSE]
}

tencent_realtime_mf_cols <- c(
  "code",
  "main_buy", "main_sale", "main_net", "main_ratio",
  "chive_buy", "chive_sale", "chive_net", "chive_ratio", "tot_buy",
  "V11_Unknow", "V12_Unknow", "Name", "Date", "Hist1", "Hist2", "Hist3", "Hist4")

tencent_realtime_mf_num_cols <- c(
  "main_buy", "main_sale", "main_net", "main_ratio",
  "chive_buy", "chive_sale", "chive_net", "chive_ratio", "tot_buy"
)

tencent_realtime_mf_data <- realtime_js_str_var(baseurl = "http://qt.gtimg.cn/utf8",
                                                rand_var = "_",
                                                code_var = "q",
                                                encode = "UTF-8",
                                                max_batch = 500L,
                                                var_prefix = "v_ff_",
                                                var_name = "tencent_code",
                                                split = "~",
                                                cols = tencent_realtime_mf_cols)

#' Query realtime moneyflow data from Tencent
#'
#' There is no timestamp in the returned data, so this moneyflow data can't be
#' used reliably as time-series
#'
#' @param tencent_code Tencent codes to query
#' @param api an tsapi object
#'
#' @return data.table
#' @export
#'
tencent_realtime_moneyflow <- function(tencent_code, api = TushareApi()) {

  dt <- tencent_realtime_mf_data(paste0("ff_", tencent_code))
  dt[, (tencent_realtime_mf_num_cols) := lapply(.SD, as.numeric), .SDcol = tencent_realtime_mf_num_cols]
  parse_date <- date_parser(api)
  dt[, Date := parse_date(Date)]

  dt
}
