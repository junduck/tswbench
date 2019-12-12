
sina_numeric_columns <- c(#ohlc
                          "today_open", "pre_close", "last", "today_high", "today_low", "bid", "ask", "vol", "amount",
                          #bid and ask
                          "bid1_vol", "bid1", "bid2_vol", "bid2", "bid3_vol", "bid3", "bid4_vol", "bid4", "bid5_vol", "bid5",
                          "ask1_vol", "ask1", "ask2_vol", "ask2", "ask3_vol", "ask3", "ask4_vol", "ask4", "ask5_vol", "ask5")

sina_realtime_parse_row <- function(row) {
  #datetime
  if (is.na(row[31]) || is.na(row[32])) {
    row[31] <- NA
  } else {
    row[31] <- paste(row[31], row[32])
  }
  #everything else
  list(
    name       = row[1],
    today_open = row[2],
    pre_close  = row[3],
    last       = row[4],
    today_high = row[5],
    today_low  = row[6],
    bid        = row[7],
    ask        = row[8],
    vol        = row[9],
    amount     = row[10],
    bid1_vol   = row[11],
    bid1       = row[12],
    bid2_vol   = row[13],
    bid2       = row[14],
    bid3_vol   = row[15],
    bid3       = row[16],
    bid4_vol   = row[17],
    bid4       = row[18],
    bid5_vol   = row[19],
    bid5       = row[20],
    ask1_vol   = row[21],
    ask1       = row[22],
    ask2_vol   = row[23],
    ask2       = row[24],
    ask3_vol   = row[25],
    ask3       = row[26],
    ask4_vol   = row[27],
    ask4       = row[28],
    ask5_vol   = row[29],
    ask5       = row[30],
    trade_time = row[31]
  )
}

sina_realtime_parse <- function(data, dt_cast) {

  ans <- data %>%
    #split by comma
    strsplit(., ",", fixed = TRUE) %>%
    #parse by row
    lapply(., sina_realtime_parse_row) %>%
    #build data.table
    data.table::rbindlist(.)

  #convert data type
  ans[, (sina_numeric_columns) := lapply(.SD, as.numeric), .SDcols = sina_numeric_columns]
  ans[, trade_time := dt_cast(trade_time)]

  ans
}

#' Query Sina realtime quotes
#'
#' Jun's note: It takes about 2 secs to query whole market here in Australia and
#' time slice of Sina L1 data is about 3 secs.
#'
#' @param api A tsapi object
#' @param sina_code Sina codes to query quotes
#'
#' @return a data.table
#' @export
#'
sina_realtime <- function(api = TushareApi(), sina_code) {

  n <- length(sina_code) %/% 800L + 1L
  code_part <- suppressWarnings(split(sina_code, f = seq_len(n)))

  dt_cast <- cast_time(api)
  base_url <- "http://hq.sinajs.cn/rn=%0.f&list=%s"

  ans <- lapply(code_part, function(codes) {
    #concatenate codes by comma and construct request url
    req_url <- sprintf(base_url, unclass(Sys.time()), paste0(codes, collapse = ","))
    req_ans <- httr::GET(req_url) %>%
      httr::content(., as = "text") %>%
      #extract in between quotes
      stringr::str_extract_all(., pattern = '(?<=")(.*?)(?=")') %>%
      magrittr::extract2(1L) %>%
      #parse data
      sina_realtime_parse(., dt_cast)
    #put sina_code in data.table
    req_ans[, sina_code := codes]

    req_ans
  })

  ans <- data.table::rbindlist(ans)
  data.table::setkeyv(ans, cols = "sina_code")

  ans
}

#' Convert symbols to Sina codes
#'
#' @param symbol a vector of character symbols
#'
#' @return a vector of Sina codes
#' @export
#'
get_sina_code <- function(symbol) {

  #Jun's note: this method does not work for netease symbols which are [0,1][0-9]{6}

  #extract 6 digits
  ans <- stringr::str_extract(symbol, "[0-9]{6}")
  #attach prefix
  is_shanghai <- startsWith(ans, "6")
  ans[ is_shanghai] <- paste0("sh", ans[ is_shanghai])
  ans[!is_shanghai] <- paste0("sz", ans[!is_shanghai])

  ans
}
