
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
  data.table::setkeyv(ans, cols = c("sina_code", "trade_time"))

  ans
}

#' A simple timed loop to process Sina realtime quote
#'
#' @param api A tsapi object
#' @param sina_code A vector of Sina codes
#' @param data_handler A data handler function, that accepts a data.table as input
#' @param data_writer A data writer function to write recieved Sina quotes
#' @param result_reporter A function to report results from data_handler
#' @param incremental Whether to pass incremental data to data_handler. If TRUE, only new recieved data is passed, otherwise accumulated history data is passed.
#' @param combined Whether to pass all quotes in sina_code combined. If TRUE all quotes are passed to data_handler, otherwise quotes are passed to data_handler grouped by their sina_code.
#' @param walltime Timeout for each loop. Sina updates level 1 quotes on a 3 seconds basis.
#'
#' @return a worker function
#' @export
#'
#' @examples
#' codes <- c("sz000001", "sh600000")
#' worker <- sina_realtime_worker(sina_code = codes, data_handler = example_data_handler, result_reporter = example_result_reporter, combined = TRUE, walltime = 3)
sina_realtime_worker <- function(api = TushareApi(), sina_code, data_handler,
                                 data_writer = NULL, result_reporter = NULL,
                                 incremental = TRUE, combined = FALSE, walltime = 3) {

  #fix Sina codes
  sina_code <- get_sina_code(sina_code)

  #match functions
  data_handler <- match.fun(data_handler)
  if (is.null(data_writer)) {
    data_writer <- function(...) NULL
  } else {
    data_writer <- match.fun(data_writer)
  }
  if (is.null(result_reporter)) {
    result_reporter <- function(...) NULL
  } else {
    result_reporter <- match.fun(result_reporter)
  }

  worker <- function() {

    #defer reset time limit to default
    on.exit({
      setTimeLimit()
    })

    #call Sina once to establish connection
    old_frame <- sina_realtime(api, sina_code)[0, ]
    uni_frame <- old_frame
    inc_frame <- old_frame

    while (TRUE) {

      #set time limit for current loop
      setTimeLimit(elapsed = walltime, transient = TRUE)

      #loop timer
      t_loop <- Sys.time()

      tryCatch({

        #query data from Sina
        new_frame <- sina_realtime(api = api, sina_code = sina_code)

        #parse incremental data
        inc_frame <- data.table::fsetdiff(new_frame, old_frame)
        old_frame <- new_frame

        if (nrow(inc_frame)) {

          if (incremental) {
            #only incremental data is passed to data_handler
            if (combined) {
              result <- do.call(data.table::data.table, data_handler(inc_frame))
            } else {
              result <- inc_frame[, data_handler(.SD), by = name]
            }
          } else {
            #all data since loop is passed to data_handler
            uni_frame <- data.table::funion(uni_frame, inc_frame)
            data.table::setkeyv(uni_frame, c("sina_code", "trade_time"))
            if (combined) {
              result <- do.call(data.table::data.table, data_handler(uni_frame))
            } else {
              result <- uni_frame[, data_handler(.SD), by = name]
            }
          }

          #add timestamp to result
          result[, timestamp := Sys.time()]
          #report result
          result_reporter(result)
          #write data
          data_writer(inc_frame)
        }

        #check loop time if walltime is not set to Inf
        if (!is.infinite(walltime)) {
          t_delta <- Sys.time() - t_loop
          t_remain <- walltime - t_delta
          Sys.sleep(t_remain)
        }

      }, error = function(err) {
        msg <- err$message
        if (msg != "reached elapsed time limit") {
          stop(err, call. = FALSE)
        }
      })
    }
  }

  worker
}

#' Generate a simple csv writer for sina_realtime_worker()
#'
#' @param file output csv file path
#'
#' @return a data_write function that appends Sina quote data to csv file
#' @export
#'
#' @examples
#' data_writer <- csv_data_write("records.csv")
csv_data_writer <- function(file = tempfile(fileext = "csv")) {

  data_writer <- function(frame) {
    data.table::fwrite(frame, file = file, append = TRUE)
  }

  data_writer
}

#' An example result_reporter for sina_realtime_worker()
#'
#' @param result result passed by worker
#'
#' @return NULL
#' @export
#'
example_result_reporter <- function(result) {

  for (i in seq_len(nrow(result))) {
    msg <- sprintf("[%s] %sing %s at price %.3f", result$timestamp[i], result$side[i], result$code[i], result$price[i])
    print(msg)
  }

  NULL
}

#' An example data_handler for sina_realtime_worker()
#'
#' @param frame a frame of realtime quote data passed by worker
#'
#' @return a named list, which is further converted to a data.table then passed to result_reporter
#' @export
#'
example_data_handler <- function(frame) {

  #pick a random side
  side <- if (runif(1) >= 0.5) "SELL" else "BUY"
  #pick a random stock to buy/sell
  idx <- sample(nrow(frame), 1)
  code <- frame$sina_code[idx]
  price <- frame$last[idx]

  list(side = side, code = code, price = price)
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
