split_dict_values <- function(rec) {

  #Tushare Sub returns dict_values(['val1', 'val2', ... , 'valn'])

  #dict_values can contain python None value, evaluate None as NA
  None <- NA
  expr_text <- paste0("c(", stringr::str_sub(rec, start = 14L, end = -3L), ")")

  eval(expr = parse(text = expr_text))
}

#' Create a Tushare realtime websocket
#'
#' @param topic realtime topic to subscribe
#' @param code code to subscribe
#' @param callback callback function to process data
#' @param api an tsapi object
#'
#' @return a WebSocket
#' @export
#'
tushare_realtime_websocket <- function(topic, code, callback, api = TushareApi()) {

  if (!requireNamespace("websocket", quietly = TRUE) ||
      !requireNamespace("later", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package websocket and jsonlite are needed to create a Tushare realtime websocket", call. = FALSE)
  }

  token <- as.character(api)

  #Tushare subscription WebSocket URL
  ws <- websocket::WebSocket$new("wss://ws.waditu.com/listening", autoConnect = FALSE)

  ws$onOpen(function(event) {
    payload <- list(
      action = jsonlite::unbox("listening"),
      token  = jsonlite::unbox(token),
      data   = list()
    )
    payload$data[[topic]] = code
    ws$send(jsonlite::toJSON(payload))
  })

  ws$onMessage(function(event) {

    data <- jsonlite::fromJSON(event$data)

    #If received data is pong, schedule next ping.
    if (is.character(data$data) && (data$data == "pong")) {
      # message(Sys.time(), ": RECV pong, schedule next ping.")
      later::later(function() {
        payload <- '{"action":"ping"}'
        ws$send(payload)
      }, delay = 20.0)
      return(TRUE)
    }

    #If status is not TRUE, throw error
    if (!data$status) {
      ws$close()
      stop(data$message, call. = FALSE)
    }

    #Pass received data to callback function
    callback_data <- data$data
    callback_data$record <- split_dict_values(callback_data$record)
    do.call(callback, callback_data)

    TRUE
  })

  ws
}

#' Start Tushare websocket
#'
#' @param ws a Websocket created by tushare_realtime_websocket()
#' @param timeout timeout
#'
#' @return TRUE
#' @export
#'
tushare_realtime_start <- function(ws, timeout = 10.0) {

  # Connect to Tushare websocket and wait util connection established
  ws$connect()
  later::run_now(1.0)
  con <- ws$readyState()

  # Force run_now()
  t0 <- unclass(Sys.time())
  while (!con) {
    later::run_now(1.0)
    con <- ws$readyState()
    if (unclass(Sys.time()) - t0 > timeout) {
      stop("Connect to Tushare websocket timed out.", call. = FALSE)
    }
  }

  tushare_realtime_ping(ws)
}

#' Ping Tushare websocket
#'
#' @param ws a WebSocket
#'
#' @return TRUE
#' @export
#'
tushare_realtime_ping <- function(ws) {

  payload <- '{"action":"ping"}'
  ws$send(payload)

  TRUE
}

parse_hq_stk_tick <- function(record, today = Sys.Date(), tz = "Asia/Shanghai") {

  t_now <- Sys.time()
  t_now <- lubridate::with_tz(Sys.time(), tzone = tz)

  t_rec <- paste(as.character(today), record[3])
  t_rec <- lubridate::parse_date_time2(t_rec, orders = "%Y-%m-%d %H:%M:%OS", tz = tz)

  ans <- list(Code     = record[1],
              Name     = record[2],
              Time     = t_rec,
              TimeRecv = t_now,
              Price    = as.numeric(record[4]),
              PreClose = as.numeric(record[5]),
              Open     = as.numeric(record[6]),
              High     = as.numeric(record[7]),
              Low      = as.numeric(record[8]),
              Close    = as.numeric(record[9]),
              Vol      = as.integer(record[10]),
              Tnvr     = as.numeric(record[11]),
              Ask_P1   = as.numeric(record[12]),
              Ask_V1   = as.integer(record[13]),
              Ask_P2   = as.numeric(record[14]),
              Ask_V2   = as.integer(record[15]),
              Ask_P3   = as.numeric(record[16]),
              Ask_V3   = as.integer(record[17]),
              Ask_P4   = as.numeric(record[18]),
              Ask_V4   = as.integer(record[19]),
              Ask_P5   = as.numeric(record[20]),
              Ask_V5   = as.integer(record[21]),
              Bid_P1   = as.numeric(record[22]),
              Bid_V1   = as.integer(record[23]),
              Bid_P2   = as.numeric(record[24]),
              Bid_V2   = as.integer(record[25]),
              Bid_P3   = as.numeric(record[26]),
              Bid_V3   = as.integer(record[27]),
              Bid_P4   = as.numeric(record[28]),
              Bid_V4   = as.integer(record[29]),
              Bid_P5   = as.numeric(record[30]),
              Bid_V5   = as.integer(record[31]))

  ans
}
