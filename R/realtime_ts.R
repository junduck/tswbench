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
    event$target$send(jsonlite::toJSON(payload))
  })

  ws$onMessage(function(event) {

    data <- jsonlite::fromJSON(event$data)

    #If status is not TRUE, throw error
    if (!data$status) {
      event$target$close()
      stop(data$message, call. = FALSE)
    }

    #If received data is pong, schedule next ping.
    if (is.character(data$data) && (data$data == "pong")) {
      # message(Sys.time(), ": RECV pong, schedule next ping.")
      later::later(function() {
        if (event$target$readyState() == 1L) {
          payload <- '{"action":"ping"}'
          event$target$send(payload)
        }
      }, delay = 20.0)
    } else {
      #Pass received data to callback function
      callback_data <- data$data
      callback_data$record <- split_dict_values(callback_data$record)
      do.call(callback, callback_data)
    }

    TRUE
  })

  ws
}

#' Start/Stop/Ping Tushare websocket
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

#' @rdname tushare_realtime_start
#' @export
#'
tushare_realtime_stop <- function(ws) {

  if (ws$readyState() == 1L) {
    ws$close()
  }

  TRUE
}

#' @rdname tushare_realtime_start
#' @export
#'
tushare_realtime_ping <- function(ws) {

  if (ws$readyState() == 1L) {
    payload <- '{"action":"ping"}'
    ws$send(payload)
  }

  TRUE
}

#' Generate a record parser for Tushare realtime tick data
#'
#' @param api a tsapi object
#'
#' @return a parser function
#' @export
#'
tushare_realtime_tick_parser <- function(api = TushareApi()) {

  today = as.character(Sys.Date())
  tzone = get_tz(api)

  function(record) {
    parse_hq_stk_tick(record = record, today = today, tz = tzone)
  }
}

parse_hq_stk_tick <- function(record, today, tz) {

  t_now <- Sys.time()
  t_now <- lubridate::with_tz(Sys.time(), tzone = tz)

  t_rec <- paste0(today, record[3])
  # Parse as UTC then force_tz to avoid .mklt (SLOW)
  t_rec <- lubridate::force_tz(
    lubridate::parse_date_time2(t_rec, orders = "YmdHMOS"),
    tzone = tz
  )

  rec_numval <- as.numeric(record[4:31])
  ans <- list(Code     = record[1],
              Name     = record[2],
              Time     = t_rec,
              TimeRecv = t_now,
              Price    = rec_numval[ 1],
              PreClose = rec_numval[ 2],
              Open     = rec_numval[ 3],
              High     = rec_numval[ 4],
              Low      = rec_numval[ 5],
              Close    = rec_numval[ 6],
              Vol      = rec_numval[ 7],
              Tnvr     = rec_numval[ 8],
              Ask_P1   = rec_numval[ 9],
              Ask_V1   = rec_numval[10],
              Ask_P2   = rec_numval[11],
              Ask_V2   = rec_numval[12],
              Ask_P3   = rec_numval[13],
              Ask_V3   = rec_numval[14],
              Ask_P4   = rec_numval[15],
              Ask_V4   = rec_numval[16],
              Ask_P5   = rec_numval[17],
              Ask_V5   = rec_numval[18],
              Bid_P1   = rec_numval[19],
              Bid_V1   = rec_numval[20],
              Bid_P2   = rec_numval[21],
              Bid_V2   = rec_numval[22],
              Bid_P3   = rec_numval[23],
              Bid_V3   = rec_numval[24],
              Bid_P4   = rec_numval[25],
              Bid_V4   = rec_numval[26],
              Bid_P5   = rec_numval[27],
              Bid_V5   = rec_numval[28])

  ans
}

callback_zmqpub <- function(bind = "tcp://*:6789") {

  ctx <- rzmq::init.context(1L)
  pub <- rzmq::init.socket(ctx, "ZMQ_PUB")
  res <- rzmq::bind.socket(pub, bind)
  if (!res) {
    rm(pub)
    rm(ctx)
    stop("Error binding to address ", bind, call. = FALSE)
  }

  function(record) {
    res <- rzmq::send.socket(pub, record)
    if (!res) {
      stop("Error publishing message.", call. = FALSE)
    }
    TRUE
  }
}
