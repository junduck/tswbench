itime_now <- function(api) {
  data.table::as.ITime(lubridate::with_tz(Sys.time(), tzone = get_tz(api)))
}

normalise_srt_data <- function(dt, api) {

  se   <- toupper(stringr::str_sub(dt$sina_code, 1L, 2L))
  code <- stringr::str_extract(dt$sina_code, "[0-9].+$")

  dt[, Code  := paste0(code, ".", se)]
  dt[, idate := data.table::as.IDate(Time)]
  dt[, itime := data.table::as.ITime(Time)]
  dt[, irecv := itime_now(api)]

  dt
}

#' Return default Sina realtime database file path
#'
#' @param wd a working directory
#' @param filename database filename
#'
#' @return a string, file path to database
#' @export
#'
get_srt_db <- function(wd = getwd(), filename = paste0(Sys.Date(), ".db")) {

  wd <- normalizePath(wd)
  file.path(wd, filename)
}

#' Connect to Sina realtime database
#'
#' @param db file path to database
#'
#' @return a DBI connection
#' @export
#'
connect_srt_db <- function(db = get_srt_db()) {

  DBI::dbConnect(RSQLite::SQLite(), dbname = db)
}

#' Create a Sina realtime database
#'
#' @param db file path to database
#' @param api a tsapi object
#'
#' @return TRUE
#' @export
#'
create_srt_db <- function(db = get_srt_db(), api = TushareApi()) {

  con <- connect_srt_db(db = db)
  on.exit(DBI::dbDisconnect(con))

  #Use write ahead log to enable non-blocking readers while writing
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")

  dt <- sina_realtime_quote(sina_code = "sz000001", api = api)
  dt <- normalise_srt_data(dt = dt, api = api)

  r <- create_table(con = con, name = "sina_realtime", dt = dt, create_index = FALSE)
  if (!r) {
    stop("Failed to create table sina_realtime.", call. = FALSE)
  }

  r <- create_index(con = con,
                    name = "sina_realtime_index", tbl = "sina_realtime",
                    var = c("Code", "idate", "itime"), ASC = TRUE, unique = TRUE)
  if (!r) {
    stop("Failed to create index sina_realtime_index.", call. = FALSE)
  }
  r <- create_index(con = con,
                    name = "sina_realtime_index_dt", tbl = "sina_realtime",
                    var = c("idate", "itime"), ASC = TRUE, unique = FALSE)
  if (!r) {
    stop("Failed to create index sina_realtime_index_dt.", call. = FALSE)
  }

  TRUE
}

#' Run Sina realtime data loop
#'
#' This function simply query newest quotes from Sina and write to an sqlite
#' database as provided by db argument. Since WAL is used for the database,
#' please make sure that the file is not on a network location.
#'
#' @param db file path to database
#' @param today date of today
#' @param api a tsapi object
#'
#' @return this function loops indefinately and does not return
#' @export
#'
sina_realtime_loop <- function(db = get_srt_db(), today = Sys.Date(), api = TushareApi()) {

  if (!file.exists(db)) {
    create_srt_db(db = db, api = api)
  }
  con <- connect_srt_db(db = db)
  on.exit({DBI::dbDisconnect(con)})

  # Normal sync should be safe enough for WAL
  DBI::dbExecute(con, "PRAGMA synchronous = 1;")

  message(Sys.time(), " Request stock list from Tushare")
  slist <- api$stock_basic()
  scode <- sina_ashare_code(code = slist$ts_code, type = "stock")
  flist <- codes <- api$fund_basic(status = "L", market = "E")
  fcode <- sina_ashare_code(code = flist$ts_code, type = "fund")
  codes <- c(scode, fcode)
  icode <- c("sh000001", "sh000002", "sh000003", "sh000008", "sh000009", "sh000010",
             "sh000011", "sh000012", "sh000016", "sh000017", "sh000300", "sh000688",
             "sz399001", "sz399002", "sz399003", "sz399004", "sz399005", "sz399006",
             "sz399100", "sz399101", "sz399106", "sz399107", "sz399108", "sz399333", "sz399606",
             # Because of my preference
             # SZ Cap - Beta - Vol
             "sz399404", "sz399405", "sz399406", "sz399407", "sz399408", "sz399409",
             # SH 180/380 - Beta
             "sh000135", "sh000136", "sh000137", "sh000138",
             # Conv bonds
             "sh000139", "sz399307")
  codes <- c(scode, fcode, icode)
  message(Sys.time(), " Querying ", length(codes), " codes")

  today <- data.table::as.IDate(today)

  report_window <- 30L
  cnt <- 0L
  mean_dt_m <- make_moving_mean(report_window)
  mean_dt_c <- make_cumulative_mean()
  max_dt_m <- make_moving_max(report_window)
  mean_insert_m <- make_moving_mean(report_window)
  mean_insert_c <- make_cumulative_mean()
  mdt_m <- mdt_c <- mxdt_m <- mins_m <- mins_c <- 0.0

  while (TRUE) {

    # Sleep between 11:32:00 and 12:58:00
    t_now <- itime_now(api = api)
    if (t_now >= 41520L && t_now < 46680L) {
      t_sleep <- 46680L - t_now
      message(Sys.time(), " Sleeping for ", t_sleep, " seconds.")
      Sys.sleep(t_sleep)
    }
    # Exit on 15:32:00
    if (t_now >= 55920L) {
      message(Sys.time(), " End of market day. Exiting.")
      break
    }

    # Data query
    t <- unclass(Sys.time())
    r <- 0
    tryCatch({
      dt <- sina_realtime_quote(sina_code = codes, api = api)
      dt <- normalise_srt_data(dt = dt, api = api)
      dt <- dt[idate == today & Vol > 0]
      if (nrow(dt)) {
        r  <- insert_to(con = con, tbl = "sina_realtime", dt = dt, conflict = "ignore")
      }
    }, error = function(e) {
      #FIXME: if user interrupts R during curl_fetch_memory, it will trigger exception
      #and be captured by tryCatch() showing a warning. Thus not properly interrupted.
      #User may need to interrupt R multiple times to make this function stop.
      warning(Sys.time(), " ", toString(e))
    })

    # Timing stats
    delta_t <- unclass(Sys.time()) - t
    rate <- r / delta_t
    mdt_m <- mean_dt_m(delta_t)
    mdt_c <- mean_dt_c(delta_t)
    mxdt_m <- max_dt_m(delta_t)
    mins_m <- mean_insert_m(rate)
    mins_c <- mean_insert_c(rate)

    # Report
    cnt <- (cnt + 1L) %% report_window
    if (cnt == 0L) {
      message("----- ", Sys.time(), " -----")
      message("Recent avg query time: ", mdt_m, " max: ", mxdt_m, " cumulative: ", mdt_c)
      message("Recent avg insert rate: ", mins_m, " cumulative: ", mins_c)
    }

    if (delta_t < 1.0) {
      Sys.sleep(1.0 - delta_t)
    }

  }
}

#' Generate a Sina realtime data loader function
#'
#' The generated function reads today's incremental data loaded to db. It also
#' takes an optional argument dt_bind, if supplied with a data.table/data.frame,
#' the load rbind old dt_bind data to newly loaded data, constructing all-day
#' data
#'
#' @param db path to database file
#' @param today date of today
#' @param api a tsapi object
#'
#' @return a load function
#' @export
#'
sina_realtime_loader <- function(db = get_srt_db(), today = Sys.Date(), api = TushareApi()) {

  force(db)
  force(api)
  today <- data.table::as.IDate(today)
  irecv <- 0L

  function(dt_bind = NULL) {

    con <- connect_srt_db(db = db)
    on.exit(DBI::dbDisconnect(con))

    where_clause <- sprintf("`idate` = %d AND `irecv` > %d", today, irecv)
    dt <- select_from_where(con = con, tbl = "sina_realtime", what = "*", where = where_clause)
    if (nrow(dt)) {
      dt[, `:=`(
        Time  = lubridate::as_datetime(Time, tz = get_tz(api)),
        idate = data.table::as.IDate(idate),
        itime = data.table::as.ITime(itime),
        irecv = data.table::as.ITime(irecv)
      )]
      if (!is.null(dt_bind)) {
        dt <- data.table::rbindlist(list(dt_bind, dt))
      }
      data.table::setkeyv(dt, c("Code", "idate", "itime"))
      irecv <<- max(dt$irecv)
    } else {
      #No new data, however if dt_bind valid, return dt_bind instead
      if (!is.null(dt_bind)) {
        dt <- dt_bind
      }
    }

    dt
  }
}
