normalise_srt_data <- function(dt, api) {

  se   <- toupper(stringr::str_sub(dt$sina_code, 1L, 2L))
  code <- stringr::str_extract(dt$sina_code, "[0-9].+$")

  dt[, Code  := paste0(code, ".", se)]
  dt[, idate := data.table::as.IDate(Time)]
  dt[, itime := data.table::as.ITime(Time)]
  dt[, irecv := data.table::as.ITime(lubridate::with_tz(Sys.time(), tzone = get_tz(api)))]

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
    stop("Failed to create table sina_realtime.")
  }

  r <- create_index(con = con,
                    name = "sina_realtime_index", tbl = "sina_realtime",
                    var = c("Code", "idate", "itime"), unique = TRUE)
  if (!r) {
    stop("Failed to create index sina_realtime_index.")
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

  message(Sys.time(), " Request stock list from Tushare")
  slist <- api$stock_basic()
  codes <- get_sina_code(symbol = slist$ts_code)
  message(Sys.time(), " Querying ", length(codes), " codes")

  today <- data.table::as.IDate(today)

  t <- 0
  while (TRUE) {

    delta <- unclass(Sys.time()) - t
    if (delta < 1.0) {
      Sys.sleep(1.0 - delta)
    }

    t <- unclass(Sys.time())
    tryCatch({
      dt <- sina_realtime_quote(sina_code = codes, api = api)
      if (nrow(dt)) {
        dt <- normalise_srt_data(dt = dt, api = api)
        dt <- dt[idate == today & Vol > 0]
        r  <- insert_to(con = con, tbl = "sina_realtime", dt = dt, conflict = "ignore")
        message(Sys.time(), " Sina query: ", nrow(dt), " inserted: ", r)
      }
    }, error = function(e) {
      #FIXME: if user interrupts R during curl_fetch_memory, it will trigger exception
      #and be captured by tryCatch() showing a warning. Thus not properly interrupted.
      #User may need to interrupt R multiple times to make this function stop.
      warning(Sys.time(), " ", toString(e))
    })
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
