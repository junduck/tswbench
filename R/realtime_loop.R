#' Return current time/date as ITime/IDate
#'
#' @param api a tsapi object
#'
#' @return ITime/IDate
#' @export
#'
itime_now <- function(api = TushareApi()) {

  data.table::as.ITime(lubridate::with_tz(Sys.time(), tzone = get_tz(api)))
}

#' @rdname itime_now
#' @export
#'
idate_now <- function() {

  data.table::as.IDate(Sys.Date())
}

norm_rt_dt <- function(dt, code_col, time_col, api) {

  # Add normalised code
  dt[, Code := norm_ashare_code(dt[[code_col]])]

  # Add extra timestamp info
  dt[, idate := data.table::as.IDate(dt[[time_col]])]
  dt[, itime := data.table::as.ITime(dt[[time_col]])]
  dt[, irecv := itime_now(api = api)]

  dt
}

#' Return realtime database
#'
#' @param wd working directory
#' @param filename filename of database
#'
#' @return a character vector
#' @export
#'
get_rt_db <- function(wd = getwd(), filename = paste0(Sys.Date(), ".db")) {

  file.path(normalizePath(wd), filename)
}

#' Return DBI connection to realtime database
#'
#' @param db path to database
#'
#' @return a DBI connection
#' @export
#'
connect_rt_db <- function(db = get_rt_db()) {

  DBI::dbConnect(RSQLite::SQLite(), dbname = db)
}

create_rt_db <- function(query_func, ref_code, code_col, time_col,
                         db, tbl_name, idx_unique, idx_dttm, idx_recv, api) {

  con <- connect_rt_db(db = db)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")

  ref_dt <- query_func(ref_code, api = api)
  norm_rt_dt(ref_dt, code_col = code_col, time_col = time_col, api = api)

  r <- create_table(con = con, name = tbl_name, dt = ref_dt, create_index = FALSE)
  if (!r) {
    on.exit(file.remove(db), add = TRUE)
    msg <- sprintf("Failed to create table %s in %s.", tbl_name, db)
    stop(msg, call. = FALSE)
  }

  r <- create_index(con = con, name = idx_unique, tbl = tbl_name,
                    var = c("Code", "idate", "itime"), ASC = TRUE, unique = TRUE)
  if (!r) {
    on.exit(file.remove(db), add = TRUE)
    msg <- sprintf("Failed to create index %s on %s.", idx_unique, tbl_name)
    stop(msg, call. = FALSE)
  }

  if (!is.null(idx_dttm)) {
    r <- create_index(con = con, name = idx_dttm, tbl = tbl_name,
                      var = c("idate", "itime"), ASC = TRUE, unique = FALSE)
    if (!r) {
      msg <- sprintf("Failed to create index %s on %s.", idx_dttm, tbl_name)
      warning(msg, call. = FALSE)
    }
  }

  if (!is.null(idx_recv)) {
    r <- create_index(con = con, name = idx_recv, tbl = tbl_name,
                      var = c("idate", "irecv"), ASC = TRUE, unique = FALSE)
    if (!r) {
      msg <- sprintf("Failed to create index %s on %s.", idx_recv, tbl_name)
      warning(msg, call. = FALSE)
    }
  }

  TRUE
}

#' Default sleep timer for A-share market
#'
#' @param t_now ITime
#'
#' @return time in seconds to sleep
#' @export
#'
rt_sleep_timer <- function(t_now) {

  # Sleep between 11:32:00 and 12:58:00
  if (t_now >= 41520L && t_now < 46680L) {
    46680L - t_now
  } else {
    0L
  }
}

#' Default exit timer for A-share market
#'
#' @param t_now ITime
#'
#' @return TRUE if t_now larger than 15:32:00, FALSE otherwise
#' @export
#'
rt_exit_timer <- function(t_now) {

  # Exit on 15:32:00
  t_now >= 55920L
}

#' Create a realtime data loop
#'
#' @param query_func a query function to fetch realtime data
#' @param ref_code a reference code to query and determine data structure returned by query_func
#' @param code_col column name to code
#' @param time_col column name to time
#' @param db path to database
#' @param tbl_name table name to store data
#' @param idx_unique index name to identify unique entries
#' @param idx_dttm index on datetime
#' @param idx_recv index on time received
#' @param sleep_timer a sleep timer function
#' @param exit_timer an exit timer function
#' @param min_loop_time minimum loop time
#' @param report_window report window size
#' @param api a tsapi object
#'
#' @return a function to run the loop
#' @export
#'
create_rt_loop <- function(query_func = sina_realtime_quote,
                           ref_code   = "sz000001",
                           # data structure
                           code_col   = "sina_code",
                           time_col   = "Time",
                           # database
                           db         = get_rt_db(),
                           tbl_name   = "rtdt",
                           idx_unique = paste0(tbl_name, "_idx_unique"),
                           idx_dttm   = paste0(tbl_name, "_idx_dttm"),
                           idx_recv   = paste0(tbl_name, "_idx_recv"),
                           # loop misc
                           sleep_timer   = rt_sleep_timer,
                           exit_timer    = rt_exit_timer,
                           min_loop_time = 1.0,
                           report_window = 30L,
                           # system config
                           api = TushareApi()) {

  if (!file.exists(db)) {
    create_rt_db(query_func = query_func,
                 ref_code = ref_code,
                 code_col = code_col,
                 time_col = time_col,
                 db = db,
                 tbl_name = tbl_name,
                 idx_unique = idx_unique,
                 idx_dttm = idx_dttm,
                 idx_recv = idx_recv,
                 api = api)
  } else {
    force(query_func)
    force(code_col)
    force(time_col)
    force(api)
    force(tbl_name)
  }
  con <- connect_rt_db(db)
  on.exit({DBI::dbDisconnect(con)})
  DBI::dbExecute(con, "PRAGMA synchronous = 1;")

  # Recent query time
  mean_dt_m     <- make_moving_mean(report_window)
  # Recent max query time
  max_dt_m      <- make_moving_max(report_window)
  # Recent insert rate
  mean_insert_m <- make_moving_mean(report_window)

  function(codes, today = idate_now()) {

    if (!is(today, "IDate")) {
      today <- data.table::as.IDate(today)
    }

    con <- connect_rt_db(db)
    on.exit({DBI::dbDisconnect(con)})

    report_cnt <- 0L
    mdt_m <- mxdt_m <- mins_m <- 0.0

    message(Sys.time(), " Querying ", length(codes), " codes")
    while (TRUE) {
      t_now <- itime_now(api = api)
      # Check exit timer
      if (exit_timer(t_now = t_now)) {
        message(Sys.time(), " End of market day. Exiting.")
        break
      }
      # Check sleep timer
      t_sleep <- sleep_timer(t_now = t_now)
      if (t_sleep) {
        message(Sys.time(), sprintf(" Sleep for %s seconds.", t_sleep))
        Sys.sleep(t_sleep)
      }

      # Data query
      t <- unclass(Sys.time())
      r <- 0
      tryCatch({
        # Query and normalise
        dt <- query_func(codes, api = api)
        norm_rt_dt(dt = dt, code_col = code_col, time_col = time_col, api = api)
        # Filter valid entries
        dt <- dt[idate == today]
        # Write data
        if (nrow(dt)) {
          r  <- insert_to(con = con, tbl = tbl_name, dt = dt, conflict = "ignore")
        }
      }, error = function(e) {
        message(Sys.time(), " Error: ", toString(e))
      })

      # Timing stats
      delta_t <- unclass(Sys.time()) - t
      rate    <- r / delta_t
      mdt_m   <- mean_dt_m(delta_t)
      mxdt_m  <- max_dt_m(delta_t)
      mins_m  <- mean_insert_m(rate)
      # Report
      report_cnt <- (report_cnt + 1L) %% report_window
      if (report_cnt == 0L) {
        message("----- ", Sys.time(), " -----")
        message("Recent avg query time: ", mdt_m, " max: ", mxdt_m)
        message("Recent avg insert rate: ", mins_m)
      }

      # Sleep if querying too fast
      if (delta_t < min_loop_time) {
        Sys.sleep(min_loop_time - delta_t)
      }
    }

    return(db)
  }
}

#' Create a realtime data loader
#'
#' @param db path to database
#' @param tbl_name table name that stores realtime data
#' @param code_col column name to code
#' @param time_col column name to time
#' @param today date of today
#' @param api a tsapi object
#'
#' @return a data loader function, which returns incremental data.table of new data
#' @export
#'
create_rt_loader <- function(db       = get_rt_db(),
                             tbl_name = "rtdt",
                             code_col = "Code",
                             time_col = "Time",
                             today    = idate_now(),
                             api = TushareApi()) {

  force(db)
  force(tbl_name)
  force(code_col)
  force(time_col)
  force(api)

  if (!is(today, "IDate")) {
    today <- data.table::as.IDate(today)
  }

  irecv <- 0L
  function() {

    con <- connect_rt_db(db = db)
    on.exit(DBI::dbDisconnect(con))

    where_clause <- sprintf("`idate` = %d AND `irecv` > %d", today, irecv)
    dt <- select_from_where(con = con, tbl = tbl_name, what = "*", where = where_clause)
    if (nrow(dt)) {
      # recover POSIXct, IDate and ITime
      data.table::set(dt, j = time_col, value = lubridate::as_datetime(dt[[time_col]], tz = get_tz(api)))
      dt[, `:=`(
        idate = data.table::as.IDate(idate),
        itime = data.table::as.ITime(itime),
        irecv = data.table::as.ITime(irecv)
      )]
      data.table::setkeyv(dt, c(code_col, "idate", "itime"))
      irecv <<- max(dt$irecv)
    }

    dt
  }
}
