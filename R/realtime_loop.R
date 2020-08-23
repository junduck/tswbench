norm_rt_dt <- function(dt, code_col, time_col, api) {

  # Add normalised code
  dt[, Code := norm_ashare_code(dt[[code_col]])]

  # Add extra timestamp info
  dt[, idate := data.table::as.IDate(dt[[time_col]])]
  dt[, itime := data.table::as.ITime(dt[[time_col]])]
  dt[, irecv := itime_now(api = api)]

  dt
}

get_rt_db <- function(wd = getwd(), filename = paste0(Sys.Date(), ".db")) {

  file.path(normalizePath(wd), filename)
}

connect_rt_db <- function(db = get_rt_db()) {

  DBI::dbConnect(RSQLite::SQLite(), dbname = db)
}

create_rt_db <- function(query_func, ref_code, code_col, time_col,
                         db = get_rt_db(), api = TushareApi(),
                         tbl_name = "rtdt", idx_unique = "rtdt_idx_unique", idx_dttm = "rtdt_idx_dttm") {

  con <- connect_srt_db(db = db)
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

  r <- create_index(con = con, name = idx_dttm, tbl = tbl_name,
                    var = c("idate", "itime"), ASC = TRUE, unique = FALSE)
  if (!r) {
    on.exit(file.remove(db), add = TRUE)
    msg <- sprintf("Failed to create index %s on %s.", idx_dttm, tbl_name)
    stop(msg, call. = FALSE)
  }

  TRUE
}

create_rt_loop <- function(query_func, ref_code, code_col, time_col,
                           db = get_rt_db(), api = TushareApi(),
                           tbl_name = "rtdt", idx_unique = "rtdt_idx_unique", idx_dttm = "rtdt_idx_dttm",
                           report_window = 30L) {

  if (!file.exists(db)) {
    create_rt_db(query_func = query_func, ref_code = ref_code,
                 code_col = code_col, time_col = time_col, db = db, api = api,
                 tbl_name = tbl_name, idx_unique = idx_unique, idx_dttm = idx_dttm)
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

  mean_dt_m <- make_moving_mean(report_window)
  max_dt_m <- make_moving_max(report_window)
  mean_insert_m <- make_moving_mean(report_window)

  function(codes, today = idate_now()) {

    if (!is(today, "IDate")) {
      today <- data.table::as.IDate(today)
    }

    con <- connect_rt_db(db)
    on.exit({DBI::dbDisconnect(con)})

    cnt <- 0L
    mdt_m <- mxdt_m <- mins_m <- 0.0

    message(Sys.time(), " Querying ", length(codes), " codes")
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
      rate <- r / delta_t
      mdt_m <- mean_dt_m(delta_t)
      mxdt_m <- max_dt_m(delta_t)
      mins_m <- mean_insert_m(rate)

      # Report
      cnt <- (cnt + 1L) %% report_window
      if (cnt == 0L) {
        message("----- ", Sys.time(), " -----")
        message("Recent avg query time: ", mdt_m, " max: ", mxdt_m)
        message("Recent avg insert rate: ", mins_m)
      }

      # Sleep if querying too fast
      if (delta_t < 1.0) {
        Sys.sleep(1.0 - delta_t)
      }

    }
  }
}

create_rt_loader <- function(code_col, time_col, today = idate_now(),
                             db = get_rt_db(), api = TushareApi(), tbl_name = "rtdt") {

  force(code_col)
  force(time_col)
  force(db)
  force(api)
  force(tbl_name)

  if (!is(today, "IDate")) {
    today <- data.table::as.IDate(today)
  }

  irecv <- 0L
  function() {

    con <- connect_srt_db(db = db)
    on.exit(DBI::dbDisconnect(con))

    where_clause <- sprintf("`idate` = %d AND `irecv` > %d", today, irecv)
    dt <- select_from_where(con = con, tbl = tbl_name, what = "*", where = where_clause)
    if (nrow(dt)) {
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
