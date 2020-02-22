#' Set/Get Tushare Pro API token
#'
#' @param token a character vector
#'
#' @return token itself, invisibly.
#' @export
SetToken <- function(token) {

  tus.globals$api_token <- as.character(token)

  invisible(tus.globals$api_token)
}

#' @rdname SetToken
#' @export
GetToken <- function() {

  if (is.null(tus.globals$api_token)) {
    ""
  } else {
    tus.globals$api_token
  }
}

#' Simple do.call retry wrapper
#'
#' @param what passed to do.call
#' @param args passed to do.call
#' @param quote passed to do.call
#' @param envir passed to do.call
#' @param attempt max number of attempts
#' @param sleep sleep time between attempts
#' @param warn whether to throw warning when error captured, or an error handling function
#'
#' @return The returned value from function call
do.retry <- function(what, args, quote = FALSE, envir = parent.frame(),
                     attempt = 3, sleep = 0, warn = TRUE) {

  flag <- FALSE
  if (is.logical(warn)) {
    err_func <- function(e) {
      if (warn) {
        warning(e$message, call. = FALSE)
      }
      flag <<- FALSE
      Sys.sleep(sleep)
    }
  } else {
    warn <- match.fun(warn)
    err_func <- function(e) {
      warn(e)
      flag <<- FALSE
      Sys.sleep(sleep)
    }
  }

  for (i in seq_len(attempt)) {
    flag <- TRUE
    ans <- tryCatch({
      do.call(what, args, quote, envir)
    }, error = err_func)
    if (flag) {
      return(ans)
    }
  }

  msg <- do.call(paste0, args = as.list(
    as.character(deparse(substitute(what)))
  ))
  stop(sprintf("Calling %s failed after %d attempts.", msg, attempt), call. = FALSE)
}

#' Make raw request to Tushare Pro API
#'
#' @param api_name name of API function, please refer to online document for more information.
#' @param ... passed to API function.
#' @param fields data fields to request
#' @param token API token.
#' @param timeout timeout in seconds for httr request.
#' @param attempt attempts if timeout
#'
#' @return data.frame/data.table
TusRequest <- function(api_name, ..., fields = c(""), token = GetToken(), timeout = 10.0, attempt = 3L) {

  api_url <- "http://api.waditu.com"

  args <- list(
    token    = as.character(token),
    api_name = api_name,
    params   = list(...),
    fields   = fields
  )

  post_args <- list(
    url    = api_url,
    config = httr::timeout(timeout),
    body   = args,
    encode = "json"
  )
  req <- do.retry(httr::POST, post_args, attempt = attempt, sleep = 0.5, warn = TRUE)
  res <- httr::content(req,
                       as = "parsed",
                       type = "application/json",
                       encoding = "UTF-8")

  if (is.null(res$data)) {
    stop(res$msg, call. = FALSE)
  }

  suppressWarnings({
    if (length(res$data$items)) {
      dt <- tryCatch({
        data.table::rbindlist(res$data$items)
      }, error = function(e) {
        #error happens when null ROW is passed by fromJSON()
        null_row <- sapply(res$data$items, is.null)
        data.table::rbindlist(res$data$items[!null_row])
      })
    } else {
      #create an empty data.table
      dt <- do.call(data.table::data.table, rep_len(x = list(logical()),
                                                    length.out = length(res$data$fields)))
    }
  })
  data.table::setnames(dt, unlist(res$data$fields))

  dt
}

#' Get a Tushare API object.
#'
#' @param api_token API token.
#' @param time_mode data type for time objects
#' @param date_mode data type for date objects
#' @param logi_mode data type for logical objects
#' @param tz Default timezone of POSIXct data
#'
#' @return a tsapi object.
#' @export
#'
TushareApi <- function(api_token = GetToken(),
                       time_mode = c("POSIXct", "char"),
                       date_mode = c("POSIXct", "Date", "char"),
                       logi_mode = c("logical", "char"),
                       tz = "Asia/Shanghai") {

  time_mode <- match.arg(time_mode)
  date_mode <- match.arg(date_mode)
  logi_mode <- match.arg(logi_mode)

  return(
    structure(api_token,
      time_mode = time_mode,
      date_mode = date_mode,
      logi_mode = logi_mode,
      tz        = tz,
      class     = "tsapi")
  )
}

#' Print Values
#'
#' @param x A tsapi object
#' @param ... not used
#'
#' @return x, invisibly
#' @export
#'
'print.tsapi' <- function(x, ...) {

  val <- sprintf("Tushare API object.\n  Time mode: %s\n  Date mode: %s\n  Logi mode: %s\n  Default timezone: %s",
                 attr(x, "time_mode"),
                 attr(x, "date_mode"),
                 attr(x, "logi_mode"),
                 attr(x, "tz"))
  cat(val)

  invisible(x)
}

arg_logi <- c("is_open", "is_new", "is_audit", "is_release", "is_buyback",
              "is_ct", "update_flag")

#' Request data from Tushare API
#'
#' @param x A tsapi object
#' @param func Tushare API function to call
#'
#' @return a data.table
#' @export
#'
'$.tsapi' <- function(x, func) {

  f <- function(..., timeout = 10.0, attempt = 3L) {

    arg <- list(...)

    #fix date/time/logical arguments
    argn <- names(arg)

    #date/time
    idx <- stringr::str_detect(argn, "date$|time$|^period$")
    if (any(idx)) {
      arg[idx] <- lapply(arg[idx], cast_datetime_char, tz = attr(x, "tz"), func = func)
    }
    #logical
    for (i in seq_along(arg)) {
      if (is.logical(arg[[i]])) {
        arg[[i]] <- ifelse(arg[[i]], "1", "0")
      }
    }

    #extra arguments passed to TusRequest()
    arg$api_name <- func
    arg$token <- x
    arg$timeout <- timeout
    arg$attempt <- attempt

    dt <- do.call(TusRequest, arg)
    #parse dt
    if (nrow(dt)) {
      date_col_cast <- cast_date(x)
      time_col_cast <- cast_time(x)
      logi_col_cast <- cast_logi(x)

      cols <- colnames(dt)
      #check date
      col_date <- which(stringr::str_detect(cols, "date$|^period$"))
      if (length(col_date)) {
        data.table::set(dt, j = col_date, value = lapply(dt[, ..col_date], date_col_cast))
      }
      #check time
      col_time <- which(stringr::str_detect(cols, "time$"))
      if (length(col_time)) {
        data.table::set(dt, j = col_time, value = lapply(dt[, ..col_time], time_col_cast))
      }
      #check logi
      col_logi <- which(cols %in% arg_logi)
      if (length(col_logi)) {
        data.table::set(dt, j = col_logi, value = lapply(dt[, ..col_logi], logi_col_cast))
      }

      #try to set keys
      if (("ts_code" %in% cols) && anyDuplicated(dt$ts_code)) {
        data.table::setkeyv(dt, "ts_code")
      } else {
        cidx <- c(col_time, col_date)
        if (length(cidx)) {
          data.table::setkeyv(dt, cols[cidx[1]])
        }
      }

      #fix update_flag issue for fundamental data
      if (("update_flag" %in% cols) && ("ts_code" %in% cols) && ("end_date" %in% cols)) {
        n_ts_code <- unique(dt$ts_code)
        if (length(n_ts_code) > 1L) {
          data.table::setkeyv(dt, c("ts_code", "update_flag"))
          dt <- dt[, .SD[.N], by = ts_code]
        } else {
          data.table::setkeyv(dt, c("end_date", "update_flag"))
          dt <- dt[, .SD[.N], by = end_date]
        }
      }
    }

    dt
  }

  f
}
