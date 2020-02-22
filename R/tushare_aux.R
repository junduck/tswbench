chk_number <- function(x) stringr::str_detect(x, "^[0-9]*$")
chk_number_n <- function(x, n) stringr::str_detect(x, sprintf("^[0-9]{%d}$", n))
chk_sina <- function(x) stringr::str_detect(x, "(^sh|^sz)[0-9]{6}$")

cast_datetime_POSIXct <- function(x, tz) {

  if (lubridate::is.Date(x)) {
    #fix timezone issue with Date class when converting to POSIXct
    posix <- .POSIXct(unclass(x) * 86400.0, tz = "UTC")
    x <- lubridate::force_tz(posix, tz)
  }

  suppressWarnings({
    ans <- lubridate::as_datetime(x, tz = tz)
    if (anyNA(ans)) {
      ans <- lubridate::parse_date_time2(x, orders = "HMS", tz = tz)
    }
  })
  ans
}

cast_logical <- function(x) {

  if (is.character(x)) {
    x <- toupper(x)
    ans <- vector(mode = "logical", length = length(x))
    ans[x == "1" | x == "Y"] <- TRUE
  } else {
    ans <- as.logical(x)
  }
  ans
}

cast_date <- function(api) {

  switch(attr(api, "date_mode"),
         POSIXct = function(x) cast_datetime_POSIXct(x, tz = attr(api, "tz")),
         Date    = lubridate::as_date,
         as.character
  )
}

cast_time <- function(api) {

  switch(attr(api, "time_mode"),
         POSIXct = function(x) cast_datetime_POSIXct(x, tz = attr(api, "tz")),
         as.character
  )
}

cast_logi <- function(api) {

  switch(attr(api, "logi_mode"),
         logical = cast_logical,
         as.character)
}

cast_datetime_char <- function(x, tz, func) {

  if (lubridate::is.Date(x)) {
    dt <- x
  } else {
    dt <- lubridate::as_datetime(x, tz = tz)
  }
  hr <- lubridate::hour(dt)

  if (is.na(hr) || !hr) {
    fmt <- switch(func,
                  news = "%Y-%m-%d",
                  "%Y%m%d")
  } else {
    fmt <- "%Y-%m-%d %H:%M:%S"
  }

  ans <- as.character(dt, format = fmt)
  ans[is.na(ans)] <- ""
  ans
}

get_fields_def <- function(func) {

  if (endsWith(func, "_vip")) {
    func <- substr(func, 1L, nchar(func) - 4L)
  }

  fname <- paste0(func, ".csv")
  def_file <- system.file("fields-def", fname, package = "tswbench")

  data.table::fread(def_file)
}
