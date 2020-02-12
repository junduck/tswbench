chk_number <- function(x) stringr::str_detect(x, "^[0-9]*$")
chk_number_n <- function(x, n) stringr::str_detect(x, sprintf("^[0-9]{%d}$", n))
chk_sina <- function(x) stringr::str_detect(x, "(^sh|^sz)[0-9]{6}$")

cast_datetime_POSIXct <- function(x, tz) {
  suppressWarnings({
    ans <- lubridate::as_datetime(x, tz = tz)
    if (anyNA(ans)) {
      ans <- lubridate::parse_date_time2(x, orders = "HMS", tz = tz)
    }
  })
  ans
}

cast_datetime_Date <- function(x) {
  suppressWarnings(
    lubridate::as_date(x)
  )
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
         Date    = function(x) cast_datetime_Date(x),
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

  dt <- suppressWarnings(lubridate::as_datetime(x, tz = tz))
  hr <- lubridate::hour(dt)

  if (hr) {
    fmt <- "%Y-%m-%d %H:%M:%S"
  } else {
    fmt <- switch(func,
                  news = "%Y-%m-%d",
                  "%Y%m%d")
  }

  ans <- as.character(dt, format = fmt)
  ans[is.na(ans)] <- ""
  ans
}
