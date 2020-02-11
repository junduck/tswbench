chk_number <- function(x) stringr::str_detect(x, "^[0-9]*$")
chk_number_n <- function(x, n) stringr::str_detect(x, sprintf("^[0-9]{%d}$", n))

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

cast_datetime_chartime <- function(x, tz) {

  datetime <- cast_datetime_POSIXct(x, tz)
  ans <- as.character(datetime, format = tus.globals$time_fmt)

  ans[is.na(ans)] <- ""
  ans
}

cast_datetime_chardate <- function(x) {

  datetime <- cast_datetime_Date(x)
  ans <- as.character(datetime, format = tus.globals$date_fmt)

  ans[is.na(ans)] <- ""
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

char_date <- function(datetime) {
  if (is.character(datetime) && datetime != "") {
    datetime <- lubridate::as_date(datetime)
  }
  as.character(datetime, format = tus.globals$date_fmt)
}

char_time <- function(datetime) {
  if (is.character(datetime) && datetime != "") {
    datetime <- lubridate::as_datetime(datetime)
  }
  as.character(datetime, format = tus.globals$time_fmt)
}
