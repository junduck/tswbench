#Tushare API object attributes
get_tm <- function(api) attr(api, "time_mode")
get_dm <- function(api) attr(api, "date_mode")
get_lm <- function(api) attr(api, "logi_mode")
get_tz <- function(api) attr(api, "tz")

chk_number   <- function(x) stringr::str_detect(x, "^[0-9]*$")
chk_number_n <- function(x, n) stringr::str_detect(x, sprintf("^[0-9]{%d}$", n))
chk_sina     <- function(x) stringr::str_detect(x, "(^sh|^sz)[0-9]{6}$")

as_POSIXct <- function(x, tz) {

  if (lubridate::is.Date(x)) {
    #fix timezone issue with Date class when converting to POSIXct
    ans <- lubridate::force_tz(.POSIXct(unclass(x) * 86400.0, tz = "UTC"), tzone = tz)
  } else if (is.character(x)) {
    #try full dttm format
    ans <- suppressWarnings({
      lubridate::parse_date_time2(x, orders = "%Y%m%d%H%M%OS", tz = tz)
    })
    #try intraday dttm format
    na_idx <- is.na(ans)
    if (any(na_idx)) {
      ans[na_idx] <- suppressWarnings({
        lubridate::parse_date_time2(x[na_idx], orders = "%H%M%OS", tz = tz)
      })
    }
  } else {
    ans <- lubridate::as_datetime(x, tz = tz)
  }

  ans
}

as_logical <- function(x) {

  if (is.character(x)) {
    x <- toupper(x)
    ans <- vector(mode = "logical", length = length(x))
    ans[x == "1" | x == "Y"] <- TRUE
  } else {
    ans <- as.logical(x)
  }

  ans
}

date_parser <- function(api) {

  switch(get_dm(api),
         POSIXct = function(x) as_POSIXct(x, tz = get_tz(api)),
         Date    = lubridate::as_date,
         as.character
  )
}

datetime_parser <- function(api) {

  switch(get_tm(api),
         POSIXct = function(x) as_POSIXct(x, tz = get_tz(api)),
         as.character)
}

logical_parser <- function(api) {

  switch(get_lm(api),
         logical = as_logical,
         as.character)
}

datetime_to_char <- function(x, tz) {

  if (is.character(x)) {
    return(x)
  }

  x <- as_POSIXct(x = x, tz = tz)
  hr <- data.table::hour(x)

  if (is.na(hr) || !hr) {
    #hour is not present, cast as date
    fmt <- "%Y%m%d"
  } else {
    fmt <- "%Y-%m-%d %H:%M:%S"
  }

  ans <- as.character(x, format = fmt)
  ans[is.na(ans)] <- ""
  ans
}
