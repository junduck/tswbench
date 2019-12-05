cast_date <- function(api) {
  switch(attr(api, "date_mode"),
         POSIXct = function(x) as.POSIXct(x,
                                          tz = attr(api, "tz"),
                                          format = tus.globals$date_fmt),
         Date    = function(x) as.Date(x,
                                       tz = attr(api, "tz"),
                                       format = tus.globals$date_fmt),
         char    = as.character
  )
}

cast_time <- function(api) {
  switch(attr(api, "time_mode"),
         POSIXct = function(x) as.POSIXct(x,
                                          tz = attr(api, "tz"),
                                          format = tus.globals$time_fmt),
         char    = as.character
  )
}

cast_logi <- function(api) {
  switch(attr(api, "logi_mode"),
         logical = function(x) {
           ans <- vector(mode = "logical", length = length(x))
           if (is.character(x)) {
             idx <- x == "1" | x == "Y"
             ans[idx] <- TRUE
           } else {
             idx <- x != 0
             ans[idx] <- TRUE
           }
           return(ans)
         },
         char = as.character)
}

char_date <- function(datetime) {
  datetime <- lubridate::as_date(datetime)
  as.character(datetime, format = tus.globals$date_fmt)
}

char_time <- function(datetime) {
  datetime <- lubridate::as_datetime(datetime)
  as.character(datetime, format = tus.globals$time_fmt)
}
