#' Convert A-share intraday timestamps to seconds traded
#'
#' 9:30 is converted to 0th second, 13:00 is converted to 7201st second
#'
#' @param t a vector of timestamps
#'
#' @return a vector of integer, counted seconds of t
#' @export
#'
tsec_intraday_ashare <- function(t) {

  t <- data.table::as.ITime(t)
  # align to 9:30
  t <- t - 34200L
  # afternoon session, align to 13:00 + 1 sec
  idx2 <- t >= 12600L
  if (any(idx2)) {
    t[idx2] <- t[idx2] - 5399L
  }

  t
}

#' Calculate suitable time intervals for intraday resampling
#'
#' Trading hours are defined in session, a list of integer vectors, each defining
#' a trading session by 6 integers: session start hour, minute, second, session end
#' hour, minute, second.
#'
#' @param t a timestamp, POSIXct and ITime are supported
#' @param period resample time period in seconds
#' @param session session definition
#'
#' @return a vector of POSIXct/ITime depending on t
#' @export
#'
trsmp_intraday <- function(t, period, session) {

  by <- as.integer(period)

  if (is(t, "ITime")) {
    intv <- list()
    for (i in seq_along(session)) {
      s <- as.integer(session[[i]])
      t1 <- s[1L] * 3600L + s[2L] * 60L + s[3L]
      t2 <- s[4L] * 3600L + s[5L] * 60L + s[6L]
      intv[[i]] <- seq.int(t1, t2, by)
    }
    do.call(c, intv)
  } else {
    tl1 <- tl2 <- as.POSIXlt(t[[1L]])
    tz <- attr(tl1, "tzone")
    intv <- list()
    for (i in seq_along(session)) {
      s <- as.integer(session[[i]])
      tl1$hour <- s[1L]; tl1$min <- s[2L]; tl1$sec <- s[3L]
      tl2$hour <- s[4L]; tl2$min <- s[5L]; tl2$sec <- s[6L]
      intv[[i]] <- seq(unclass(as.POSIXct(tl1)),
                       unclass(as.POSIXct(tl2)),
                       by)
    }
    intv <- do.call(c, intv)
    lubridate::as_datetime(intv, tz = tz)
  }
}

#' Calculate suitable time intervals for intraday resampling, A-share version
#'
#' For A-share intraday only. 9:30 and 13:00 is sampled to reflect orders made
#' in opening session and lunch break. Extended hour (15:00 - 15:30) is not included.
#'
#' @param t a timestamp, POSIXct and ITime are supported
#' @param period resample time period in seconds
#' @param session session of trading day
#'
#' @return a vector of POSIXct/ITime depending on t
#' @export
#'
trsmp_intraday_ashare <- function(t, period, session = c("day", "morning", "afternoon")) {

  session = match.arg(session)

  by <- as.integer(period)
  if (7200L %% by) {
    msg <- sprintf("Can't split trading hours by %d seconds.", by)
    stop(msg, call. = FALSE)
  }

  #shift 3.5 hr / 12600 sec for afternoon section
  if (is(t, "ITime")) {
    #09:30 ITime
    t1 <- 34200L
    #11:30 ITime
    t2 <- 41400L
    intv <- seq.int(t1, t2, by)
    switch(session,
           day       = data.table::as.ITime(c(intv, intv + 12600L)),
           morning   = data.table::as.ITime(intv),
           afternoon = data.table::as.ITime(intv + 12600L))
  } else {
    tl1 <- tl2 <- as.POSIXlt(t[[1L]])
    tz <- attr(t, "tzone")
    tl1$hour <- 9 ; tl1$min <- 30; tl1$sec <- 0
    tl2$hour <- 11; tl2$min <- 30; tl2$sec <- 0
    intv <- seq(unclass(as.POSIXct(tl1)),
                unclass(as.POSIXct(tl2)),
                by)
    switch(session,
           day       = lubridate::as_datetime(c(intv, intv + 12600), tz = tz),
           morning   = lubridate::as_datetime(intv, tz = tz),
           afternoon = lubridate::as_datetime(intv + 12600, tz = tz))
  }
}

#' Group variable by intervals, left open
#'
#' @param t a vector of POSIXct/ITime
#' @param intv time intervals to group
#' @param grp_id wheter to return group id or grouped value
#'
#' @return a vector of group id or POSIXct/ITime
#' @export
#'
grp_interval <- function(x, intv, grp_id = TRUE) {

  x_grpid <- findInterval(x, intv, left.open = TRUE)
  if (grp_id) {
    x_grpid
  } else {
    n <- length(intv)
    x_grpid <- x_grpid + 1L
    if (any(x_grpid > n)) {
      intv <- c(intv, data.table::last(x))
    }
    intv[x_grpid]
  }
}

grp_period <- function(t, period, grp_id = TRUE) {

  intv <- trsmp_intraday_ashare(t, period = period, session = "day")
  grp_interval(x = t, intv = intv, grp_id = grp_id)
}

#' Resample A-share OHLC data
#'
#' Input data dt should have these columns:
#' Code, Date, Time, Open, High, Low, Close, Vol, Tnvr
#'
#' Output data contains these columns:
#' Code, Date, Time, Open, High, Low, Close, Vol, Tnvr, VWAP, Avg
#'
#' Among which, VWAP stands for volume weighted average price and Avg stands for
#' average price traded in corresponding bar.
#'
#' @param dt OHLC data.table
#' @param period resample time period
#' @param align_time whether to align time to resampled time intervals
#'
#' @return data.table
#' @export
#'
resample_ohlc_ashare <- function(dt, period, align_time = FALSE) {

  if (is(dt$Time, "ITime") || length(unique(dt$Date)) == 1L) {
    resample_ohlc_ashare1(dt, period, align_time)
  } else {
    resample_ohlc_ashare2(dt, period, align_time)
  }
}

#' Resample OHLC variance 1:
#'
#' Handles data of same date.
resample_ohlc_ashare1 <- function(dt, period, align_time) {

  data.table::setkeyv(dt, c("Code", "Date", "Time"))
  intv <- trsmp_intraday_ashare(dt$Time, period = period, session = "day")

  if (is.null(dt$VWAP)) {
    dt[, `:=`(cum_Vol = cumsum(Vol), cum_Tnvr = cumsum(Tnvr)), by = c("Code", "Date")]
    dt[, `:=`(VWAP = cum_Tnvr / cum_Vol)]
    on.exit({
      dt[, `:=`(cum_Vol = NULL, cum_Tnvr = NULL, VWAP = NULL)]
    })
  }

  if (align_time) {
    dt[, t_grps := grp_interval(Time, intv = intv, grp_id = FALSE), by = c("Code", "Date")]
    x <- dt[, list(Open  = first(Open),
                   High  = max(High),
                   Low   = min(Low),
                   Close = last(Close),
                   Vol   = sum(Vol),
                   Tnvr  = sum(Tnvr),
                   VWAP  = last(VWAP)), by = c("Code", "Date", "t_grps")]
    data.table::setnames(x, "t_grps", "Time")
  } else {
    dt[, t_grps := grp_interval(Time, intv = intv, grp_id = TRUE), by = c("Code", "Date")]
    x <- dt[, list(Time  = last(Time),
                   Open  = first(Open),
                   High  = max(High),
                   Low   = min(Low),
                   Close = last(Close),
                   Vol   = sum(Vol),
                   Tnvr  = sum(Tnvr),
                   VWAP  = last(VWAP)), by = c("Code", "Date", "t_grps")]
    x[, t_grps := NULL]
  }
  dt[, t_grps := NULL]

  x[, Avg := Tnvr / Vol]
  x
}

#' Resample OHLC variance 2:
#'
#' Handles data of different dates.
resample_ohlc_ashare2 <- function(dt, period, align_time) {

  data.table::setkeyv(dt, c("Code", "Date", "Time"))

  if (is.null(dt$VWAP)) {
    dt[, `:=`(cum_Vol = cumsum(Vol), cum_Tnvr = cumsum(Tnvr)), by = c("Code", "Date")]
    dt[, `:=`(VWAP = cum_Tnvr / cum_Vol)]
    on.exit({
      dt[, `:=`(cum_Vol = NULL, cum_Tnvr = NULL, VWAP = NULL)]
    })
  }

  if (align_time) {
    t_grps <- dt[, .(Time = grp_period(Time, period = period, grp_id = FALSE)), by = c("Code", "Date")]
    x <- dt[, list(Open  = first(Open),
                   High  = max(High),
                   Low   = min(Low),
                   Close = last(Close),
                   Vol   = sum(Vol),
                   Tnvr  = sum(Tnvr),
                   VWAP  = last(VWAP)), by = t_grps]
  } else {
    t_grps <- dt[, .(TimeGrps = grp_period(Time, period = period, grp_id = TRUE)), by = c("Code", "Date")]
    x <- dt[, list(Time  = last(Time),
                   Open  = first(Open),
                   High  = max(High),
                   Low   = min(Low),
                   Close = last(Close),
                   Vol   = sum(Vol),
                   Tnvr  = sum(Tnvr),
                   VWAP  = last(VWAP)), by = t_grps]
    x[, TimeGrps := NULL]
  }

  x[, Avg := Tnvr / Vol]
  x
}
