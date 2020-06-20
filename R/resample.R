#' Generate resample time intervals by perios, generic version
#'
#' Calculate suitable time intervals for intraday data resampling. Trading hours
#' are defined in session, a list of integer vectors, each defining a trading
#' session by 6 integers: session start hour, minute, second, session end hour,
#' minute, second.
#'
#' @param t a timestamp, POSIXct and ITime are supported
#' @param period resample time period in seconds
#' @param session session definition
#'
#' @return a vector of POSIXct/ITime depending on t
#' @export
#'
resample_intraday <- function(t, period, session) {

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

#' Generate resample time intervals by period, A-share version
#'
#' For A-share intraday only. 9:30 and 1:00 is sampled to reflect orders made
#' in opening session and lunch break. Extended hour (3:00 - 3:30) is not included.
#'
#' @param t a timestamp, POSIXct and ITime are supported
#' @param period resample time period in seconds
#' @param session session of trading day
#'
#' @return a vector of POSIXct/ITime depending on t
#' @export
#'
resample_intraday_ashare <- function(t, period, session = c("day", "morning", "afternoon")) {

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
    tz <- attr(tl1, "tzone")
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

#' Group time by time intervals
#'
#' @param t a vector of POSIXct/ITime
#' @param intv time intervals to group
#' @param grp_id wheter to return group id or grouped time
#'
#' @return a vector of group id or POSIXct/ITime
#' @export
#'
group_by_interval <- function(t, intv, grp_id = TRUE) {

  t_grps <- findInterval(t, intv, left.open = TRUE)
  if (grp_id) {
    t_grps
  } else {
    n <- length(intv)
    t_grps <- t_grps + 1L
    if (any(t_grps > n)) {
      intv <- c(intv, data.table::last(t))
    }
    intv[t_grps]
  }
}

group_by_period <- function(t, period, grp_id = TRUE) {

  intv <- resample_intraday_ashare(t, period = period, session = "day")
  group_by_interval(t = t, intv = intv, grp_id = grp_id)
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

resample_ohlc_ashare1 <- function(dt, period, align_time) {

  data.table::setkeyv(dt, c("Code", "Date", "Time"))
  intv <- resample_intraday_ashare(dt$Time, period = period, session = "day")

  if (is.null(dt$VWAP)) {
    dt[, `:=`(cum_Vol = cumsum(Vol), cum_Tnvr = cumsum(Tnvr)), by = c("Code", "Date")]
    dt[, `:=`(VWAP = cum_Tnvr / cum_Vol)]
    on.exit({
      dt[, `:=`(cum_Vol = NULL, cum_Tnvr = NULL, VWAP = NULL)]
    })
  }

  if (align_time) {
    dt[, t_grps := group_by_interval(Time, intv = intv, grp_id = FALSE), by = c("Code", "Date")]
    x <- dt[, list(Open  = first(Open),
                   High  = max(High),
                   Low   = min(Low),
                   Close = last(Close),
                   Vol   = sum(Vol),
                   Tnvr  = sum(Tnvr),
                   VWAP  = last(VWAP)), by = c("Code", "Date", "t_grps")]
    data.table::setnames(x, "t_grps", "Time")
  } else {
    dt[, t_grps := group_by_interval(Time, intv = intv, grp_id = TRUE), by = c("Code", "Date")]
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
    t_grps <- dt[, .(Time = group_by_period(Time, period = period, grp_id = FALSE)), by = c("Code", "Date")]
    x <- dt[, list(Open  = first(Open),
                   High  = max(High),
                   Low   = min(Low),
                   Close = last(Close),
                   Vol   = sum(Vol),
                   Tnvr  = sum(Tnvr),
                   VWAP  = last(VWAP)), by = t_grps]
  } else {
    t_grps <- dt[, .(TimeGrps = group_by_period(Time, period = period, grp_id = TRUE)), by = c("Code", "Date")]
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
