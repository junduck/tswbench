detect_acc <- function(dt) {

  # Check if volume is sorted when ordered by time
  data.table::setkeyv(dt, c("Code", "Date", "Time"))

  # Only sample ~1% of all codes
  codes <- unique(dt$Code)
  n <- length(codes) %/% 100L + 1L
  codes <- sample(codes, n)

  if (is.null(dt[["Vol"]])) {
    test <- dt[Code %chin% codes, .(unsorted = is.unsorted(Volume)), by = c("Code", "Date")]
  } else {
    test <- dt[Code %chin% codes, .(unsorted = is.unsorted(Vol)), by = c("Code", "Date")]
  }

  !any(test$unsorted)
}

#' Transform to OHLC from tick data / order book data
#'
#' Input data dt should have these columns:
#' Code, Date, Time, Price, Volume (Vol), Turnover (Tnvr)
#'
#' Output data contains these columns:
#' Code, Date, Time, Open, High, Low, Close, Vol, Tnvr, VWAP, Avg
#'
#' Note: this function sets key to dt on Code, Date, Time
#'
#' @param dt tick data or order book data
#' @param vol_acc whether volume data is accumulated, auto detected if not supplied
#'
#' @return a data.table
#' @export
#'
transform_ohlc <- function(dt, vol_acc) {

  # first() and last() rely on correct time order
  data.table::setkeyv(dt, c("Code", "Date", "Time"))

  # Detect if volume is accumulated
  if (missing(vol_acc)) {
    vol_acc <- detect_acc(dt = dt)
  }

  # TODO: GForce does not work with expr like sum(get(vol_col)), for performance
  # we have to hard-code supported columns, not the most elegant way but works
  if (is.null(dt[["Vol"]])) {
    # Volume and Turnover
    data.table::setnames(dt, old = c("Volume", "Turnover"), new = c("Vol", "Tnvr"))
    on.exit({
      # Recover original name
      data.table::setnames(dt, old = c("Vol", "Tnvr"), new = c("Volume", "Turnover"))
    })
  }

  if (vol_acc) {
    # Convert accumulated Volume/Turnover to sliced Volume/Turnover
    dt[, `:=`(tmp_vol  = Vol  - data.table::shift(Vol,  n = 1L, fill = 0),
              tmp_tnvr = Tnvr - data.table::shift(Tnvr, n = 1L, fill = 0)),
       by = c("Code", "Date")]
    ohlc <- dt[, list(Open  = first(Price),
                      High  = max(Price),
                      Low   = min(Price),
                      Close = last(Price),
                      Vol   = sum(tmp_vol),
                      Tnvr  = sum(tmp_tnvr)),
               by = c("Code", "Date", "Time")][Vol != 0L]
    # Remove temporary columns
    dt[, `:=`(tmp_vol = NULL, tmp_tnvr = NULL)]
  } else {
    ohlc <- dt[, list(Open  = first(Price),
                      High  = max(Price),
                      Low   = min(Price),
                      Close = last(Price),
                      Vol   = sum(Vol),
                      Tnvr  = sum(Tnvr)),
               by = c("Code", "Date", "Time")]
  }

  data.table::setkeyv(ohlc, c("Code", "Date", "Time"))
  # Potential cumsum integer overflow, cast to numeric first
  ohlc[, Vol := as.numeric(Vol)][, `:=`(VWAP = cumsum(Tnvr) / cumsum(Vol),
                                        Avg = Tnvr / Vol),
                                 by = c("Code", "Date")]

  ohlc
}

#' Transform to price distribution from tick data / order book data
#'
#' Input data dt should have these columns:
#' Code, Date, Time, Price, Volume (Vol), Turnover (Tnvr)
#'
#' Output data contains these columns:
#' Code, Date, Price, Vol, Tnvr
#'
#' @param dt tick data or order book data
#' @param vol_acc whether volume data is accumulated, auto detected if not supplied
#'
#' @return a data.table
#' @export
#'
transform_price <- function(dt, vol_acc) {

  if (missing(vol_acc)) {
    vol_acc <- detect_acc(dt = dt)
  }

  if (is.null(dt[["Vol"]])) {
    # Volume and Turnover
    data.table::setnames(dt, old = c("Volume", "Turnover"), new = c("Vol", "Tnvr"))
    on.exit({
      # Recover original name
      data.table::setnames(dt, old = c("Vol", "Tnvr"), new = c("Volume", "Turnover"))
    })
  }

  if (vol_acc) {
    # Convert accumulated Volume/Turnover to sliced Volume/Turnover
    dt[, `:=`(tmp_vol  = Vol  - data.table::shift(Vol,  n = 1L, fill = 0),
              tmp_tnvr = Tnvr - data.table::shift(Tnvr, n = 1L, fill = 0)),
       by = c("Code", "Date")]
    price <- dt[, list(Vol  = sum(as.numeric(tmp_vol)),
                       Tnvr = sum(tmp_tnvr)),
                by = c("Code", "Date", "Price")]
    # Remove temporary columns
    dt[, `:=`(tmp_vol = NULL, tmp_tnvr = NULL)]
  } else {
    price <- dt[, list(Vol  = sum(as.numeric(Vol)),
                       Tnvr = sum(Tnvr)),
                by = c("Code", "Date", "Price")]
  }

  data.table::setkeyv(price, c("Code", "Date", "Price"))

  price
}
