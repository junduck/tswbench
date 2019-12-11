#TODO: C implementation for performance

#' Construct volume clock by volume
#'
#' @param dt a data.table/data.frame
#' @param vol volume per tick
#' @param p_col price colume
#' @param v_col volume colume
#'
#' @return a data.table of average price, high and low
#' @export
#'
vclock_volume <- function(dt, vol, p_col = "Price", v_col = "Volume") {

  data <- data.table::copy(dt)
  data[, val := get(p_col) * get(v_col)]

  n <- nrow(data)
  nbin <- 0

  lead_vol <- 0
  lead_pri <- 0
  lead_tot <- 0

  acc  <- numeric(0)
  high <- numeric(0)
  low  <- numeric(0)

  intv_start <- 1

  for (i in seq_len(n)) {
    #accumulate val
    lead_tot <- lead_vol * lead_pri + data$val[i]
    #accumulate vol
    lead_vol <- lead_vol + data[[v_col]][i]
    #calculate avg price
    lead_pri <- lead_tot / lead_vol
    if (lead_vol >= vol) {
      nbin <- lead_vol %/% vol
      acc <- c(acc, rep(lead_pri, nbin))
      lead_vol <- lead_vol %% vol
      #update high, low
      high <- c(high, rep(max(data[[p_col]][intv_start:i]), nbin))
      low  <- c(low,  rep(min(data[[p_col]][intv_start:i]), nbin))
      #update interval
      intv_start <- i + 1L
    }
  }
  #tail
  acc  <- c(acc,  lead_pri)
  if (intv_start <= n) {
    high <- c(high, max(data[[p_col]][intv_start:n]))
    low  <- c(low,  min(data[[p_col]][intv_start:n]))
  } else {
    high <- c(high, lead_pri)
    low  <- c(low, lead_pri)
  }


  data.table::data.table(
    avg_price = acc,
    high      = high,
    low       = low
  )
}

#' Construct volume clock by total value
#'
#' @param dt a data.table/data.frame
#' @param val total value per tick
#' @param p_col price colume
#' @param v_col volume colume
#'
#' @return a data.table of average price, high and low
#' @export
#'
vclock_value <- function(dt, val, p_col = "Price", v_col = "Volume") {

  data <- data.table::copy(dt)
  data[, val := get(p_col) * get(v_col)]

  n <- nrow(data)
  nbin <- 0

  lead_vol <- 0
  lead_pri <- 0
  lead_tot <- 0

  acc  <- numeric(0)
  high <- numeric(0)
  low  <- numeric(0)
  vol  <- numeric(0)

  intv_start <- 1

  for (i in seq_len(n)) {
    #accumulate val
    lead_tot <- lead_tot + data$val[i]
    #accumulate vol
    lead_vol <- lead_vol + data[[v_col]][i]
    #calculate avg price
    lead_pri <- lead_tot / lead_vol
    if (lead_tot >= val) {
      nbin <- lead_tot %/% val
      acc <- c(acc, rep(lead_pri, nbin))
      lead_tot <- lead_tot %% val
      lead_vol <- lead_tot / lead_pri
      #update high, low
      high <- c(high, rep(max(data[[p_col]][intv_start:i]), nbin))
      low  <- c(low,  rep(min(data[[p_col]][intv_start:i]), nbin))
      #update interval
      intv_start <- i + 1L
    }
  }

  #tail
  acc  <- c(acc,  lead_pri)
  if (intv_start <= n) {
    high <- c(high, max(data[[p_col]][intv_start:n]))
    low  <- c(low,  min(data[[p_col]][intv_start:n]))
  } else {
    high <- c(high, lead_pri)
    low  <- c(low, lead_pri)
  }

  data.table::data.table(
    avg_price = acc,
    high      = high,
    low       = low
  )
}
