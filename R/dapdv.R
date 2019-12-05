#Discounted Assets on Prob Distribution

get_intraday <- function(intraday, api) {

  ohlc <- data.table::copy(intraday)

  data.table::setkeyv(ohlc, "trade_time")
  n <- nrow(ohlc)
  code <- ohlc$ts_code[n]
  date <- as.Date(ohlc$trade_time[n])

  indi <- api$daily_basic(ts_code = code, end_date = date,
                          fields = c("trade_date", "close", "pe_ttm", "pb", "ps_ttm", "total_mv"))
  indi[, td_posix := as.POSIXct(trade_date)]
  data.table::setkeyv(indi, "td_posix")

  indi[, pe_p := pe_ttm * close]
  indi[, pb_p := pb     * close]
  indi[, ps_p := ps_ttm * close]

  dt <- indi[ohlc, roll = TRUE]
  dt[, pe := pe_p / i.close]
  dt[, pb := pb_p / i.close]
  dt[, ps := ps_p / i.close]

  dt[, close := i.close]
  dt[, trade_time := td_posix]

  dt[, c("ts_code", "trade_time", "close", "pe", "pb", "ps", "vol")]
}

get_assets <- function(ts_code, start_date = "", api) {

  inc <-       api$income(ts_code = ts_code,
                          start_date = start_date,
                          report_type = 2,
                          fields = c("ts_code", "end_date", "n_income", "update_flag"))
  bal <- api$balancesheet(ts_code = ts_code,
                          start_date = start_date,
                          fields = c("ts_code", "end_date", "total_share", "total_assets", "total_liab", "update_flag"))
  inc[, rinc := roll::roll_sum(n_income, width = 4)]

  data.table::setkeyv(inc, "end_date")
  data.table::setkeyv(bal, "end_date")
  inc[bal, ][!is.na(ts_code), ][, c("update_flag", "i.update_flag", "i.ts_code") := NULL][, total_equity := total_assets - total_liab]
}

gastwirth <- function(x) {
  q <- quantile(x, probs = c(1/3, 1/2, 2/3), na.rm = TRUE)
  sum(c(0.3, 0.4, 0.3) * q)
}

est_return <- function(assets, N, drop_out = 0.25, target = c("roe", "roa")) {

  target <- match.arg(target)
  if (target == "roe") {
    val <- assets$rinc / assets$total_equity
  } else {
    val <- assets$rinc / assets$total_assets
  }

  #Discount
  (1 + gastwirth(val) * (1 - drop_out))^N
}

eval_bands <- function(x) {

  #perform k-means
  cl <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k = c(1, 120), method = "linear", estimate.k = "BIC")
  #we only need 3 clusters at max
  n <- length(cl$centers)
  if (n <= 3) {
    ce <- cl$centers
  } else {
    ce <- cl$centers[c(1, n %/% 2L, n)]
  }

  #perform EM
  di <- mixtools::normalmixEM(x, mu = ce)

  list(
    mu     = di$mu,
    sigma  = di$sigma,
    lambda = di$lambda
  )
}

sdnorm <- function(x, mu, sigma, lambda) {

  lambda * stats::dnorm(x, mean = mu, sd = sigma)
}

#' Estimate Target Price
#'
#' Discounted Assets on Probability Distribution is an experimental valuation
#' method to estimate the value of an investment based on its future assets.
#' DAPD is determined by estimated growth from historic ROA (or ROE if high
#' leverage ratio) and distribution of market preference from recent intraday
#' data.
#'
#' Returned value is a list of estimated market cap / price (low, reasonable, high)
#' and estimated market cap / price in N years.
#'
#' Data is provided by Tushare Pro API.
#'
#' @param api a tsapi object
#' @param ts_code Tushare code
#' @param div_yield estimated dividen yield
#' @param risk_free risk-free rate
#' @param N year to maturity
#' @param mode growth estimate mode (auto, roe or roa)
#' @param start_date start date of historic data
#' @param intraday_freq intraday data frequency
#' @param intraday_bar bars of intraday data used
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#'     api <- TushareApi("YOUR TUSHARE API TOKEN")
#'     #Valuate 600900.SH base on 3 yr growth. Dividen yield is simply taken from latest data
#'     #and risk-free rate is based on 3 yr China Treasury Bond yield.
#'     dapdv(api, "600900.SH", div_yield = 0.0379, risk_free = 0.028, N = 3, start_date = "2010-01-01")
#'     dapdv(api, "000001.SZ", div_yield = 0.0084, risk_free = 0.026, N = 1, start_date = "2010-01-01")
#' }
dapdv <- function(api, ts_code, div_yield, risk_free, N = 3, mode = c("auto", "roe", "roa"),
                  start_date = "", intraday_freq = 15, intraday_bar = 4000) {

  mode <- match.arg(mode)

  freq <- paste0(intraday_freq, "min")
  p <- api$stk_mins(ts_code = ts_code, freq = freq)
  np <- nrow(p)
  bar <- seq.int(to = np, length.out = intraday_bar)
  perf <- get_intraday(p[bar, ], api)
  bands <- eval_bands(perf$pb)

  a <- get_assets(ts_code, start_date = start_date, api)
  if (mode == "auto") {
    ratio <- median(a$total_liab / a$total_assets, na.rm = TRUE)
    if (ratio < 0.5) {
      mode = "roa"
    } else {
      mode = "roe"
    }
  }
  na <- nrow(a)
  r <- est_return(a, N = N, drop_out = div_yield * gastwirth(perf$pe), target = mode)

  discount <- (1 + risk_free)^N
  val <- sort(bands$mu) * a$total_equity[na] * r
  list(
    market_cap   = val / discount,
    price        = val / discount / a$total_share[na],
    N_market_cap = val,
    N_price      = val / a$total_share[na]
  )
}

#' Run dapdv shiny app
#'
#' @export
run_dapdv <- function() {

  appDir <- system.file("shiny-app", "dapdv", package = "tswbench")
  if (appDir == "") {
    stop("Failed to locate dapdv app. Please re-install `tswbench`.")
  }

  shiny::runApp(appDir, display.mode = "normal")
}
