#Discounted Assets on Prob Distribution

gastwirth <- function(x) {
  q <- stats::quantile(x, probs = c(1/3, 1/2, 2/3), na.rm = TRUE)
  sum(c(0.3, 0.4, 0.3) * q)
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
#' DAPD is determined by estimated growth from historic ROE and market valuation
#' preference from distribution of recent intraday PB values.
#'
#' Returned value is a data.table of current prices (discounted) and target prices
#' in N years.
#'
#' Data is provided by Tushare Pro API.
#'
#' @param api a tsapi object
#' @param ts_code Tushare code
#' @param div_rate estimated dividen rate
#' @param discount_rate discount rate
#' @param N year of stable growth
#' @param start_date start date of historic growth
#' @param intraday_freq intraday data frequency
#' @param intraday_bar bars of intraday data used
#' @param arb_end an arbitrary end date for testing only
#'
#' @return a data.table
#' @export
#'
#' @examples
#' \dontrun{
#'     api <- TushareApi("YOUR TUSHARE API TOKEN")
#'     #Valuate 601117.SH base on 3 yr growth. Dividen rate is simply taken from
#'     #latest data and discount rate is assumed 10%
#'     dapdv(api, "601117.SH", 0.0191 * 11.08, 0.1, 3, start_date = "2010-01-01")
#' }
dapdv <- function(api, ts_code, div_rate, discount_rate, N = 3, start_date = "", intraday_freq = 15, intraday_bar = 4000, arb_end = "") {

  #get recent intraday OHLC data
  ohlci <- intraday(api, ts_code = ts_code, freq = as.character(intraday_freq), end_date = arb_end)
  #calculate prob distribution of  recently traded PB
  bars <- seq.int(to = nrow(ohlci), length.out = intraday_bar)
  pb_band <- eval_bands(ohlci$pb[bars])
  band_order <- order(pb_band$mu)
  mu <- pb_band$mu[band_order]
  sigma <- pb_band$sigma[band_order]
  lambda <- pb_band$lambda[band_order]

  #get historic OHLC data
  ohlcd <- daily(api, ts_code = ts_code, start_date = start_date, end_date = arb_end)
  #calculate ROE
  ohlcd[, roe := pb / pe_ttm]
  #estimate ROE using Gastwirth estimator
  est_roe <- gastwirth(ohlcd$roe)

  #Estimate equity growth in N years.
  est_growth <- (1 + est_roe * (1 - div_rate))^N
  target_price <- sort(mu) * est_growth / ohlcd[.N, pb] * ohlcd[.N, close]

  discount_price <- target_price * (1 + discount_rate)^(1 - N)

  data.table::data.table(discounted_price = discount_price, target = target_price, Expected_PB = mu, sigma = sigma, lambda = lambda)
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
