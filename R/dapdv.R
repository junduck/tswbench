#Discounted Assets on Prob Distribution

gastwirth <- function(x, na.rm = TRUE) {
  q <- stats::quantile(x, probs = c(1/3, 1/2, 2/3), na.rm = TRUE)
  sum(c(0.3, 0.4, 0.3) * q)
}

eval_bands <- function(x, k) {

  x <- x[!is.na(x)]

  #perform k-means
  cl <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k = k, method = "linear", estimate.k = "BIC")
  di <- mixtools::normalmixEM(x, mu = cl$centers, maxit = 2000)

  mu_order <- order(di$mu, decreasing = TRUE)

  list(
    mu     = di$mu[mu_order],
    sigma  = di$sigma[mu_order],
    lambda = di$lambda[mu_order],
    distr  = di
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
#' @param discount_rate discount rate
#' @param div_rate estimated dividen rate, default is estimated from historic div rate
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
dapdv <- function(api, ts_code, discount_rate, div_rate = NULL, N = 3, start_date = "2010-01-01", intraday_freq = 15, intraday_bar = 4000, arb_end = "") {

  #Query recent intraday OHLC data, which is used to evaluate PB distribution
  ohlci <- intraday(api, ts_code = ts_code, freq = as.character(intraday_freq), end_date = arb_end)

  #Calculate prob distribution of recently traded PB
  pb_band <- eval_bands(ohlci$pb[seq.int(to = nrow(ohlci), length.out = intraday_bar)], k = 3)

  #Extract expected PB (and corresponding sigma/SD, lambda/scale)
  est_PB        <- pb_band$mu
  est_PB_sigma  <- pb_band$sigma
  est_PB_lambda <- pb_band$lambda

  #Query historic daily OHLC data, which is used to estimate ROE
  ohlcd <- daily(api, ts_code = ts_code, start_date = start_date, end_date = arb_end)
  #Calculate ROE
  ohlcd[, roe := pb / pe_ttm]
  ohlcd[, div_rate := dv_ttm * pe_ttm / 100.0]

  #Estimate growth, based on Gastwirth estimator and historic ROE distribution
  roe_band       <- eval_bands(ohlcd$roe, k = 3)
  est_roe        <- c(gastwirth(ohlcd$roe), roe_band$mu)
  est_roe_sigma  <- c(NA,                   roe_band$sigma)
  est_roe_lambda <- c(NA,                   roe_band$lambda)
  if (is.null(div_rate)) {
    est_div <- gastwirth(ohlcd$div_rate)
  } else {
    est_div <- div_rate
  }

  #Estimate equity in N years.
  est_growth <- (1 + est_roe * (1 - est_div))^N

  #Estimate target price for each growth estimation
  # pricing model is derived from:
  #   market_cap = equity * PB
  target_price <- numeric(0)
  for (i in seq_along(est_growth)) {
    tmp <- (ohlcd[.N, close] / ohlcd[.N, pb]) * est_growth[i] * est_PB
    target_price <- c(target_price, tmp)
  }
  discount_price <- target_price * (1 + discount_rate)^(1 - N)

  ans <- data.table::data.table(discounted = discount_price,
                                target     = target_price,
                                Est_ROE    = rep(est_roe,        each = length(est_PB)),
                                ROE_sigma  = rep(est_roe_sigma,  each = length(est_PB)),
                                ROE_lambda = rep(est_roe_lambda, each = length(est_PB)),
                                Est_PB     = est_PB,
                                PB_sigma   = est_PB_sigma,
                                PB_lambda  = est_PB_lambda)
  attr(ans, "distr_ROE") <- roe_band$distr
  attr(ans, "distr_PB") <- pb_band$distr

  ans
}

#' Plot dapdv prob density distribution
#'
#' @param dt result from dapdv
#' @param plot value to plot
#'
#' @return a ggplot object
#' @export
#'
dapdv_plot <- function(dt, plot = c("ROE", "PB")) {

  pb <- match.arg(plot)
  if (pb == "ROE") {
    EM <- attr(dt, "distr_ROE")
  } else {
    EM <- attr(dt, "distr_PB")
  }

  x       <- with(EM, seq(max(min(x) - max(sigma), 0), max(x) + max(sigma), len = 1000))
  pars    <- with(EM, data.frame(comp = colnames(posterior), mu, sigma,lambda))
  em.df   <- data.frame(x = rep(x,each = nrow(pars)), pars)
  em.df$y <- with(em.df, sdnorm(x, mu = mu, sigma = sigma, lambda = lambda))

  ggplot(data.frame(x = EM$x), aes(x, y = ..density..)) +
    geom_histogram(fill = NA, color = "black", bins = 50)+
    geom_polygon(data = em.df,aes(x, y, fill = comp), color = "grey50", alpha = 0.3) +
    scale_fill_discrete(plot, labels = format(em.df$mu, digits = 3)) +
    theme_bw()
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
