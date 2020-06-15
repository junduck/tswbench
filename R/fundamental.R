#update_flag should be set explicitly

#' Query whole-market quarterly reports
#'
#' @param type type of report to query
#' @param period OPTIONAL, period to query
#' @param y OPTIONAL, year to query
#' @param q OPTIONAL, quarty to query
#' @param ... additional arguments passed. Please refer to Tushare manual
#' @param api a tsapi object
#' @param timeout timeout of the query
#'
#' @return a data.table
#' @export
#'
report_market <- function(type = c("income", "balancesheet", "cashflow", "forecast",
                                   "express", "fina_indicator", "fina_mainbz"),
                          period, y, q, ..., api = TushareApi(), timeout = 60) {

  type <- match.arg(type)
  func <- paste0(type, "_vip")
  fdef <- get_api_field(type)

  if (missing(period)) {
    if (missing(y) || missing(q)) {
      stop("At least period or year and quarter should be provided.")
    }
    y <- as.character(y)
    if (!chk_number_n(y, 4)) {
      stop("A valid year should be provided.")
    }
    q <- suppressWarnings(as.integer(q))
    q <- switch(q, "0331", "0630", "0930", "1231")
    if (is.null(q)) {
      stop("A valid quarter should be provided.")
    }
    period <- paste0(y, q)
  }

  qfunc <- `$.tsapi`(api, func)
  qfunc(period = period, ..., fields = fdef$field, timeout = timeout)
}

#' Query quarterly reports
#'
#' @param type type of report to query
#' @param ts_code Tushare code
#' @param ... additional arguments passed. Please refer to Tushare manual
#' @param api a tsapi object
#' @param timeout timeout of the query
#'
#' @return a data.table
#' @export
#'
report_quarter <- function(type = c("income", "balancesheet", "cashflow", "forecast",
                                    "express", "fina_indicator", "fina_mainbz"),
                           ts_code, ..., api = TushareApi(), timeout = 60) {

  type <- match.arg(type)
  fdef <- get_api_field(type)

  qfunc <- `$.tsapi`(api, type)
  qfunc(ts_code = ts_code, ..., fields = fdef$field, timeout = timeout)
}
