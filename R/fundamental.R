#update_flag should be set explicitly

#' Query whole-market quarterly reports
#'
#' @param api a tsapi object
#' @param type type of report to query
#' @param period OPTIONAL, period to query
#' @param y OPTIONAL, year to query
#' @param q OPTIONAL, quarty to query
#' @param ... additional arguments passed. Please refer to Tushare manual
#' @param timeout timeout of the query
#'
#' @return a data.table
#' @export
#'
market_report <- function(api, type = c("income", "balancesheet", "cashflow", "forecast",
                                        "express", "fina_indicator", "fina_mainbz"),
                               period, y, q, ..., timeout = 60) {

  type <- match.arg(type)
  func <- paste0(type, "_vip")
  fdef <- get_fields_def(type)

  if (missing(period)) {
    if (missing(y) || missing(q)) {
      stop("At least period or year and quarter should be provided.")
    }
    y <- as.character(y)
    q <- suppressWarnings(as.integer(q))
    if (!chk_number_n(y, 4)) {
      stop("A valid year should be provided.")
    }
    q <- switch(q, "0331", "0630", "0931", "1231")
    if (is.null(q)) {
      stop("A valid quarter should be provided.")
    }
    period <- paste0(y, q)
  }

  qfunc <- `$.tsapi`(api, func)
  qfunc(period = period, ..., fields = fdef$field, timeout = timeout)
}
