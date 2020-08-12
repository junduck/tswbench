#' Select bookdepth columns from order book / tick data
#'
#' @param dt order book or tick data
#' @param book_depth max book depth
#' @param code_col column name of code, defaults to Code
#' @param date_col column name of date, defaults to NULL (not selected)
#' @param time_col column name of time, defaults to Time
#'
#' @return a data.table
#' @export
#'
select_bookdepth <- function(dt, book_depth = 5L,
                             code_col = "Code", date_col = NULL, time_col = "Time") {

  col_bidp <- paste0("Bid_P", seq.int(from = book_depth, to = 1L))
  col_bidv <- paste0("Bid_V", seq.int(from = book_depth, to = 1L))
  col_askp <- paste0("Ask_P", seq.int(from = 1L, to = book_depth))
  col_askv <- paste0("Ask_V", seq.int(from = 1L, to = book_depth))

  cols <- c(code_col, date_col, time_col,
            col_bidp, col_askp, col_bidv, col_askv)


  dt[, cols, with = FALSE]
}

#' Calculate price and volume spread from order book / tick data
#'
#' dt is changed by reference
#'
#' @param dt order book or tick data
#' @param book_depth max book depth
#' @param reorder whether to reorder columns
#'
#' @return a data.table, dt is changed by reference
#' @export
#'
spread_bookdepth <- function(dt, book_depth = 5L, reorder = TRUE) {

  dt[, Spread_P1 := Ask_P1 - Bid_P1]
  dt[, Spread_V1 := Bid_V1 - Ask_V1]

  if (reorder) {
    col_bidp <- paste0("Bid_P", seq.int(from = book_depth, to = 1L))
    col_bidv <- paste0("Bid_V", seq.int(from = book_depth, to = 1L))
    col_askp <- paste0("Ask_P", seq.int(from = 1L, to = book_depth))
    col_askv <- paste0("Ask_V", seq.int(from = 1L, to = book_depth))

    all_cols <- names(dt)
    cols <- c(col_bidp, "Spread_P1", col_askp, col_bidv, "Spread_V1", col_askv)

    data.table::setcolorder(dt, c(setdiff(all_cols, cols), cols))
  }

  dt
}

#' Filter order book / tick data by available book depth
#'
#' @param dt order book or tick data
#' @param avail_depth minimum available book depth
#' @param book_depth max book depth
#'
#' @return a data.table
#' @export
#'
filter_bookdepth <- function(dt, avail_depth = book_depth, book_depth = 5L) {

  if (avail_depth > book_depth) {
    avail_depth <- book_depth
  }

  col_bidv <- paste0("Bid_V", seq.int(from = 1L, to = avail_depth))
  col_askv <- paste0("Ask_V", seq.int(from = 1L, to = avail_depth))
  cols <- c(col_bidv, col_askv)

  filter <- dt[, rowSums(.SD == 0L), .SDcols = cols] == 0
  dt[filter]
}
