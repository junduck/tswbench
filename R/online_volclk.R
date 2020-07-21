#' Construct volume clock from order
#'
#' @param bin volume bin size
#' @param return return data type
#'
#' @return a stateful online function
#' @export
#'
make_volclk_order <- function(bin, return = c("l", "m")) {

  return <- match.arg(return)
  bin <- as.double(bin)
  stopifnot(bin > 0)

  calc <- new(ocls_volclk_order, bin)
  switch(return,
         l = function(price, volume) {
           val <- calc$update(price, volume)
           list(
             Open    = val[, 1],
             High    = val[, 2],
             Low     = val[, 3],
             Close   = val[, 4],
             VWAP    = val[, 5],
             Bins    = val[, 6],
             TotBins = val[, 7]
           )
         },
         m = function(price, volume) {
           val <- calc$update(price, volume)
           colnames(val) <- c("Open", "High", "Low", "Close", "VWAP", "Bins", "TotBins")
           val
         })
}

#' Construct volume clock from tick
#'
#' @param bin volume bin size
#' @param return return data type
#'
#' @return a stateful online function
#' @export
#'
make_volclk_tick <- function(bin, return = c("l", "m")) {

  return <- match.arg(return)
  bin <- as.double(bin)
  stopifnot(bin > 0)

  calc <- new(ocls_volclk_tick, bin)
  switch(return,
         l = function(price, volume) {
           val <- calc$update(price, volume)
           list(
             Open    = val[, 1],
             High    = val[, 2],
             Low     = val[, 3],
             Close   = val[, 4],
             VWAP    = val[, 5],
             Bins    = val[, 6],
             TotBins = val[, 7]
           )
         },
         m = function(price, volume) {
           val <- calc$update(price, volume)
           colnames(val) <- c("Open", "High", "Low", "Close", "VWAP", "Bins", "TotBins")
           val
         })
}
