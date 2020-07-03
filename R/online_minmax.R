#' Online moving min/max
#'
#' @param window moving window size
#' @param arg index: return min/max index within moving window, distance: return the reverse of index
#' @param ... not used
#'
#' @return a stateful online function
#' @export
#'
make_moving_min <- function(window, ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  calc <- new(ocls_moving_min, window)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_moving_min
#' @export
#'
make_moving_max <- function(window, ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  calc <- new(ocls_moving_max, window)
  function(x) {
    calc$update(x)
  }
}

#' @rdname make_moving_min
#' @export
#'
make_moving_argmin <- function(window, arg = c("index", "distance"), ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  arg = match.arg(arg)
  delta_w = window + 1L;

  calc <- new(ocls_moving_argmin, window)
  switch(arg,
         distance = function(x) {
           calc$update(x)
         },
         index    = function(x) {
           delta_w - calc$update(x)
         })
  function(x) {
    return(calc$update(x))
  }
}

#' @rdname make_moving_min
#' @export
#'
make_moving_argmax <- function(window, arg = c("index", "distance"), ...) {

  window <- as.integer(window)
  stopifnot(window >= 3L)

  arg = match.arg(arg)
  delta_w = window + 1L;

  calc <- new(ocls_moving_argmax, window)
  switch(arg,
         distance = function(x) {
           calc$update(x)
         },
         index    = function(x) {
           delta_w - calc$update(x)
         })
}
