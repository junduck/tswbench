#' Online exponential moving average
#'
#' Avalable variances are:
#' make_ema: normal EMA
#' make_dema: double EMA
#' make_tema: triple EMA
#' make_zlema: zero lag EMA
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_ema <- function(period, ...) {

  a <- 2.0 / (period + 1.0)
  ema <- 0.0
  n <- FALSE

  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      if (n) {
        ema <<- (x[i] - ema) * a + ema
      } else {
        ema <<- x[i]
        n <<- TRUE
      }
      ans[i] <- ema
    }
    ans
  }
}

#' @rdname make_ema
#' @export
#'
make_dema <- function(period, ...) {

  a    <- 2.0 / (period + 1.0)
  ema  <- 0.0
  ema2 <- 0.0
  n <- FALSE

  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      if (n) {
        ema  <<- (x[i] - ema) * a + ema
        ema2 <<- (ema - ema2) * a + ema2
      } else {
        ema <<- x[i]
        ema2 <<- x[i]
        n <<- TRUE
      }
      ans[i] <- ema * 2 - ema2
    }
    ans
  }
}

#' @rdname make_ema
#' @export
#'
make_tema <- function(period, ...) {

  a    <- 2.0 / (period + 1.0)
  ema  <- 0.0
  ema2 <- 0.0
  ema3 <- 0.0
  n <- FALSE

  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      if (n) {
        ema  <<- (x[i] - ema)  * a + ema
        ema2 <<- (ema  - ema2) * a + ema2
        ema3 <<- (ema2 - ema3) * a + ema3
      } else {
        ema  <<- x[i]
        ema2 <<- x[i]
        ema3 <<- x[i]
      }
      ans[i] <- 3.0 * ema - 3.0 * ema2 + ema3
    }
    ans
  }
}

#' @rdname make_ema
#' @export
#'
make_zlema <- function(period, ...) {

  lag <- (as.integer(period) - 1L) %/% 2L
  a <- 2.0 / (period + 1.0)
  n <- 0L

  buf <- collections::queue()
  ema <- 0.0

  function(x) {

    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      buf$push(x[i])
      if (n < lag) {
        n <<- n + 1L
        ema <<- x[i]
      } else {
        datum <- x[i] + (x[i] - buf$pop())
        ema <<- (datum - ema) * a + ema
      }
      ans[i] <- ema
    }
    ans
  }
}

#' Wilders smoothing average
#'
#' @param period smoothing period
#' @param ... not used
#'
#' @return a stateful online function
#' @export
#'
make_wilders <- function(period, ...) {

  a <- 1.0 / period
  n <- FALSE

  wilders <- 0.0

  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      if (n) {
        wilders <<- (x[i] - wilders) * a + wilders
      } else {
        wilders <<- x[i]
        n <<- TRUE
      }
      ans[i] <- wilders
    }
    ans
  }
}

#' Simple moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_sma <- function(period, ...) {

  p <- period
  n <- 0L
  buf <- collections::queue()

  s <- 0.0

  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      buf$push(x[i])
      if (n < p) {
        s <<- s + x[i]
        n <<- n + 1L
      } else {
        s <<- s + x[i] - buf$pop()
      }
      ans[i] <- s / n
    }
    ans
  }
}

#' Weighted moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_wma <- function(period, ...) {

  p <- period
  n <- 0L
  buf <- collections::queue()

  wtot <- 0L
  sw <- 0.0
  s  <- 0.0

  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      buf$push(x[i])
      if (n < p) {
        n <<- n + 1L
        sw <<- sw + x[i] * n
        s  <<- s  + x[i]
        wtot <<- wtot + n
      } else {
        sw <<- sw + x[i] * n - s
        s  <<- s  + x[i] - buf$pop()
      }
      ans[i] <- sw / wtot
    }
    ans
  }
}

#' Kaufman adaptive moving average
#'
#' @param period MA period
#' @param period_short short period for efficiency ratio
#' @param period_long long period for efficiency ratio
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_kama <- function(period, period_short = 2, period_long = 30, ...) {

  p <- period
  n <- 0L
  buf <- collections::deque()

  a_short <- 2.0 / (period_short + 1.0)
  a_long  <- 2.0 / (period_long  + 1.0)
  a_delta <- a_short - a_long
  a0 <- (a_delta + a_long) * (a_delta + a_long)
  sdelta <- 0.0

  kama <- 0.0
  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      if (n) {
        sdelta <<- sdelta + abs(x[i] - buf$peek())
      }
      buf$push(x[i])
      if (n < p) {
        n <<- n + 1L
        kama <<- x[i]
      } else {
        old <- buf$popleft()
        if (sdelta != 0.0) {
          e <- abs(x[i] - old) / sdelta * a_delta + a_long
          a <- e * e
        } else {
          a <- a0
        }
        kama   <<- (x[i] - kama) * a + kama
        ans[i] <- kama
        #update sdelta
        sdelta <<- sdelta - abs(old - buf$peekleft())
      }
    }
    ans
  }
}

#' Hull moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_hma <- function(period, ...) {

  p <- as.integer(period)
  m <- p %/% 2L
  s <- floor(sqrt(p))

  wma_p <- make_wma(period = p)
  wma_m <- make_wma(period = m)
  wma_s <- make_wma(period = s)

  function(x) {
    npt <- length(x)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      ans[i] <- wma_s(2 * wma_m(x[i]) - wma_p(x[i]))
    }
    ans
  }
}

#' Volume weighted moving average
#'
#' @param period MA period
#' @param ... not used
#'
#' @return an online function
#' @export
#'
make_vwma <- function(period, ...) {

  w <- period
  n <- 0L
  bufp <- collections::queue()
  bufv <- collections::queue()

  spv <- 0.0
  sv  <- 0.0

  function(p, v) {
    np <- length(p)
    nv <- length(v)
    if (np != nv) {
      stop("p and v have different lengths.", call. = FALSE)
    }
    ans <- vector(mode = "numeric", length = np)
    for (i in seq_len(np)) {
      pv <- p[i] * v[i]
      bufp$push(pv)
      bufv$push(v)
      if (n < w) {
        n <<- n + 1L
        spv <<- spv + pv
        sv  <<- sv  +  v
      } else {
        spv <<- spv + pv - bufp$pop()
        sv  <<- sv  +  v - bufv$pop()
      }
      ans[i] <- spv / sv
    }
    ans
  }
}
