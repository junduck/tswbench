gcd <- function(x, y) {
  while (y) {
    r <- x %% y
    x <- y
    y <- r
  }
  x
}

lcm <- function(x, y) {
  x %/% gcd(x, y) * y
}

#' Moving z-score algorithm for anomaly detection
#'
#' @param window moving window size
#' @param zscore z-score threshold for signal
#' @param attenu attenuation for signal
#'
#' @return an online function
#' @export
#'
moving_zscore <- function(window, zscore, attenu) {

  if (window < 3L) {
    stop("Window size must be at least 3.", call. = FALSE)
  }

  w <- window
  n <- 0L
  buf <- collections::deque()

  s   <- 0.0
  s2  <- 0.0
  avg <- 0.0
  var <- 0.0

  z <- zscore
  r <- attenu

  function(y) {

    npt <- length(y)
    ans <- vector(mode = "numeric", length = npt)
    for (i in seq_len(npt)) {
      if (n < w) {
        #Fill initial window
        new <- y[i]
        s <<- s + new
        n <<- n + 1L
        d0 <- 0.0
      } else {
        if (abs(y[i] - avg) > z * var) {
          ans[i] <- (y[i] - avg) / var
          new <- r * y[i] + (1.0 - r) * buf$peek()
        } else {
          new <- y[i]
        }
        old <- buf$popleft()
        s <<- s + new - old
        d0 <- old - avg
      }
      buf$push(new)
      d   <- new - avg
      dd0 <- d - d0
      s2  <<- s2 + d * d - d0 * d0 - dd0 * dd0 / n
      avg <<- s / n
      var <<- sqrt(s2 / (n - 1L))
    }
    ans
  }
}

#' Moving moments for up to 4th order
#'
#' @param window moving window size
#' @param order order of mements
#'
#' @return an online function
#' @export
#'
moving_moment <- function(window, order = 4L) {

  w  <- window
  n  <- 0L
  n2 <- 0L
  n3 <- 0L
  buf <- collections::deque()

  s <- s2 <- s3 <- s4 <- 0.0
  m <- 0.0

  d   <- 0.0
  d0  <- 0.0
  dd0 <- 0.0

  push <- function(x) {
    if (n < w) {
      s  <<- s + x
      n  <<- n + 1L
      n2 <<- n  * n
      n3 <<- n2 * n
    } else {
      old <- buf$popleft()
      s  <<- s + x - old
      d0 <<- old - m
    }
    buf$push(x)
    d   <<- x - m
    dd0 <<- d - d0
    m   <<- s / n
  }

  switch(order,
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 1L)
           for (i in seq_len(npt)) {
             push(x[i])
             ans[i, 1L] <- m
           }
           ans
         },
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 2L)
           for (i in seq_len(npt)) {
             push(x[i])
             s2 <<- s2 + d * d - d0 * d0 - dd0 * dd0 / n
             ans[i, 1L] <- m
             ans[i, 2L] <- s2 / n
           }
           ans
         },
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 3L)
           for (i in seq_len(npt)) {
             push(x[i])
             d_2   <- d * d
             d0_2  <- d0 * d0
             dd0_2 <- dd0 * dd0
             s3 <<- s3 + d_2 * d - d0_2 * d0 -
               3.0 * dd0 * (s2 + d_2 - d0_2) / n +
               2.0 * dd0_2 * dd0 / n2
             s2 <<- s2 + d_2 - d0_2 - dd0_2 / n
             ans[i, 1L] <- m
             ans[i, 2L] <- s2 / n
             ans[i, 3L] <- s3 / n
           }
           ans
         },
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 4L)
           for (i in seq_len(npt)) {
             push(x[i])
             d_2   <- d * d
             d0_2  <- d0 * d0
             dd0_2 <- dd0 * dd0
             s4 <<- s4 + d_2 * d_2 - d0_2 * d0_2 +
               4.0 * dd0 * (s3 + d_2 * d - d0_2 * d0) / n +
               6.0 * (s2 + d_2 - d0_2) * dd0_2 / n2 -
               3.0 * dd0_2 * dd0_2 / n3
             s3 <<- s3 + d_2 * d - d0_2 * d0 -
               3.0 * dd0 * (s2 + d_2 - d0_2) / n +
               2.0 * dd0_2 * dd0 / n2
             s2 <<- s2 + d_2 - d0_2 - dd0_2 / n
             ans[i, 1L] <- m
             ans[i, 2L] <- s2 / n
             ans[i, 3L] <- s3 / n
             ans[i, 4L] <- s4 / n
           }
           ans
         },
         stop("Unsupported order ", order, call. = FALSE))
}

#' Cumulative moments for up to 4th order
#'
#' @param order order of moments
#'
#' @return an online function
#' @export
#'
cumulative_moment <- function(order = 4L) {

  n  <- 0.0

  s <- s2 <- s3 <- s4 <- 0.0
  m <- 0.0

  d  <- 0.0

  push <- function(x) {
    n <<- n + 1
    s <<- s + x
    d <<- x - m
    m <<- s / n
  }

  switch(order,
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 1L)
           for (i in seq_len(npt)) {
             push(x[i])
             ans[i, 1L] <- m
           }
           ans
         },
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 2L)
           for (i in seq_len(npt)) {
             push(x[i])
             s2 <<- s2 + (1.0 - 1.0 / n) * d * d
             ans[i, 1L] <- m
             ans[i, 2L] <- s2 / n
           }
           ans
         },
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 2L)
           for (i in seq_len(npt)) {
             push(x[i])
             d_2 <- d * d
             s3 <<- s3 +
               (1.0 - 2.0 / n / n) * d_2 * d -
               3.0 * d * (s2 + d_2) / n
             s2 <<- s2 +
               (1.0 - 1.0 / n) * d_2
             ans[i, 1L] <- m
             ans[i, 2L] <- s2 / n
             ans[i, 3L] <- s3 / n
           }
           ans
         },
         function(x) {
           npt <- length(x)
           ans <- matrix(nrow = npt, ncol = 2L)
           for (i in seq_len(npt)) {
             push(x[i])
             d_2 <- d * d
             s4 <<- s4 +
               (1.0 - 3.0 / n / n / n) * d_2 * d_2 -
               4.0 * d * (s3 + d_2 * d) / n +
               6.0 * (s2 + d_2) * d_2 / n / n
             s3 <<- s3 +
               (1.0 - 2.0 / n / n) * d_2 * d -
               3.0 * d * (s2 + d_2) / n
             s2 <<- s2 +
               (1.0 - 1.0 / n) * d_2
             ans[i, 1L] <- m
             ans[i, 2L] <- s2 / n
             ans[i, 3L] <- s3 / n
           }
           ans
         },
         stop("Unsupported order ", order, call. = FALSE))
}

#' Moving covariance
#'
#' @param window moving window size
#'
#' @return an online function
#' @export
#'
moving_cov <- function(window) {

  w <- window
  n <- 0L
  bufx <- collections::deque()
  bufy <- collections::deque()

  sx <- sy <- sxy <- 0.0
  mx <- my <- 0.0

  dx   <- dy   <- 0.0
  d0x  <- d0y  <- 0.0

  push <- function(x, y) {
    if (n < w) {
      sx <<- sx + x
      sy <<- sy + y
      n  <<- n + 1L
    } else {
      oldx <- bufx$popleft()
      oldy <- bufy$popleft()
      sx  <<- sx + x - oldx
      sy  <<- sy + y - oldy
      d0x <<- oldx - mx
      d0y <<- oldy - my
    }
    bufx$push(x)
    bufy$push(y)
    dx  <<- x - mx
    dy  <<- y - my
    sxy <<- sxy + dx * dy - d0x * d0y - (dx - d0x) * (dy - d0y) / n
    mx  <<- sx / n
    my  <<- sy / n
  }

  function(x, y) {
    nx <- length(x)
    ny <- length(y)
    if (nx != ny) {
      stop("x and y have different lengths", call. = FALSE)
    }
    ans <- vector(mode = "numeric", length = nx)
    for (i in seq_len(nx)) {
      push(x[i], y[i])
      ans[i] <- sxy / (n - 1L)
    }
    ans
  }
}

#' Cumulative covariance
#'
#' @return an online function
#' @export
#'
cumulative_cov <- function() {

  n <- 0.0

  sx <- sy <- sxy <- 0.0
  mx <- my <- 0.0

  push <- function(x, y) {
    n <<- n + 1.0
    sxy <<- sxy + (1.0 - 1.0 / n) * (x - mx) * (y - my)
    sx  <<- sx + x
    sy  <<- sy + y
    mx  <<- sx / n
    my  <<- sy / n
  }

  function(x, y) {
    nx <- length(x)
    ny <- length(y)
    if (nx != ny) {
      stop("x and y have different lengths", call. = FALSE)
    }
    ans <- vector(mode = "numeric", length = nx)
    for (i in seq_len(nx)) {
      push(x[i], y[i])
      ans[i] <- sxy / (n - 1.0)
    }
    ans
  }
}
