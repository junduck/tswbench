#movavg from pracma
#TODO: performance
ma <- function(x, n, type=c("s", "t", "w", "m", "e", "r")) {

  type <- match.arg(type)

  n  <- as.integer(n)
  nx <- length(x)
  y  <- numeric(nx)

  if (type == "s") {         # simple
    for (k in 1:(n - 1))
      y[k] <- mean(x[1:k])
    for (k in n:nx)
      y[k] <- mean(x[(k - n + 1):k])
  } else if (type == "t") {  # triangular
    n <- ceiling((n + 1)/2)
    s <- ma(x, n, "s")
    y <- ma(s, n, "s")
  } else if (type == "w") {  # weighted
    for (k in 1:(n - 1))
      y[k] <- 2 * sum((k:1) * x[k:1]) / (k * (k + 1))
    for (k in n:nx)
      y[k] <- 2 * sum((n:1) * x[k:(k - n + 1)]) / (n * (n + 1))
  } else if (type == "m") {  # modified
    y[1] <- x[1]
    for (k in 2:nx)
      y[k] <- y[k - 1] + (x[k] - y[k - 1]) / n
  } else if (type == "e") {  # exponential
    a <- 2 / (n + 1)
    y[1] <- x[1]
    for (k in 2:nx)
      y[k] <- a * x[k] + (1 - a) * y[k - 1]
  } else if (type == "r") {  # running
    a <- 1/n
    y[1] <- x[1]
    for (k in 2:nx)
      y[k] <- a * x[k] + (1 - a) * y[k - 1]
  }

  y
}

AddMA <- function(ohlc, column = "close", type = "s", periods = c(5, 20, 120)) {

  for (t in type) {
    for (p in periods) {
      col <- paste0(toupper(t), "MA", p)
      if (length(column) > 1) {
        col <- paste0(column, "_", col)
      }
      ohlc[, (col) := lapply(.SD, ma, n = p, type = t), .SDcols = column]
    }
  }

  ohlc
}
