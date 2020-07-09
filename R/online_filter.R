#' Create an RLS (Recursive Least Squares) filter object
#'
#' @param width width of the filter
#' @param lambda forgetting factor
#' @param sigma initial factor for covariance matrix
#'
#' @return an RLS filter object
#' @export
#'
new_filter_RLS <- function(width, lambda = 0.99, sigma = 1.0) {

  width <- as.integer(width)
  stopifnot(width >= 2L, lambda > 0.0 && lambda <= 1.0)

  return(new(RLS, width, lambda, sigma))
}

#' Create an NLMS (Normalized Least Mean Squares) filter object
#'
#' @param width width of the filter
#' @param mu learning factor
#' @param eps a positive epsilon to make small norm numeric stable
#'
#' @return an NLMS filter object
#' @export
#'
new_filter_NLMS <- function(width, mu = 0.1, eps = 1.0) {

  width = as.integer(width)
  stopifnot(width >= 2.0, mu > 0.0, eps > 0.0)

  return(new(NLMS, width, mu, eps))
}

#' Create a GNGD (Generalized Normalized Gradient Descent) filter object
#'
#' @param width width of the filter
#' @param mu learning factor
#' @param eps a positive epsilon to make small norm numeric stable
#' @param rho gradient descend factor for adaptive eps
#'
#' @return a GNGD filter object
#' @export
#'
new_filter_GNGD <- function(width, mu = 0.1, eps = 1.0, rho = 0.1) {

  width <- as.integer(width)
  stopifnot(width >= 2.0, mu > 0.0, eps > 0.0, rho > 0.0)

  return(new(GNGD(width, mu, eps, rho)))
}

#' Create an online RLS filter for polynomial model
#'
#' @param order order of the polynomial
#' @param lambda forgetting factor
#' @param sigma initial factor for covariance matrix
#'
#' @return a stateful online function
#' @export
#'
make_filter_rls_poly <- function(order, lambda = 0.99, sigma = 1.0) {

  order <- as.integer(order, 0.0 < lambda && lambda <= 1.0)
  stopifnot(order >= 2L)

  calc <- new(ocls_filter_rls_poly, order, lambda, sigma)
  function(x) {
    calc$update(x)
  }
}

#' Create an online RLS filter for linear combination model
#'
#' @param width width of the filter
#' @param lambda forgetting factor
#' @param sigma initial factor for covariance matrix
#'
#' @return a stateful online function
#' @export
#'
make_filter_rls_linear <- function(width, lambda = 0.99, sigma = 1.0) {

  width <- as.integer(width)
  stopifnot(width >= 2L)

  calc <- new(ocls_filter_rls_linear, width, lambda, sigma)
  function(x) {
    calc$update(x)
  }
}
