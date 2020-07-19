#include "ocls_filter.h"

using namespace Rcpp;

// ===== filter_RLS =====
filter_RLS::filter_RLS(int n, double lambda, double sigma)
  : lambda(lambda),
    ilambda(1.0 / lambda),
    g(n,    arma::fill::zeros),
    P(n, n, arma::fill::eye),
    w(n,    arma::fill::zeros) {
  P *= sigma;
}

const arma::colvec& filter_RLS::get_filter() {
  return w;
}

void filter_RLS::update(arma::colvec x, double d) {
  // priori error
  auto a = d - arma::dot(x, w);
  // gain
  auto Px = P * x;
  g = Px / (lambda + arma::dot(x, Px));
  // cov matrix
  P = ilambda * (1 - arma::dot(g, x)) * P;
  // update filter
  w += a * g;
}

double filter_RLS::filter(arma::colvec x) {
  return arma::dot(x, w);
}

// ===== filter_NLMS =====
filter_NLMS::filter_NLMS(int n, double mu, double eps)
  : mu(mu),
    eps(eps),
    w(n, arma::fill::zeros) {
}

const arma::colvec& filter_NLMS::get_filter() {
  return w;
}

void filter_NLMS::update(arma::colvec x, double d) {
  // priori error
  auto a = d - arma::dot(x, w);
  // update filter
  w += mu * a * x / (arma::dot(x, x) + eps);
}

double filter_NLMS::filter(arma::colvec x) {
  return arma::dot(x, w);
}

// ===== filter_GNGD =====
// Mandic, D. P. (2004). A generalized normalized gradient descent algorithm. IEEE signal processing letters, 11(2), 115-118.
filter_GNGD::filter_GNGD(int n, double mu, double eps, double rho)
  : mu(mu),
    eps(eps),
    rho(rho),
    w(n, arma::fill::zeros),
    last_a(0.0),
    last_norm(eps),
    last_x(n, arma::fill::zeros) {
}

const arma::colvec& filter_GNGD::get_filter() {
  return w;
}

void filter_GNGD::update(arma::colvec x, double d) {
  // priori error
  auto a = d - arma::dot(x, w);
  // NLMS
  w += eta * a * x;
  // learning factor
  auto norm = arma::dot(x, x) + eps;
  eta = mu / norm;
  // epsilon
  eps -= rho * mu * a * last_a * arma::dot(x, last_x) / (last_norm * last_norm);
  // memory
  last_a    = a;
  last_norm = norm;
  last_x    = x;
}

double filter_GNGD::filter(arma::colvec x) {
  return arma::dot(x, w);
}

// ===== ocls_filter_rls_poly =====
ocls_filter_rls_poly::ocls_filter_rls_poly(int order, double lambda, double sigma)
  : rls(order + 1, lambda, sigma),
    w(order),
    n(0) {
  polybuf.push_front(1.0);
}

double ocls_filter_rls_poly::update_one(double x) {
  buf.push_front(x);
  if (n < w) {
    // cumulative stage
    n += 1;
  } else {
    // windowed stage
    rls.update(last_x, x);
    buf.pop_back();
    polybuf.pop_back();
  }
  for (auto i = 0; i < n; ++i) {
    polybuf[i] *= buf[i];
  }
  // prepend zeroth order
  polybuf.push_front(1.0);
  if (n < w) {
    return NA_REAL;
  } else {
    last_x = arma::colvec(std::vector<double>(polybuf.begin(), polybuf.end()));
    return rls.filter(last_x);
  }
}

NumericVector ocls_filter_rls_poly::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_filter_rls_poly::value() {
  return rls.filter(last_x);
}

// ===== ocls_filter_rls_linear =====
ocls_filter_rls_linear::ocls_filter_rls_linear(int width, double lambda, double sigma)
  : rls(width + 1, lambda, sigma),
    w(width),
    n(0){
  buf.push_front(1.0);
}

double ocls_filter_rls_linear::update_one(double x) {
  buf[0] = x;
  buf.push_front(1.0);
  if (n < w) {
    // cumulative stage
    n += 1;
  } else {
    // windowed stage
    rls.update(last_x, x);
    buf.pop_back();
  }
  if (n < w) {
    return NA_REAL;
  } else {
    last_x = arma::colvec(std::vector<double>(buf.begin(), buf.end()));
    return rls.filter(last_x);
  }
}

NumericVector ocls_filter_rls_linear::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_filter_rls_linear::value() {
  return rls.filter(last_x);
}

RCPP_MODULE(ocls_filter) {
  using namespace Rcpp;

  class_<filter_RLS>("filter_RLS")

    .constructor<int, double, double>()

    .method("get_filter", &filter_RLS::get_filter, "get_filter")
    .method("update", &filter_RLS::update, "update")
    .method("filter", &filter_RLS::filter, "filter")
    ;

  class_<filter_NLMS>("filter_NLMS")

    .constructor<int, double, double>()

    .method("get_filter", &filter_NLMS::get_filter, "get_filter")
    .method("update", &filter_NLMS::update, "update")
    .method("filter", &filter_NLMS::filter, "filter")
    ;

  class_<filter_GNGD>("filter_GNGD")

    .constructor<int, double, double, double>()

    .method("get_filter", &filter_GNGD::get_filter, "get_filter")
    .method("update", &filter_GNGD::update, "update")
    .method("filter", &filter_GNGD::filter, "filter")
    ;

  class_<ocls_filter_rls_poly>("ocls_filter_rls_poly")
    .constructor<int, double, double>()
    .method("update_one", &ocls_filter_rls_poly::update_one, "Update state by one value")
    .method("update", &ocls_filter_rls_poly::update, "Update state")
    .method("value", &ocls_filter_rls_poly::value, "Get last value")
  ;

  class_<ocls_filter_rls_linear>("ocls_filter_rls_linear")
    .constructor<int, double, double>()
    .method("update_one", &ocls_filter_rls_linear::update_one, "Update state by one value")
    .method("update", &ocls_filter_rls_linear::update, "Update state")
    .method("value", &ocls_filter_rls_linear::value, "Get last value")
  ;
}
