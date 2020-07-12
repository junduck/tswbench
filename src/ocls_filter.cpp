#include "RcppArmadillo.h"
using namespace Rcpp;

typedef std::deque<double> deque;

class RLS {

  arma::colvec g;
  arma::mat P;
  arma::colvec w;
  double lambda, ilambda;

public:

  RLS(int n, double lambda, double sigma)
    : lambda(lambda),
      ilambda(1.0 / lambda),
      g(n,    arma::fill::zeros),
      P(n, n, arma::fill::eye),
      w(n,    arma::fill::zeros) {
    P *= sigma;
  }

  const arma::colvec& get_filter() {
    return w;
  }

  void update(arma::colvec x, double d) {
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

  double filter(arma::colvec x) {
    return arma::dot(x, w);
  }
};

class NLMS {

  double mu, eps;
  arma::colvec w;

public:

  NLMS(int n, double mu, double eps)
    : mu(mu),
      eps(eps),
      w(n, arma::fill::zeros) {
  }

  const arma::colvec& get_filter() {
    return w;
  }

  void update(arma::colvec x, double d) {
    // priori error
    auto a = d - arma::dot(x, w);
    // update filter
    w += mu * a * x / (arma::dot(x, x) + eps);
  }

  double filter(arma::colvec x) {
    return arma::dot(x, w);
  }
};

// Mandic, D. P. (2004). A generalized normalized gradient descent algorithm. IEEE signal processing letters, 11(2), 115-118.
class GNGD {

  double eta, mu, eps, rho;
  arma::colvec w;

  double last_a, last_norm;
  arma::colvec last_x;

public:
  GNGD(int n, double mu, double eps, double rho)
    : mu(mu),
      eps(eps),
      rho(rho),
      w(n, arma::fill::zeros),
      last_a(0.0),
      last_norm(eps),
      last_x(n, arma::fill::zeros) {
  }

  const arma::colvec& get_filter() {
    return w;
  }

  void update(arma::colvec x, double d) {
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

  double filter(arma::colvec x) {
    return arma::dot(x, w);
  }
};

class ocls_filter_rls_poly {

  RLS rls;
  int w, n;

  deque buf, polybuf;
  arma::colvec last_x;

public:

  ocls_filter_rls_poly(int order, double lambda, double sigma)
    : rls(order + 1, lambda, sigma), w(order),
      n(0) {
    polybuf.push_front(1.0);
  }

  double update_one(double x) {

    buf.push_front(x);

    auto d = NA_REAL;
    if (n < w) {
      n += 1;
    } else {
      rls.update(last_x, x);
      buf.pop_back();
      polybuf.pop_back();
    }

    for (auto i = 0; i < n; ++i) {
      polybuf[i] *= buf[i];
    }
    // prepend zeroth order
    polybuf.push_front(1.0);

    last_x = arma::colvec(std::vector<double>(polybuf.begin(), polybuf.end()));

    if (n < w) {
      return NA_REAL;
    } else {
      return rls.filter(last_x);
    }
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  double value() {
    return rls.filter(last_x);
  }
};

class ocls_filter_rls_linear {

  RLS rls;
  int w, n;

  deque buf;
  arma::colvec last_x;

public:

  ocls_filter_rls_linear(int width, double lambda, double sigma)
    : rls(width, lambda, sigma),
      w(width),
      n(0){
  }

  double update_one(double x) {

    buf.push_front(x);

    auto d = NA_REAL;
    if (n < w) {
      n += 1;
    } else {
      rls.update(last_x, x);
      buf.pop_back();
    }

    last_x = arma::colvec(std::vector<double>(buf.begin(), buf.end()));

    if (n < w) {
      return NA_REAL;
    } else {
      return rls.filter(last_x);
    }
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  double value() {
    return rls.filter(last_x);
  }
};


RCPP_MODULE(ocls_filter){
  using namespace Rcpp;

  class_<RLS>("RLS")

    .constructor<int, double, double>()

    .method("get_filter", &RLS::get_filter, "get_filter")
    .method("update", &RLS::update, "update")
    .method("filter", &RLS::filter, "filter")
    ;

  class_<NLMS>("NLMS")

    .constructor<int, double, double>()

    .method("get_filter", &NLMS::get_filter, "get_filter")
    .method("update", &NLMS::update, "update")
    .method("filter", &NLMS::filter, "filter")
    ;

  class_<GNGD>("GNGD")

    .constructor<int, double, double, double>()

    .method("get_filter", &GNGD::get_filter, "get_filter")
    .method("update", &GNGD::update, "update")
    .method("filter", &GNGD::filter, "filter")
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
