#pragma once

#include "ocls_common.h"

// ===== filter_RLS =====
class filter_RLS {

  arma::colvec g;
  arma::mat P;
  arma::colvec w;
  double lambda, ilambda;

public:
  filter_RLS(int n, double lambda, double sigma);

  const arma::colvec& get_filter();
  void update(arma::colvec x, double d);
  double filter(arma::colvec x);
};

// ===== filter_NLMS =====
class filter_NLMS {

  double mu, eps;
  arma::colvec w;

public:
  filter_NLMS(int n, double mu, double eps);

  const arma::colvec& get_filter();
  void update(arma::colvec x, double d);
  double filter(arma::colvec x);
};

// ===== filter_GNGD =====
class filter_GNGD {

  double eta, mu, eps, rho;
  arma::colvec w;
  double last_a, last_norm;
  arma::colvec last_x;

public:
  filter_GNGD(int n, double mu, double eps, double rho);

  const arma::colvec& get_filter();
  void update(arma::colvec x, double d);
  double filter(arma::colvec x);
};

// ===== ocls_filter_rls_poly =====
class ocls_filter_rls_poly {

  filter_RLS rls;
  int w, n;
  deque buf, polybuf;
  arma::colvec last_x;

public:
  ocls_filter_rls_poly(int order, double lambda, double sigma);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_filter_rls_linear =====
class ocls_filter_rls_linear {

  filter_RLS rls;
  int w, n;
  deque buf;
  arma::colvec last_x;

public:
  ocls_filter_rls_linear(int width, double lambda, double sigma);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};
