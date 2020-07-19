#pragma once

#include "ocls_common.h"

// ===== ocls_lag =====
class ocls_lag {

  int w, n;
  deque buf;
  double na_fill;

public:
  ocls_lag(int lag, double na_fill);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_lag_delta =====
class ocls_lag_delta {

  int w, n;
  deque buf;
  double na_fill;

public:
  ocls_lag_delta(int lag, double na_fill);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_lag_ratio =====
class ocls_lag_ratio {

  int w, n;
  deque buf;
  double na_fill;

public:
  ocls_lag_ratio(int lag, double na_fill);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_lag_delta_moving_sum =====
class ocls_lag_delta_moving_sum {

  int w, l, nx, nd;
  deque buf_x, buf_d;
  double s;

public:
  ocls_lag_delta_moving_sum(int window, int lag);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};
