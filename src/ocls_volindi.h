#pragma once

#include "ocls_common.h"

// ===== ocls_obv =====
class ocls_obv {

  bool init;
  double obv, cls;

public:
  ocls_obv();

  double update_one(double close, double volume);
  Rcpp::NumericVector update(Rcpp::NumericVector close, Rcpp::NumericVector volume);
  double value();
};

// ===== ocls_pnvi =====
class ocls_pnvi {

  bool init;
  double nvi, pvi, cls, vol;

public:
  ocls_pnvi(double base_index = 1000.0);

  Rcpp::NumericVector update_one(double close, double volume);
  Rcpp::NumericMatrix update(Rcpp::NumericVector close, Rcpp::NumericVector volume);
  Rcpp::NumericVector value();
};

// ===== ocls_mfi =====
class ocls_mfi {

  double up, down, last_price;
  deque buf_up, buf_down;
  int w, n;

public:
  ocls_mfi(int period);

  double update_one(double close, double volume);
  Rcpp::NumericVector update(Rcpp::NumericVector close, Rcpp::NumericVector volume);
  double value();
};
