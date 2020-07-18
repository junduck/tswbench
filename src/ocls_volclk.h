#pragma once

#include "ocls_common.h"

// ===== ocls_volclk_order =====
class ocls_volclk_order {

  bool init;
  double bin, lead, lead_tnvr;
  double open, high, low, close;

public:
  ocls_volclk_order(double bin_vol);

  Rcpp::NumericVector update_one(double price, double volume);
  Rcpp::NumericMatrix update(Rcpp::NumericVector price, Rcpp::NumericVector volume);
  Rcpp::NumericVector value();
};

// ===== ocls_volclk_tick =====
class ocls_volclk_tick {

  ocls_volclk_order volclk;
  double vol_last_tick;

public:
  ocls_volclk_tick(double bin_vol);

  Rcpp::NumericVector update_one(double price, double volume);
  Rcpp::NumericMatrix update(Rcpp::NumericVector price, Rcpp::NumericVector volume);
  Rcpp::NumericVector value();
};
