#pragma once

#include "ocls_common.h"
#include "skiplist.h"

// ===== ocls_moving_sort =====
class ocls_moving_sort {

  int n, w;
  deque buf;
  IndexableSkiplist<double> skiplist;

public:
  ocls_moving_sort(int window);

  double get_index(int idx);
  Rcpp::NumericVector as_vector();
  void update_one(double x);
  void update(Rcpp::NumericVector x);
  Rcpp::NumericVector value();
};

// ===== ocls_moving_median =====
class ocls_moving_median {

  int n, w, idx1, idx2;
  deque buf;
  IndexableSkiplist<double> skiplist;

public:
  ocls_moving_median(int window);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_moving_quantile =====
class ocls_moving_quantile {

  int n, w, nidx;
  deque buf;
  IndexableSkiplist<double> skiplist;
  Rcpp::IntegerVector qidx;

public:
  ocls_moving_quantile(int window, Rcpp::IntegerVector idx);

  Rcpp::NumericVector update_one(double x);
  Rcpp::NumericMatrix update(Rcpp::NumericVector x);
  Rcpp::NumericVector value();
};
