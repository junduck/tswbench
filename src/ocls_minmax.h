#pragma once

#include "ocls_common.h"

// ===== ocls_moving_min =====
class ocls_moving_min {

  int w, n;
  deque buf, deq;

  void enQ(double x) ;
  void deQ(double x);

public:
  ocls_moving_min(int window);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_moving_max =====
class ocls_moving_max {

  int w, n;
  deque buf, deq;

  void enQ(double x) ;
  void deQ(double x);

public:
  ocls_moving_max(int window);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_moving_argmin =====
class ocls_moving_argmin {

  int w, n;
  deque buf, deq;
  // TODO: investigate better algorithm
  ideque argdeq;

  void enQ(double x) ;
  void deQ(double x);

public:
  ocls_moving_argmin(int window);

  int update_one(double x);
  Rcpp::IntegerVector update(Rcpp::NumericVector x);
  int value();
};

// ===== ocls_moving_argmax =====
class ocls_moving_argmax {

  int w, n;
  deque buf, deq;
  ideque argdeq;

  void enQ(double x) ;
  void deQ(double x);

public:
  ocls_moving_argmax(int window);

  int update_one(double x);
  Rcpp::IntegerVector update(Rcpp::NumericVector x);
  int value();
};
