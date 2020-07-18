#pragma once

#include "ocls_common.h"

// ===== ocls_ema =====
class ocls_ema {

  bool init;
  double alpha, ema;

public:
  ocls_ema(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_dema =====
class ocls_dema {

  bool init;
  double alpha, ema, ema2;

public:
  ocls_dema(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_tema =====
class ocls_tema {

  bool init;
  double alpha, ema, ema2, ema3;

public:
  ocls_tema(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_zlema =====
class ocls_zlema {

  int n, lag;
  double alpha, ema;
  deque buf;
  double datum;

public:
  ocls_zlema(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_wilders =====
class ocls_wilders {

  bool init;
  double alpha, wilders;

public:
  ocls_wilders(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_sma =====
class ocls_sma {

  int n, p;
  double m;
  deque buf;

public:
  ocls_sma(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_wma =====
class ocls_wma {

  int n, p, tot_w;
  //total weight, weighted sum, sum
  double ws, s;
  deque buf;

public:
  ocls_wma(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_hma =====
class ocls_hma {

  int p, m, s;
  ocls_wma wma_p, wma_m, wma_s;

public:
  ocls_hma(int period);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_kama =====
class ocls_kama {

  int n, p;
  double a_short, a_long, a_delta, a0, sdelta, kama;
  deque buf;
  double old, e, a;

public:
  ocls_kama(int period, int period_short = 2, int period_long = 30);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_vwma =====
class ocls_vwma {

  int n, p_;
  deque buft, bufv;
  // sum of turnover, volume
  double st, sv;
  double t;

public:
  ocls_vwma(int period);

  double update_one(double p, double v);
  Rcpp::NumericVector update(Rcpp::NumericVector p, Rcpp::NumericVector v);
  double value();
};

// ===== ocls_vidya =====
class ocls_vidya {

  bool init;
  int pshort, plong, ns, nl;
  double alpha, vidya;
  deque bufs, bufl;
  double s2s, ms, ds, d0s, dd0s,
  s2l, ml, dl, d0l, dd0l;
  double nr;

public:
  ocls_vidya(int period_short, int period_long, double alpha);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};
