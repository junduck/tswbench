#pragma once

#include "ocls_common.h"

// ===== ocls_moving_mean =====
class ocls_moving_mean {

  int n, w;
  deque buf;
  double m;

public:
  ocls_moving_mean(int window);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_cumulative_mean =====
class ocls_cumulative_mean {

  double n, m;

public:
  ocls_cumulative_mean();

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_moving_mae =====
class ocls_moving_mae {

  double w, n;
  deque buf;
  double m, mae;

public:
  ocls_moving_mae(int window);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_moving_sd =====
class ocls_moving_sd {

  int n, w;
  deque buf;
  double s2, m, d, d0, dd0;

public:
  ocls_moving_sd(int window);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_cumulative_sd =====
class ocls_cumulative_sd {

  double n;
  double s2, m;
  double d;

public:
  ocls_cumulative_sd();

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_moving_volatility =====
class ocls_moving_volatility {

  int n, w;
  deque buf;
  double s2, m, d, d0, dd0;

public:
  ocls_moving_volatility(int window);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_cumulative_volatility =====
class ocls_cumulative_volatility {

  double n;
  double s2, m;
  double d;

public:
  ocls_cumulative_volatility();

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_moving_moment =====
class ocls_moving_moment {

  int o;
  int w, n, n2, n3;
  deque buf;
  double s2, s3, s4, m;
  double d, d0, dd0, d_2, d0_2, dd0_2;

public:
  ocls_moving_moment(int window, int order = 4);

  Rcpp::NumericVector update_one(double x);
  Rcpp::NumericMatrix update(Rcpp::NumericVector x);
  Rcpp::NumericVector value();
};

// ===== ocls_moving_stats =====
class ocls_moving_stats {

  int o, w, n;
  double adj_sd, adj_sk;
  ocls_moving_moment omm;

public:
  ocls_moving_stats(int window, int order = 4);

  Rcpp::NumericVector update_one(double x);
  Rcpp::NumericMatrix update(Rcpp::NumericVector x);
  Rcpp::NumericVector value();
};

// ===== ocls_cumulative_moment =====
class ocls_cumulative_moment {

  int o;
  double n;
  double s2, s3, s4, m;
  double d, d_2;

public:
  ocls_cumulative_moment(int order = 4);

  Rcpp::NumericVector update_one(double x);
  Rcpp::NumericMatrix update(Rcpp::NumericVector x);
  Rcpp::NumericVector value();
};

// ===== ocls_cumulative_stats =====
class ocls_cumulative_stats {

  int o;
  double n, adj_sd, adj_sk;
  ocls_cumulative_moment ocm;

public:
  ocls_cumulative_stats(int order = 4);

  Rcpp::NumericVector update_one(double x);
  Rcpp::NumericMatrix update(Rcpp::NumericVector x);
  Rcpp::NumericVector value();
};

// ===== ocls_moving_cov =====
class ocls_moving_cov {

  int w, n;
  deque bufx, bufy;
  double sxy, mx, my;
  double dx, dy, d0x, d0y;

public:
  ocls_moving_cov(int window);

  double update_one(double x, double y);
  Rcpp::NumericVector update(Rcpp::NumericVector x, Rcpp::NumericVector y);
  double value();
};

// ===== ocls_cumulative_cov =====
class ocls_cumulative_cov {

  double n;
  double sxy;
  double mx, my, dx, dy;

public:
  ocls_cumulative_cov();

  double update_one(double x, double y);
  Rcpp::NumericVector update(Rcpp::NumericVector x, Rcpp::NumericVector y);
  double value();
};

// ===== ocls_moving_zscore =====
class ocls_moving_zscore {

  int n, w;
  deque buf;
  double s2, m, sd, z, r;
  double newpt, oldpt, d, d0, dd0;
  double signal;

public:
  ocls_moving_zscore(int window, double zscore, double attenu);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};

// ===== ocls_cumulative_zscore =====
class ocls_cumulative_zscore {

  double n, s2, m, sd, z, r;
  double newpt, oldpt, d;
  double signal;

public:
  ocls_cumulative_zscore(double zscore, double attenu);

  double update_one(double x);
  Rcpp::NumericVector update(Rcpp::NumericVector x);
  double value();
};
