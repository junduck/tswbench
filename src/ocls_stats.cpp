#include "ocls_stats.h"

using namespace Rcpp;

// ===== ocls_moving_mean =====
ocls_moving_mean::ocls_moving_mean(int window)
  : w(window),
    n(0),
    m(0.0) {
}

double ocls_moving_mean::update_one(double x) {
  buf.push_front(x);
  if (n < w) {
    // cumulative stage
    n += 1;
    m += (x - m) / n;
  } else {
    // windowed stage
    m += (x - buf.back()) / n;
    buf.pop_back();
  }
  return m;
}

NumericVector ocls_moving_mean::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_mean::value() {
  return m;
}

// ===== ocls_cumulative_mean =====
ocls_cumulative_mean::ocls_cumulative_mean() {
  n = m = 0.0;
}

double ocls_cumulative_mean::update_one(double x) {
  n += 1.0;
  m += (x - m) / n;
  return m;
}

NumericVector ocls_cumulative_mean::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_cumulative_mean::value() {
  return m;
}

// ===== ocls_moving_mae =====
ocls_moving_mae::ocls_moving_mae(int window)
  : w(window),
    n(0) {
  m = mae = 0.0;
}

double ocls_moving_mae::update_one(double x) {
  buf.push_front(x);
  if (n < w) {
    // cumulative stage
    n += 1;
    m += (x - m) / n;
  } else {
    // windowed stage
    m += (x - buf.back()) / n;
    buf.pop_back();
  }
  // O(n) seems to be the only way. Compiler opt may be possible.
  mae = 0.0;
  for (auto i = 0; i < n; ++i) {
    mae += fabs(buf[i] - m);
  }
  mae /= n;
  return mae;
}

NumericVector ocls_moving_mae::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_mae::value() {
  return mae;
}

// ===== ocls_moving_sd =====
ocls_moving_sd::ocls_moving_sd(int window)
  : w(window),
    n(0) {
  s2 = m = d = d0 = dd0 = 0.0;
}

double ocls_moving_sd::update_one(double x) {
  buf.push_front(x);
  d = x - m;
  if (n < w) {
    // cumulative stage
    n += 1;
    m += d / n;
  } else {
    // windowed stage
    auto old = buf.back();
    buf.pop_back();
    d0 = old - m;
    m += (x - old) / n;
  }
  dd0 = d - d0;
  s2 += d * d - d0 * d0 - dd0 * dd0 / n;
  return value();
}

NumericVector ocls_moving_sd::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_sd::value() {
  return sqrt(s2 / (n - 1));
}

// ===== ocls_cumulative_sd =====
ocls_cumulative_sd::ocls_cumulative_sd()
  : n(0.0) {
  s2 = m = d = 0.0;
}

double ocls_cumulative_sd::update_one(double x) {
  n += 1.0;
  d  = x - m;
  m += d / n;
  // TODO: investigate numeric stability
  s2 += (1.0 - 1.0 / n) * d * d;
  return value();
}

NumericVector ocls_cumulative_sd::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_cumulative_sd::value() {
  return sqrt(s2 / (n - 1.0));
}

// ===== ocls_moving_volatility =====
ocls_moving_volatility::ocls_moving_volatility(int window)
  : w(window),
    n(0) {
  s2 = m = d = d0 = dd0 = 0.0;
}

double ocls_moving_volatility::update_one(double x) {
  buf.push_front(x);
  d = x - m;
  if (n < w) {
    // cumulative stage
    n += 1;
    m += d / n;
  } else {
    // windowed stage
    auto old = buf.back();
    buf.pop_back();
    d0 = old - m;
    m += (x - old) / n;
  }
  dd0 = d - d0;
  s2 += d * d - d0 * d0 - dd0 * dd0 / n;
  return value();
}

NumericVector ocls_moving_volatility::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_volatility::value() {
  return sqrt(s2 / (n - 1)) / m;
}

// ===== ocls_cumulative_volatility =====
ocls_cumulative_volatility::ocls_cumulative_volatility()
  : n(0.0) {
  s2 = m = d = 0.0;
}

double ocls_cumulative_volatility::update_one(double x) {
  n += 1.0;
  d  = x - m;
  m += d / n;
  // TODO: investigate numeric stability
  s2 += (1.0 - 1.0 / n) * d * d;
  return value();
}

NumericVector ocls_cumulative_volatility::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  double d_2;
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_cumulative_volatility::value() {
  return sqrt(s2 / (n - 1.0)) / m;
}

// ===== ocls_moving_moment =====
ocls_moving_moment::ocls_moving_moment(int window, int order)
  : o(order),
    w(window) {
  n = n2 = n3 = 0;
  s2 = s3 = s4 = m = 0.0;
  d = d0 = dd0 = 0.0;
}

NumericVector ocls_moving_moment::update_one(double x) {
  buf.push_front(x);
  d = x - m;
  if (n < w) {
    // cumulative stage
    n += 1;
    m += d / n;
    n2 = n * n;
    n3 = n2 * n;
  } else {
    // windowed stage
    auto old = buf.back();
    buf.pop_back();
    d0 = old - m;
    m += (x - old) / n;
  }
  dd0   = d - d0;
  d_2   = d * d;
  d0_2  = d0 * d0;
  dd0_2 = dd0 * dd0;
  switch (o) {
  // TODO: investigate numeric stability
  case 4:
    s4 += d_2 * d_2 - d0_2 * d0_2
    - 4.0 * dd0 * (s3 + d_2 * d - d0_2 * d0) / n
    + 6.0 * (s2 + d_2 - d0_2) * dd0_2 / n2
    - 3.0 * dd0_2 * dd0_2 / n3;
  case 3:
    s3 += d_2 * d - d0_2 * d0
    - 3.0 * dd0 * (s2 + d_2 - d0_2) / n
    + 2.0 * dd0_2 * dd0 / n2;
  case 2:
    s2 += d_2 - d0_2 - dd0_2 / n;
  }
  return value();
}

NumericMatrix ocls_moving_moment::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericMatrix(npt, 4);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y(i, _) = update_one(x[i]);
  }
  return y;
}

NumericVector ocls_moving_moment::value() {
  return NumericVector{m, s2 / n, s3 / n, s4 / n};
}

// ===== ocls_moving_stats =====
ocls_moving_stats::ocls_moving_stats(int window, int order)
  : o(order),
    w(window),
    n(0),
    omm(window, order) {
  adj_sd = adj_sk  = 0.0;
}

NumericVector ocls_moving_stats::update_one(double x) {
  auto y = omm.update_one(x);
  if (n < w) {
    n += 1;
    adj_sd = sqrt(double(n) / (n - 1));
    // Adjusted Fisher-Pearson coefs of skewness, still BIASED
    adj_sk = sqrt(double(n * (n - 1))) / (n - 2);
  }
  switch (o) {
  case 4:
    y[3]  = y[3] / pow(y[1], 2.0) - 3.0;
  case 3:
    y[2]  = adj_sk * y[2] / pow(y[1], 1.5);
  case 2:
    y[1]  = adj_sd * sqrt(y[1]);
  }
  return y;
}

NumericMatrix ocls_moving_stats::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericMatrix(npt, 4);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y(i, _) = update_one(x[i]);
  }
  return y;
}

NumericVector ocls_moving_stats::value() {
  auto y = omm.value();
  switch (o) {
  case 4:
    y[3]  = y[3] / pow(y[2], 2.0) - 3.0;
  case 3:
    y[2]  = adj_sk * y[2] / pow(y[1], 1.5);
  case 2:
    y[1]  = adj_sd * sqrt(y[1]);
  }
  return y;
}

// ===== ocls_cumulative_moment =====
ocls_cumulative_moment::ocls_cumulative_moment(int order)
  : o(order),
    n(0.0) {
  s2 = s3 = s4 = m = d = 0.0;
}

NumericVector ocls_cumulative_moment::update_one(double x) {
  n += 1.0;
  d  = x - m;
  m += d / n;
  d_2 = d * d;
  switch (o) {
  // TODO: investigate numeric stability
  case 4:
    s4 += (1.0 - 3.0 / n / n / n) * d_2 * d_2
    - 4.0 * d * (s3 + d_2 * d) / n
    + 6.0 * (s2 + d_2) * d_2 / n / n;
  case 3:
    s3 += (1.0 + 2.0 / n / n) * d_2 * d
    - 3.0 * d * (s2 + d_2) / n;
  case 2:
    s2 += (1.0 - 1.0 / n) * d_2;
  }
  return value();
}

NumericMatrix ocls_cumulative_moment::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericMatrix(npt, 4);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y(i, _) = update_one(x[i]);
  }
  return y;
}

NumericVector ocls_cumulative_moment::value() {
  return NumericVector{m, s2 / n, s3 / n, s4 / n};
}

// ===== ocls_cumulative_stats =====
ocls_cumulative_stats::ocls_cumulative_stats(int order)
  : o(order),
    n(0.0),
    ocm(order) {
  adj_sd = adj_sk  = 0.0;
}

NumericVector ocls_cumulative_stats::update_one(double x) {
  auto y = ocm.update_one(x);
  n += 1.0;
  adj_sd = sqrt(n / (n - 1.0));
  adj_sk = sqrt(n * (n - 1.0)) / (n - 2.0);
  switch (o) {
  case 4:
    y[3]  = y[3] / pow(y[2], 2.0) - 3.0;
  case 3:
    y[2]  = adj_sk * y[2] / pow(y[1], 1.5);
  case 2:
    y[1]  = adj_sd * sqrt(y[1]);
  }
  return y;
}

NumericMatrix ocls_cumulative_stats::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericMatrix(npt, 4);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y(i, _) = update_one(x[i]);
  }
  return y;
}

NumericVector ocls_cumulative_stats::value() {
  auto y = ocm.value();
  switch (o) {
  case 4:
    y[3]  = y[3] / pow(y[2], 2.0) - 3.0;
  case 3:
    y[2]  = adj_sk * y[2] / pow(y[1], 1.5);
  case 2:
    y[1]  = adj_sd * sqrt(y[1]);
  }
  return y;
}

// ===== ocls_moving_cov =====
ocls_moving_cov::ocls_moving_cov(int window)
  : w(window),
    n(0) {
  sxy = mx = my = 0.0;
  dx = dy = d0x = d0y = 0.0;
}

double ocls_moving_cov::update_one(double x, double y) {
  bufx.push_front(x);
  bufy.push_front(y);
  dx = x - mx;
  dy = y - my;
  if (n < w) {
    // cumulative stage
    n += 1;
    mx += dx / n;
    my += dy / n;
  } else {
    auto oldx = bufx.back();
    auto oldy = bufy.back();
    bufx.pop_back();
    bufy.pop_back();
    d0x = oldx - mx;
    d0y = oldy - my;
    mx += (x - oldx) / n;
    my += (y - oldy) / n;
  }
  sxy += dx * dy - d0x * d0y - (dx - d0x) * (dy - d0y) / n;
  return value();
}

NumericVector ocls_moving_cov::update(NumericVector x, NumericVector y) {
  auto npt = x.length();
  auto z = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    z[i] = update_one(x[i], y[i]);
  }
  return z;
}

double ocls_moving_cov::value() {
  return sxy / (n - 1);
}

// ===== ocls_cumulative_cov =====
ocls_cumulative_cov::ocls_cumulative_cov()
  : n(0.0) {
  sxy = mx = my = dx = dy = 0.0;
}

double ocls_cumulative_cov::update_one(double x, double y) {
  dx   = x - mx;
  dy   = y - my;
  n   += 1.0;
  mx  += dx / n;
  my  += dy / n;
  sxy += (1.0 - 1.0 / n) * dx * dy;
  return value();
}

NumericVector ocls_cumulative_cov::update(NumericVector x, NumericVector y) {
  auto npt = x.length();
  auto ans = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    ans[i] = update_one(x[i], y[i]);
  }
  return ans;
}

double ocls_cumulative_cov::value() {
  return sxy / (n - 1.0);
}

// ===== ocls_moving_zscore =====
ocls_moving_zscore::ocls_moving_zscore(int window, double zscore, double attenu)
  : w(window),
    n(0),
    z(zscore),
    r(attenu) {
  s2 = m = sd = 0.0;
  d = d0 = dd0 = 0.0;
  signal = 0.0;
}

double ocls_moving_zscore::update_one(double x) {
  if (n < w) {
    // cumulative stage
    newpt = x;
    n += 1;
    d  = newpt - m;
    m += d / n;
  } else {
    // windowed stage
    if (fabs(x - m) > z * sd) {
      signal = (x - m) / sd;
      // apply attenuation
      newpt = r * x + (1.0 - r) * buf.front();
    } else {
      signal = 0.0;
      newpt = x;
    }
    oldpt = buf.back();
    buf.pop_back();
    d  = newpt - m;
    d0 = oldpt - m;
    m += (newpt - oldpt) / n;
  }
  buf.push_front(newpt);
  // update stats
  dd0 = d - d0;
  s2 += d * d - d0 * d0 - dd0 * dd0 / n;
  sd  = sqrt(s2 / (n - 1));
  return signal;
}

NumericVector ocls_moving_zscore::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_zscore::value() {
  return signal;
}

// ===== ocls_cumulative_zscore =====
ocls_cumulative_zscore::ocls_cumulative_zscore(double zscore, double attenu)
  : n(0.0),
    z(zscore),
    r(attenu) {
  s2 = m = 0.0;
  newpt = oldpt = 0.0;
}

double ocls_cumulative_zscore::update_one(double x) {
  if (n > 2.0 && fabs(x - m) > z * sd) {
    signal = (x - m) / sd;
    newpt  = r * x + (1.0 - r) * oldpt;
  } else {
    signal = 0.0;
    newpt  = x;
  }
  n  += 1.0;
  d   = newpt - m;
  m  += d / n;
  s2 += (1.0 - 1.0 / n) * d * d;
  sd  = sqrt(s2 / (n - 0.1));
  oldpt = newpt;
  return signal;
}

NumericVector ocls_cumulative_zscore::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_cumulative_zscore::value() {
  return signal;
}

RCPP_MODULE(ocls_stats) {
  using namespace Rcpp;

  class_<ocls_moving_mean>("ocls_moving_mean")
    .constructor<int>()
    .method("update_one", &ocls_moving_mean::update_one, "Update state by one value")
    .method("update", &ocls_moving_mean::update, "Update state")
    .method("value", &ocls_moving_mean::value, "Get last value")
  ;

  class_<ocls_cumulative_mean>("ocls_cumulative_mean")
    .constructor()
    .method("update_one", &ocls_cumulative_mean::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_mean::update, "Update state")
    .method("value", &ocls_cumulative_mean::value, "Get last value")
  ;

  class_<ocls_moving_mae>("ocls_moving_mae")
    .constructor<int>()
    .method("update_one", &ocls_moving_mae::update_one, "Update state by one value")
    .method("update", &ocls_moving_mae::update, "Update state")
    .method("value", &ocls_moving_mae::value, "Get last value")
  ;

  class_<ocls_moving_sd>("ocls_moving_sd")
    .constructor<int>()
    .method("update_one", &ocls_moving_sd::update_one, "Update state by one value")
    .method("update", &ocls_moving_sd::update, "Update state")
    .method("value", &ocls_moving_sd::value, "Get last value")
  ;

  class_<ocls_cumulative_sd>("ocls_cumulative_sd")
    .constructor()
    .method("update_one", &ocls_cumulative_sd::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_sd::update, "Update state")
    .method("value", &ocls_cumulative_sd::value, "Get last value")
  ;

  class_<ocls_moving_volatility>("ocls_moving_volatility")
    .constructor<int>()
    .method("update_one", &ocls_moving_volatility::update_one, "Update state by one value")
    .method("update", &ocls_moving_volatility::update, "Update state")
    .method("value", &ocls_moving_volatility::value, "Get last value")
  ;

  class_<ocls_cumulative_volatility>("ocls_cumulative_volatility")
    .constructor()
    .method("update_one", &ocls_cumulative_volatility::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_volatility::update, "Update state")
    .method("value", &ocls_cumulative_volatility::value, "Get last value")
  ;

  class_<ocls_moving_moment>("ocls_moving_moment")
    .constructor<int, int>()
    .method("update_one", &ocls_moving_moment::update_one, "Update state by one value")
    .method("update", &ocls_moving_moment::update, "Update state")
    .method("value", &ocls_moving_moment::value, "Get last value")
  ;

  class_<ocls_moving_stats>("ocls_moving_stats")
    .constructor<int, int>()
    .method("update_one", &ocls_moving_stats::update_one, "Update state by one value")
    .method("update", &ocls_moving_stats::update, "Update state")
    .method("value", &ocls_moving_stats::value, "Get last value")
  ;

  class_<ocls_cumulative_moment>("ocls_cumulative_moment")
    .constructor<int>()
    .method("update_one", &ocls_cumulative_moment::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_moment::update, "Update state")
    .method("value", &ocls_cumulative_moment::value, "Get last value")
  ;

  class_<ocls_cumulative_stats>("ocls_cumulative_stats")
    .constructor<int>()
    .method("update_one", &ocls_cumulative_stats::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_stats::update, "Update state")
    .method("value", &ocls_cumulative_stats::value, "Get last value")
  ;

  class_<ocls_moving_cov>("ocls_moving_cov")
    .constructor<int>()
    .method("update_one", &ocls_moving_cov::update_one, "Update state by one value")
    .method("update", &ocls_moving_cov::update, "Update state")
    .method("value", &ocls_moving_cov::value, "Get last value")
  ;

  class_<ocls_cumulative_cov>("ocls_cumulative_cov")
    .constructor()
    .method("update_one", &ocls_cumulative_cov::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_cov::update, "Update state")
    .method("value", &ocls_cumulative_cov::value, "Get last value")
  ;

  class_<ocls_moving_zscore>("ocls_moving_zscore")
    .constructor<int, double, double>()
    .method("update_one", &ocls_moving_zscore::update_one, "Update state by one value")
    .method("update", &ocls_moving_zscore::update, "Update state")
    .method("value", &ocls_moving_zscore::value, "Get last value")
  ;

  class_<ocls_cumulative_zscore>("ocls_cumulative_zscore")
    .constructor<double, double>()
    .method("update_one", &ocls_cumulative_zscore::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_zscore::update, "Update state")
    .method("value", &ocls_cumulative_zscore::value, "Get last value")
  ;
}
