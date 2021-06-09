#include "ocls_lag.h"

using namespace Rcpp;

// ===== ocls_lag =====
ocls_lag::ocls_lag(int lag, double na_fill)
  : w(lag),
    n(0),
    na_fill(na_fill) {
}

double ocls_lag::update_one(double x) {
  double y;
  buf.push_front(x);
  if (n < w) {
    n += 1;
    y = na_fill;
  } else {
    y = buf.back();
    buf.pop_back();
  }
  return y;
}

NumericVector ocls_lag::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_lag::value() {
  return buf.back();
}

// ===== ocls_lag_delta =====
ocls_lag_delta::ocls_lag_delta(int lag, double na_fill)
  : w(lag),
    n(0),
    na_fill(na_fill) {
}

double ocls_lag_delta::update_one(double x) {
  double y;
  buf.push_front(x);
  if (n < w) {
    n += 1;
    y = na_fill;
  } else {
    y = x - buf.back();
    buf.pop_back();
  }
  return y;
}

NumericVector ocls_lag_delta::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_lag_delta::value() {
  return buf.front() - buf.back();
}

// ===== ocls_lag_ratio =====
ocls_lag_ratio::ocls_lag_ratio(int lag, double na_fill)
  : w(lag),
    n(0),
    na_fill(na_fill) {
}

double ocls_lag_ratio::update_one(double x) {
  double y;
  buf.push_front(x);
  if (n < w) {
    n += 1;
    y = na_fill;
  } else {
    y = x / buf.back();
    buf.pop_back();
  }
  return y;
}

NumericVector ocls_lag_ratio::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_lag_ratio::value() {
  return buf.front() / buf.back();
}

// ===== ocls_lag_delta_moving_sum =====
ocls_lag_delta_moving_sum::ocls_lag_delta_moving_sum(int window, int lag)
  : w(window),
    l(lag),
    nx(0),
    nd(0),
    s(0.0){
}

double ocls_lag_delta_moving_sum::update_one(double x) {
  double d = 0.0;
  // lagged delta
  buf_x.push_front(x);
  if (nx < l) {
    nx += 1;
  } else {
    d = fabs(x - buf_x.back());
    buf_x.pop_back();
  }
  // moving sum
  buf_d.push_front(d);
  if (nd < w) {
    // cumulative stage
    nd += 1;
    s  += d;
  } else {
    // windowed stage
    s += d - buf_d.back();
    buf_d.pop_back();
  }
  return s;
}
// ocls_moving_sum_lag_delta
NumericVector ocls_lag_delta_moving_sum::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_lag_delta_moving_sum::value() {
  return s;
}

RCPP_MODULE(ocls_lag){
  using namespace Rcpp;

  class_<ocls_lag>("ocls_lag")
    .constructor<int, double>()
    .method("update_one", &ocls_lag::update_one, "Update state by one value")
    .method("update", &ocls_lag::update, "Update state")
    .method("value", &ocls_lag::value, "Get last value")
    ;

  class_<ocls_lag_delta>("ocls_lag_delta")
    .constructor<int, double>()
    .method("update_one", &ocls_lag_delta::update_one, "Update state by one value")
    .method("update", &ocls_lag_delta::update, "Update state")
    .method("value", &ocls_lag_delta::value, "Get last value")
    ;

  class_<ocls_lag_ratio>("ocls_lag_ratio")
    .constructor<int, double>()
    .method("update_one", &ocls_lag_ratio::update_one, "Update state by one value")
    .method("update", &ocls_lag_ratio::update, "Update state")
    .method("value", &ocls_lag_ratio::value, "Get last value")
    ;

  class_<ocls_lag_delta_moving_sum>("ocls_lag_delta_moving_sum")
    .constructor<int, int>()
    .method("update_one", &ocls_lag_delta_moving_sum::update_one, "Update state by one value")
    .method("update", &ocls_lag_delta_moving_sum::update, "Update state")
    .method("value", &ocls_lag_delta_moving_sum::value, "Get last value")
    ;
}
