#include "ocls_volindi.h"

using namespace Rcpp;

// ===== ocls_obv =====
ocls_obv::ocls_obv()
  : init(false),
    obv(0.0),
    cls(0.0){
}

double ocls_obv::update_one(double close, double volume) {
  if (init) {
    if (cls == close) {
      obv = 0.0;
    } else if (cls > close) {
      obv += volume;
    } else {
      obv -= volume;
    }
  } else {
    init = true;
  }
  cls = close;
  return obv;
}

NumericVector ocls_obv::update(NumericVector close, NumericVector volume) {
  auto npt = close.length();
  auto y = NumericVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(close[i], volume[i]);
  }
  return y;
}

double ocls_obv::value() {
  return obv;
}

// ===== ocls_pnvi =====
ocls_pnvi::ocls_pnvi(double base_index)
  : init(false),
    pvi(base_index),
    nvi(base_index),
    cls(0.0),
    vol(0.0) {
}

NumericVector ocls_pnvi::update_one(double close, double volume) {
  if (init) {
    if (volume > vol) {
      pvi += pvi * (close / cls - 1.0);
    } else if (volume < vol) {
      nvi += nvi * (close / cls - 1.0);
    }
  } else {
    init = true;
  }
  cls = close;
  vol = volume;
  return NumericVector{pvi, nvi};
}

NumericMatrix ocls_pnvi::update(NumericVector close, NumericVector volume) {
  auto npt = close.length();
  auto ans = NumericMatrix(npt, 2);
  for (auto i = 0; i < npt; ++i) {
    ans(i, _) = update_one(close[i], volume[i]);
  }
  return ans;
}

NumericVector ocls_pnvi::value() {
  return NumericVector{pvi, nvi};
}

// ===== ocls_mfi =====
ocls_mfi::ocls_mfi(int period)
  : w(period),
    n(0) {
  // small eps to avoid NaN
  up = down = 0.000001;
}

double ocls_mfi::update_one(double price, double volume) {
  double tnvr = price * volume;
  double new_up, new_down;
  if (!n) {
    last_price = price;
  }
  new_up = new_down = 0.0;
  if (price != last_price) {
    if (price > last_price) {
      new_up = tnvr;
    } else {
      new_down = tnvr;
    }
  }
  last_price = price;
  buf_up.push_front(new_up);
  buf_down.push_front(new_down);
  if (n < w) {
    // cumulative stage
    up += new_up;
    down += new_down;
    n += 1;
  } else {
    // windowed stage
    up += new_up - buf_up.back();
    buf_up.pop_back();
    down += new_down - buf_down.back();
    buf_down.pop_back();
  }
  return value();
}

NumericVector ocls_mfi::update(NumericVector price, NumericVector volume) {
  auto npt = price.length();
  auto y = NumericVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(price[i], volume[i]);
  }
  return y;
}

double ocls_mfi::value() {
  return 100.0 * (1.0 - 1.0 / (1.0 + up / down));
}

RCPP_MODULE(ocls_volindi){
  using namespace Rcpp;

  class_<ocls_obv>("ocls_obv")
    .constructor()
    .method("update_one", &ocls_obv::update_one, "Update state by one value")
    .method("update", &ocls_obv::update, "Update state")
    .method("value", &ocls_obv::value, "Get last value")
    ;

  class_<ocls_pnvi>("ocls_pnvi")
    .constructor<double>()
    .method("update_one", &ocls_pnvi::update_one, "Update state by one value")
    .method("update", &ocls_pnvi::update, "Update state")
    .method("value", &ocls_pnvi::value, "Get last value")
    ;

  class_<ocls_mfi>("ocls_mfi")
    .constructor<int>()
    .method("update_one", &ocls_mfi::update_one, "Update state by one value")
    .method("update", &ocls_mfi::update, "Update state")
    .method("value", &ocls_mfi::value, "Get last value")
    ;
}
