#include "ocls_order.h"

using namespace Rcpp;

// ===== ocls_moving_sort =====
ocls_moving_sort::ocls_moving_sort(int window)
  : skiplist(window),
    n(0),
    w(window) {
}

double ocls_moving_sort::get_index(int idx) {
  return skiplist[idx];
}

NumericVector ocls_moving_sort::to_vector() {
  return wrap(skiplist.to_vector());
}

void ocls_moving_sort::update_one(double x) {
  buf.push_front(x);
  skiplist.insert(x);
  if (n < w) {
    n += 1;
  } else {
    skiplist.remove(buf.back());
    buf.pop_back();
  }
}

void ocls_moving_sort::update(NumericVector x) {
  auto npt = x.length();
  for (decltype(npt) i = 0; i < npt; ++i) {
    update_one(x[i]);
  }
}

NumericVector ocls_moving_sort::value() {
  return to_vector();
}

// ===== ocls_moving_median =====
ocls_moving_median::ocls_moving_median(int window)
  : skiplist(window),
    w(window){
  n = idx1 = idx2 = 0;
}

double ocls_moving_median::update_one(double x) {
  buf.push_front(x);
  skiplist.insert(x);
  if (n < w) {
    // cumulative stage
    n += 1;
    if (n % 2) {
      idx1 = idx2 = n / 2;
    } else {
      idx1 = n / 2;
      idx2 = idx1 - 1;
    }
  } else {
    // windowed stage
    skiplist.remove(buf.back());
    buf.pop_back();
  }
  return value();
}

NumericVector ocls_moving_median::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_median::value() {
  double y;
  if (idx1 == idx2) {
    y = skiplist[idx1];
  } else {
    y = skiplist[idx1] / 2.0 + skiplist[idx2] / 2.0;
  }
  return y;
}

// ===== ocls_moving_quantile =====
ocls_moving_quantile::ocls_moving_quantile(int window, IntegerVector idx)
  : skiplist(window),
    w(window),
    n(0),
    qidx(idx),
    nidx(idx.length()) {
}

NumericVector ocls_moving_quantile::update_one(double x) {
  buf.push_front(x);
  skiplist.insert(x);
  auto y = NumericVector(nidx);
  if (n + 1 < w) {
    n += 1;
  } else {
    for (auto i = 0; i < nidx; ++i) {
      y[i] = skiplist[qidx[i]];
    }
    skiplist.remove(buf.back());
    buf.pop_back();
  }
  return y;
}

NumericMatrix ocls_moving_quantile::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericMatrix(npt, nidx);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y(i, _) = update_one(x[i]);
  }
  return y;
}

NumericVector ocls_moving_quantile::value() {
  auto y = NumericVector(nidx);
  if (n + 1 >= w) {
    for (auto i = 0; i < nidx; ++i) {
      y[i] = skiplist[qidx[i]];
    }
  }
  return y;
}

// ===== ocls_cumulative_psquare =====
ocls_cumulative_psquare::ocls_cumulative_psquare(NumericVector q)
  : nq(q.length()) {
  for (auto i = 0; i < nq; ++i) {
    psq.push_back(psquare(q[i]));
  }
}

NumericVector ocls_cumulative_psquare::update_one(double x) {
  for (auto i = 0; i < nq; ++i) {
    psq[i].insert(x);
  }
  return value();
}

NumericMatrix ocls_cumulative_psquare::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericMatrix(npt, nq);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y(i, _) = update_one(x[i]);
  }
  return y;
}

NumericVector ocls_cumulative_psquare::value() {
  auto y = NumericVector(nq);
  for (auto i = 0; i < nq; ++i) {
    y[i] = psq[i].value();
  }
  return y;
}

// ===== ocls_cumulative_quantile =====
ocls_cumulative_quantile::ocls_cumulative_quantile(int k, double c, bool lazy)
  : kll(k, c, lazy) {
}

NumericMatrix ocls_cumulative_quantile::update_one(double x) {
  kll.insert(x);
  return value();
}

NumericMatrix ocls_cumulative_quantile::update(NumericVector x) {
  for (auto x_elem : x) {
    kll.insert(x_elem);
  }
  return value();
}

NumericMatrix ocls_cumulative_quantile::value() {
  auto quantiles = kll.cdf();
  auto npts = quantiles.first.size();
  NumericMatrix ans(npts, 2);
  ans(_, 0) = as<NumericVector>(wrap(quantiles.first));
  ans(_, 1) = as<NumericVector>(wrap(quantiles.second));
  return ans;
}

void ocls_cumulative_quantile::insert(NumericVector x) {
  auto npt = x.length();
  for (decltype(npt) i = 0; i < npt; ++i) {
    kll.insert(x[i]);
  }
}

NumericVector ocls_cumulative_quantile::quantile(NumericVector probs) {

  auto ans = kll.quantile(as<std::vector<double>>(probs));
  return wrap(ans);
}

RCPP_MODULE(ocls_order){
  using namespace Rcpp;

  class_<ocls_moving_sort>("ocls_moving_sort")
    .constructor<int>()
    .method("get_index", &ocls_moving_sort::get_index, "Get element at index")
    .method("to_vector", &ocls_moving_sort::to_vector, "Get all elements")
    .method("update_one", &ocls_moving_sort::update_one, "Update state by one value")
    .method("update", &ocls_moving_sort::update, "Update state")
    .method("value", &ocls_moving_sort::value, "Get last value")
    ;

  class_<ocls_moving_median>("ocls_moving_median")
    .constructor<int>()
    .method("update_one", &ocls_moving_median::update_one, "Update state by one value")
    .method("update", &ocls_moving_median::update, "Update state")
    .method("value", &ocls_moving_median::value, "Get last value")
    ;

  class_<ocls_moving_quantile>("ocls_moving_quantile")
    .constructor<int, IntegerVector>()
    .method("update_one", &ocls_moving_quantile::update_one, "Update state by one value")
    .method("update", &ocls_moving_quantile::update, "Update state")
    .method("value", &ocls_moving_quantile::value, "Get last value")
    ;

  class_<ocls_cumulative_psquare>("ocls_cumulative_psquare")
    .constructor<NumericVector>()
    .method("update_one", &ocls_cumulative_psquare::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_psquare::update, "Update state")
    .method("value", &ocls_cumulative_psquare::value, "Get last value")
    ;

  class_<ocls_cumulative_quantile>("ocls_cumulative_quantile")
    .constructor<int, double, bool>()
    .method("update_one", &ocls_cumulative_quantile::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_quantile::update, "Update state")
    .method("value", &ocls_cumulative_quantile::value, "Get last value")
    .method("quantile", &ocls_cumulative_quantile::quantile, "Get quantile")
    ;
}
