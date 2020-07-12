#include <Rcpp.h>
using namespace Rcpp;

typedef std::deque<double> deque;

class ocls_lag {

  int w, n;
  deque buf;

public:

  ocls_lag(int lag)
    : w(lag),
      n(0) {
  }

  double update_one(double x) {
    double y;
    buf.push_front(x);
    if (n < w) {
      n += 1;
      y = NA_REAL;
    } else {
      y = buf.back();
      buf.pop_back();
    }
    return y;
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  double value() {
    return buf.back();
  }
};

class ocls_lag_delta {

  int w, n;
  deque buf;

public:

  ocls_lag_delta(int lag)
    : w(lag),
      n(0) {
  }

  double update_one(double x) {
    double y;
    buf.push_front(x);
    if (n < w) {
      n += 1;
      y = NA_REAL;
    } else {
      y = x - buf.back();
      buf.pop_back();
    }
    return y;
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  double value() {
    return buf.front() - buf.back();
  }

};

class ocls_lag_ratio {

  int w, n;
  deque buf;

public:

  ocls_lag_ratio(int lag)
    : w(lag),
      n(0) {
  }

  double update_one(double x) {
    double y;
    buf.push_front(x);
    if (n < w) {
      n += 1;
      y = NA_REAL;
    } else {
      y = x / buf.back();
      buf.pop_back();
    }
    return y;
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  double value() {
    return buf.front() / buf.back();
  }

};

RCPP_MODULE(ocls_utils){
  using namespace Rcpp;

  class_<ocls_lag>("ocls_lag")

    .constructor<int>()

    .method("update_one", &ocls_lag::update_one, "Update state by one value")
    .method("update", &ocls_lag::update, "Update state")
    .method("value", &ocls_lag::value, "Get last value")
    ;

  class_<ocls_lag_delta>("ocls_lag_delta")

    .constructor<int>()

    .method("update_one", &ocls_lag_delta::update_one, "Update state by one value")
    .method("update", &ocls_lag_delta::update, "Update state")
    .method("value", &ocls_lag_delta::value, "Get last value")
    ;

  class_<ocls_lag_ratio>("ocls_lag_ratio")

    .constructor<int>()

    .method("update_one", &ocls_lag_ratio::update_one, "Update state by one value")
    .method("update", &ocls_lag_ratio::update, "Update state")
    .method("value", &ocls_lag_ratio::value, "Get last value")
    ;
}
