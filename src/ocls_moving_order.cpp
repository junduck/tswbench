#include <Rcpp.h>
#include "skiplist.h"
using namespace Rcpp;

typedef std::deque<double> deque;

class ocls_moving_sort {

  int n, w;
  deque buf;
  IndexableSkiplist<double> skiplist;

public:

  ocls_moving_sort(int window): skiplist(window) {
    n = 0;
    w = window;
  }

  double get_index(int idx) {
    return skiplist[idx];
  }

  NumericVector as_vector() {
    return wrap(skiplist.as_vector());
  }

  void update_one(double x) {
    if (n < w) {
      n += 1;
    } else {
      skiplist.remove(buf.back());
      buf.pop_back();
    }
    buf.push_front(x);
    skiplist.insert(x);
    return;
  }

  void update(NumericVector x) {
    auto npt = x.length();
    for (auto i = 0; i < npt; ++i) {
      update_one(x[i]);
    }
    return;
  }

  NumericVector value() {
    return as_vector();
  }

};

class ocls_moving_median {

  int n, w, idx1, idx2;
  deque buf;
  IndexableSkiplist<double> skiplist;

public:

  ocls_moving_median(int window): skiplist(window) {
    w = window;
    n = idx1 = idx2 = 0;
  }

  double update_one(double x) {
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
    buf.push_front(x);
    skiplist.insert(x);

    return value();
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
    double y;
    if (idx1 == idx2) {
      y = skiplist[idx1];
    } else {
      y = skiplist[idx1] / 2.0 + skiplist[idx2] / 2.0;
    }
    return y;
  }
};

class ocls_moving_quantile {

  int n, w, nidx;
  deque buf;
  IndexableSkiplist<double> skiplist;
  IntegerVector qidx;

public:

  ocls_moving_quantile(int window, IntegerVector idx): skiplist(window) {
    w = window;
    n = 0;
    qidx = idx;
    nidx = qidx.length();
  }

  NumericVector update_one(double x) {
    buf.push_front(x);
    skiplist.insert(x);
    auto y = NumericVector(nidx);
    if (n < w - 1) {
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

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, nidx);
    for (auto i = 0; i < npt; ++i) {
      y(i, _) = update_one(x[i]);
    }
    return y;
  }

  NumericVector value() {
    auto y = NumericVector(nidx);
    if (n + 1 > w) {
      for (auto i = 0; i < nidx; ++i) {
        y[i] = skiplist[qidx[i]];
      }
    }
    return y;
  }

};

RCPP_MODULE(ocls_moving_order){
  using namespace Rcpp;

  class_<ocls_moving_sort>("ocls_moving_sort")

    .constructor<int>()

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
}
