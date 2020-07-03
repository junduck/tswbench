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

  NumericVector as_vector() {
    return wrap(skiplist.as_vector());
  }

  double get_index(int idx) {
    return skiplist[idx];
  }

  void insert(NumericVector x) {
    auto npt = x.length();
    for (auto i = 0; i < npt; ++i) {
      if (n < w) {
        n += 1;
      } else {
        skiplist.remove(buf.back());
        buf.pop_back();
      }
      buf.push_front(x[i]);
      skiplist.insert(x[i]);
    }
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

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
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
      buf.push_front(x[i]);
      skiplist.insert(x[i]);
      if (idx1 == idx2) {
        y[i] = skiplist[idx1];
      } else {
        y[i] = skiplist[idx1] / 2.0 + skiplist[idx2] / 2.0;
      }

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

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, nidx);
    for (auto i = 0; i < npt; ++i) {
      buf.push_front(x[i]);
      skiplist.insert(x[i]);
      if (n < w - 1) {
        n += 1;
      } else {
        for (auto j = 0; j < nidx; ++j) {
          y(i, j) = skiplist[qidx[j]];
        }
        skiplist.remove(buf.back());
        buf.pop_back();
      }
    }
    return y;
  }

  NumericVector value() {
    auto y = NumericVector(nidx);
    for (auto i = 0; i < nidx; ++i) {
      y[i] = skiplist[qidx[i]];
    }
    return y;
  }

};

RCPP_MODULE(ocls_moving_order){
  using namespace Rcpp;

  class_<ocls_moving_sort>("ocls_moving_sort")

    .constructor<int>()

    .method("insert", &ocls_moving_sort::insert, "Insert values")
    .method("get_index", &ocls_moving_sort::get_index, "Get value at index")
    .method("as_vector", &ocls_moving_sort::as_vector, "Convert to vector")
    ;

  class_<ocls_moving_median>("ocls_moving_median")

    .constructor<int>()

    .method("update", &ocls_moving_median::update, "Update state")
    .method("value", &ocls_moving_median::value, "Get last value")
    ;

  class_<ocls_moving_quantile>("ocls_moving_quantile")

    .constructor<int, IntegerVector>()

    .method("update", &ocls_moving_quantile::update, "Update state")
    .method("value", &ocls_moving_quantile::value, "Get last value")
    ;
}
