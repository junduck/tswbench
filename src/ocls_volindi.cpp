#include <Rcpp.h>
using namespace Rcpp;

class ocls_obv {

  bool init;
  double obv, cls;

public:

  ocls_obv()
    : init(false),
      obv(0.0),
      cls(0.0){
  }

  double update_one(double close, double volume) {
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

  NumericVector update(NumericVector close, NumericVector volume) {
    auto npt = close.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(close[i], volume[i]);
    }
    return y;
  }

  double value() {
    return obv;
  }

};

class ocls_pnvi {

  bool init;
  double nvi, pvi, cls, vol;

public:

  ocls_pnvi(double base_index = 1000.0)
    : init(false),
      pvi(base_index),
      nvi(base_index),
      cls(0.0),
      vol(0.0) {
  }

  NumericVector update_one(double close, double volume) {
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

  NumericMatrix update(NumericVector close, NumericVector volume) {
    auto npt = close.length();
    auto ans = NumericMatrix(npt, 2);
    for (auto i = 0; i < npt; ++i) {
      ans(i, _) = update_one(close[i], volume[i]);
    }
    return ans;
  }

  NumericVector value() {
    return NumericVector{pvi, nvi};
  }
};

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

}
