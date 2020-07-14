#include <Rcpp.h>
using namespace Rcpp;

typedef std::deque<double> deque;

class ocls_ema {

  bool init;
  double alpha, ema;

public:

  ocls_ema(int period) {
    init = false;
    ema = 0.0;
    alpha = 2.0 / (period + 1);
  }

  double update_one(double x) {
    if (init) {
      ema += (x - ema) * alpha;
    } else {
      ema = x;
      init = true;
    }
    return ema;
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    if (init) {
      for (auto i = 0; i < npt; ++i) {
        ema += (x[i] - ema) * alpha;
        y[i] = ema;
      }
    } else {
      y[0] = ema = x[0];
      init = true;
      for (auto i = 1; i < npt; ++i) {
        ema += (x[i] - ema) * alpha;
        y[i] = ema;
      }
    }
    return y;
  }

  double value() {
    return ema;
  }

};

class ocls_dema {

  bool init;
  double alpha, ema, ema2;

public:

  ocls_dema(int period) {
    init = false;
    ema = ema2 = 0.0;
    alpha = 2.0 / (period + 1);
  }

  double update_one(double x) {
    if (init) {
      ema  += (x   - ema ) * alpha;
      ema2 += (ema - ema2) * alpha;
    } else {
      ema = ema2 = x;
      init = true;
    }
    return ema2;
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    if (init) {
      for (auto i = 0; i < npt; ++i) {
        ema  += (x[i] - ema)  * alpha;
        ema2 += (ema  - ema2) * alpha;
        y[i] = ema2;
      }
    } else {
      y[0] = ema = ema2 = x[0];
      init = true;
      for (auto i = 1; i < npt; ++i) {
        ema  += (x[i] - ema)  * alpha;
        ema2 += (ema  - ema2) * alpha;
        y[i] = ema2;
      }
    }
    return y;
  }

  double value() {
    return ema2;
  }

};

class ocls_tema {

  bool init;
  double alpha, ema, ema2, ema3;

public:

  ocls_tema(int period) {
    init = false;
    ema = ema2 = ema3 = 0.0;
    alpha = 2.0 / (period + 1);
  }

  double update_one(double x) {
    if (init) {
      ema  += (x    - ema ) * alpha;
      ema2 += (ema  - ema2) * alpha;
      ema3 += (ema2 - ema3) * alpha;
    } else {
      ema = ema2 = ema3 = x;
      init = true;
    }
    return ema3;
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    if (init) {
      for (auto i = 0; i < npt; ++i) {
        ema  += (x[i] - ema)  * alpha;
        ema2 += (ema  - ema2) * alpha;
        ema3 += (ema2 - ema3) * alpha;
        y[i] = ema3;
      }
    } else {
      y[0] = ema = ema2 = ema3 = x[0];
      init = true;
      for (auto i = 1; i < npt; ++i) {
        ema  += (x[i] - ema)  * alpha;
        ema2 += (ema  - ema2) * alpha;
        ema3 += (ema2 - ema3) * alpha;
        y[i] = ema3;
      }
    }
    return y;
  }

  double value() {
    return ema3;
  }

};

class ocls_zlema {

  int n, lag;
  double alpha, ema;
  deque buf;
  double datum;

public:

  ocls_zlema(int period) {
    n = 0;
    lag = (period - 1) / 2;
    alpha = 2.0 / (period + 1);
    ema = 0.0;
  }

  double update_one(double x) {
    buf.push_front(x);
    if (n < lag) {
      n += 1;
      ema = x;
    } else {
      datum = x + (x - buf.back());
      ema  += (datum - ema) * alpha;
      buf.pop_back();
    }
    return ema;
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
    return ema;
  }

};

class ocls_wilders {

  bool init;
  double alpha, wilders;

public:

  ocls_wilders(int period) {
    init = false;
    alpha = 1.0 / period;
    wilders = 0.0;
  }

  double update_one(double x) {
    if (init) {
      wilders += (x - wilders) * alpha;
    } else {
      wilders = x;
      init = true;
    }
    return wilders;
  }

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    if (init) {
      for (auto i = 0; i < npt; ++i) {
        wilders += (x[i] - wilders) * alpha;
        y[i] = wilders;
      }
    } else {
      y[0] = wilders = x[0];
      init = true;
      for (auto i = 1; i < npt; ++i) {
        wilders += (x[i] - wilders) * alpha;
        y[i] = wilders;
      }
    }
    return y;
  }

  double value() {
    return wilders;
  }

};

class ocls_sma {

  int n, p;
  double m;
  deque buf;

public:

  ocls_sma(int period) {
    p = period;
    n = 0;
    m = 0.0;
  }

  double update_one(double x) {
    buf.push_front(x);
    if (n < p) {
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

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  double value() {
    return m;
  }

};

class ocls_wma {

  int n, p, tot_w;
  //total weight, weighted sum, sum
  double ws, s;
  deque buf;

public:

  ocls_wma(int period) {
    p = period;
    n = 0;
    tot_w = 0;
    ws = s = 0.0;
  }

  double update_one(double x) {
    buf.push_front(x);
    if (n < p) {
      // cumulative stage
      n += 1;
      tot_w += n;
      ws    += x * n;
      s     += x;
    } else {
      ws += x * n - s;
      s  += x - buf.back();
      buf.pop_back();
    }
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
    return ws / tot_w;
  }

};

class ocls_kama {

  int n, p;
  double a_short, a_long, a_delta, a0, sdelta, kama;
  deque buf;
  double old, e, a;

public:

  ocls_kama(int period, int period_short = 2, int period_long = 30) {
    p = period;
    n = 0;
    a_short = 2.0 / (period_short + 1);
    a_long  = 2.0 / (period_long + 1);
    a_delta = a_short - a_long;
    a0      = (a_delta + a_long) * (a_delta + a_long);
    sdelta  = 0.0;
    kama    = 0.0;
  }

  double update_one(double x) {
    if (n) {
      sdelta += fabs(x - buf.front());
    }
    buf.push_front(x);
    if (n < p) {
      // cumulative stage
      n += 1;
      kama = x;
    } else {
      old = buf.back();
      buf.pop_back();
      if (sdelta != 0.0) {
        e = fabs(x - old) / sdelta * a_delta + a_long;
        a = e * e;
      } else {
        a = a0;
      }
      sdelta -= abs(old - buf.back());
      kama   += (x - kama) * a;
    }
    return kama;
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
    return kama;
  }

};

class ocls_hma {

  int p, m, s;
  ocls_wma wma_p, wma_m, wma_s;

public:

  ocls_hma(int period)
    : p(period),
      m(p / 2),
      s(sqrt(p)),
      wma_p(p),
      wma_m(m),
      wma_s(s) {
  }

  double update_one(double x) {
    auto mp = wma_p.update_one(x);
    auto mm = wma_m.update_one(x);
    auto x_new = 2.0 * mm - mp;
    return wma_s.update_one(x_new);
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
    return wma_s.value();
  }

};

class ocls_vwma {

  int n, p_;
  deque buft, bufv;
  // sum of turnover, volume
  double st, sv;
  double t;

public:

  ocls_vwma(int period) {
    p_ = period;
    n = 0;
    st = 0.0;
    sv = 0.0;
  }

  double update_one(double p, double v) {
    t = p * v;
    buft.push_front(t);
    bufv.push_front(v);
    if (n < p_) {
      // cumulative stage
      n  += 1;
      st += t;
      sv += v;
    } else {
      st += t - buft.back();
      sv += v - bufv.back();
      buft.pop_back();
      bufv.pop_back();
    }
    return value();
  }

  NumericVector update(NumericVector p, NumericVector v) {
    auto npt = p.length();
    auto y = NumericVector(npt);

    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(p[i], v[i]);
    }
    return y;
  }

  double value() {
    return st / sv;
  }

};

class ocls_vidya {

  bool init;
  int pshort, plong, ns, nl;
  double alpha, vidya;

  deque bufs, bufl;
  double s2s, ms, ds, d0s, dd0s,
         s2l, ml, dl, d0l, dd0l;
  double nr;

public:

  ocls_vidya(int period_short, int period_long, double alpha)
    : pshort(period_short),
      plong(period_long),
      init(false),
      ns(0),
      nl(0),
      alpha(alpha),
      vidya(0.0) {
    s2s = ms = ds = d0s = dd0s = 0.0;
    s2l = ml = dl = d0l = dd0l - 0.0;
    nr = alpha * sqrt(double(plong) / double(pshort));
  }

  double update_one(double x) {
    bufs.push_front(x);
    bufl.push_front(x);
    ds = x - ms;
    dl = x - ml;
    if (ns < pshort) {
      // cumulative stage: period short, period long
      ns += 1;
      nl += 1;
      ms += ds / ns;
      ml += dl / nl;
    } else if (nl < plong) {
      // cumulative stage: period long
      nl += 1;
      ml += dl / nl;
      // windowed stage: period short
      auto old = bufs.back();
      bufs.pop_back();
      d0s = old - ms;
      ms += (x - old) / ns;
    } else {
      // windowed stage: period short
      auto old = bufs.back();
      bufs.pop_back();
      d0s = old - ms;
      ms += (x - old) / ns;
      // windowed stage: period short
      old = bufl.back();
      bufl.pop_back();
      d0l = old - ml;
      ml += (x - old) / nl;
    }
    dd0s = ds - d0s;
    dd0l = dl - d0l;
    s2s += ds * ds - d0s * d0s - dd0s * dd0s / ns;
    s2l += dl * dl - d0l * d0l - dd0l * dd0l / nl;

    if (nl < plong) {
      vidya = x;
    } else {
      auto r = nr * sqrt(s2s / s2l);
      vidya = (1.0 - r) * vidya + r * x;
    }
    return vidya;
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
    return vidya;
  }

};


RCPP_MODULE(ocls_ma){
  using namespace Rcpp;

  class_<ocls_ema>("ocls_ema")

    .constructor<int>()

    .method("update_one", &ocls_ema::update_one, "Update state by one value")
    .method("update", &ocls_ema::update, "Update state")
    .method("value", &ocls_ema::value, "Get last value")
    ;

  class_<ocls_dema>("ocls_dema")

    .constructor<int>()

    .method("update_one", &ocls_dema::update_one, "Update state by one value")
    .method("update", &ocls_dema::update, "Update state")
    .method("value", &ocls_dema::value, "Get last value")
    ;

  class_<ocls_tema>("ocls_tema")

    .constructor<int>()

    .method("update_one", &ocls_tema::update_one, "Update state by one value")
    .method("update", &ocls_tema::update, "Update state")
    .method("value", &ocls_tema::value, "Get last value")
    ;

  class_<ocls_zlema>("ocls_zlema")

    .constructor<int>()

    .method("update_one", &ocls_zlema::update_one, "Update state by one value")
    .method("update", &ocls_zlema::update, "Update state")
    .method("value", &ocls_zlema::value, "Get last value")
    ;

  class_<ocls_wilders>("ocls_wilders")

    .constructor<int>()

    .method("update_one", &ocls_wilders::update_one, "Update state by one value")
    .method("update", &ocls_wilders::update, "Update state")
    .method("value", &ocls_wilders::value, "Get last value")
    ;

  class_<ocls_sma>("ocls_sma")

    .constructor<int>()

    .method("update_one", &ocls_sma::update_one, "Update state by one value")
    .method("update", &ocls_sma::update, "Update state")
    .method("value", &ocls_sma::value, "Get last value")
    ;

  class_<ocls_wma>("ocls_wma")

    .constructor<int>()

    .method("update_one", &ocls_wma::update_one, "Update state by one value")
    .method("update", &ocls_wma::update, "Update state")
    .method("value", &ocls_wma::value, "Get last value")
    ;

  class_<ocls_kama>("ocls_kama")

    .constructor<int, int, int>()

    .method("update_one", &ocls_kama::update_one, "Update state by one value")
    .method("update", &ocls_kama::update, "Update state")
    .method("value", &ocls_kama::value, "Get last value")
    ;

  class_<ocls_hma>("ocls_hma")

    .constructor<int>()

    .method("update_one", &ocls_hma::update_one, "Update state by one value")
    .method("update", &ocls_hma::update, "Update state")
    .method("value", &ocls_hma::value, "Get last value")
    ;

  class_<ocls_vwma>("ocls_vwma")

    .constructor<int>()

    .method("update_one", &ocls_vwma::update_one, "Update state by one value")
    .method("update", &ocls_vwma::update, "Update state")
    .method("value", &ocls_vwma::value, "Get last value")
    ;

  class_<ocls_vidya>("ocls_vidya")

    .constructor<int, int, double>()

    .method("update_one", &ocls_vidya::update_one, "Update state by one value")
    .method("update", &ocls_vidya::update, "Update state")
    .method("value", &ocls_vidya::value, "Get last value")
    ;

}
