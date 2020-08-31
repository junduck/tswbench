#include "ocls_ma.h"

using namespace Rcpp;

// ===== ocls_ema =====
ocls_ema::ocls_ema(int period)
  : init(false) {
  ema = 0.0;
  alpha = 2.0 / (period + 1);
}

double ocls_ema::update_one(double x) {
  if (init) {
    ema += (x - ema) * alpha;
  } else {
    ema = x;
    init = true;
  }
  return ema;
}

NumericVector ocls_ema::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  if (init) {
    for (decltype(npt) i = 0; i < npt; ++i) {
      ema += (x[i] - ema) * alpha;
      y[i] = ema;
    }
  } else {
    y[0] = ema = x[0];
    init = true;
    for (decltype(npt) i = 1; i < npt; ++i) {
      ema += (x[i] - ema) * alpha;
      y[i] = ema;
    }
  }
  return y;
}

double ocls_ema::value() {
  return ema;
}

// ===== ocls_dema =====
ocls_dema::ocls_dema(int period)
  : init(false) {
  ema = ema2 = 0.0;
  alpha = 2.0 / (period + 1);
}

double ocls_dema::update_one(double x) {
  if (init) {
    ema  += (x   - ema ) * alpha;
    ema2 += (ema - ema2) * alpha;
  } else {
    ema = ema2 = x;
    init = true;
  }
  return ema2;
}

NumericVector ocls_dema::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  if (init) {
    for (decltype(npt) i = 0; i < npt; ++i) {
      ema  += (x[i] - ema)  * alpha;
      ema2 += (ema  - ema2) * alpha;
      y[i] = ema2;
    }
  } else {
    y[0] = ema = ema2 = x[0];
    init = true;
    for (decltype(npt) i = 1; i < npt; ++i) {
      ema  += (x[i] - ema)  * alpha;
      ema2 += (ema  - ema2) * alpha;
      y[i] = ema2;
    }
  }
  return y;
}

double ocls_dema::value() {
  return ema2;
}

// ===== ocls_tema =====
ocls_tema::ocls_tema(int period)
  : init(false) {
  ema = ema2 = ema3 = 0.0;
  alpha = 2.0 / (period + 1);
}

double ocls_tema::update_one(double x) {
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

NumericVector ocls_tema::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  if (init) {
    for (decltype(npt) i = 0; i < npt; ++i) {
      ema  += (x[i] - ema)  * alpha;
      ema2 += (ema  - ema2) * alpha;
      ema3 += (ema2 - ema3) * alpha;
      y[i] = ema3;
    }
  } else {
    y[0] = ema = ema2 = ema3 = x[0];
    init = true;
    for (decltype(npt) i = 1; i < npt; ++i) {
      ema  += (x[i] - ema)  * alpha;
      ema2 += (ema  - ema2) * alpha;
      ema3 += (ema2 - ema3) * alpha;
      y[i] = ema3;
    }
  }
  return y;
}

double ocls_tema::value() {
  return ema3;
}

// ===== ocls_zlema =====
ocls_zlema::ocls_zlema(int period)
  : n(0) {
  ema = 0.0;
  lag = (period - 1) / 2;
  alpha = 2.0 / (period + 1);
}

double ocls_zlema::update_one(double x) {
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

NumericVector ocls_zlema::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_zlema::value() {
  return ema;
}

// ===== ocls_wilders =====
ocls_wilders::ocls_wilders(int period)
  : init(false) {
  wilders = 0.0;
  alpha = 1.0 / period;
}


double ocls_wilders::update_one(double x) {
  if (init) {
    wilders += (x - wilders) * alpha;
  } else {
    wilders = x;
    init = true;
  }
  return wilders;
}

NumericVector ocls_wilders::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  if (init) {
    for (decltype(npt) i = 0; i < npt; ++i) {
      wilders += (x[i] - wilders) * alpha;
      y[i] = wilders;
    }
  } else {
    y[0] = wilders = x[0];
    init = true;
    for (decltype(npt) i = 1; i < npt; ++i) {
      wilders += (x[i] - wilders) * alpha;
      y[i] = wilders;
    }
  }
  return y;
}

double ocls_wilders::value() {
  return wilders;
}

// ===== ocls_sma =====
ocls_sma::ocls_sma(int period)
  : p(period),
    n(0),
    m(0.0){
}

double ocls_sma::update_one(double x) {
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

NumericVector ocls_sma::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_sma::value() {
  return m;
}

// ===== ocls_wma =====
ocls_wma::ocls_wma(int period)
  : p(period),
    n(0),
    tot_w(0) {
  ws = s = 0.0;
}

double ocls_wma::update_one(double x) {
  buf.push_front(x);
  if (n < p) {
    // cumulative stage
    n += 1;
    tot_w += n;
    ws    += x * n;
    s     += x;
  } else {
    // windowed stage
    ws += x * n - s;
    s  += x - buf.back();
    buf.pop_back();
  }
  return value();
}

NumericVector ocls_wma::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_wma::value() {
  return ws / tot_w;
}

// ===== ocls_hma =====
ocls_hma::ocls_hma(int period)
  : p(period),
    m(p / 2),
    s(sqrt(p)),
    wma_p(p),
    wma_m(m),
    wma_s(s) {
}

double ocls_hma::update_one(double x) {
  auto mp = wma_p.update_one(x);
  auto mm = wma_m.update_one(x);
  auto x_new = 2.0 * mm - mp;
  return wma_s.update_one(x_new);
}

NumericVector ocls_hma::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_hma::value() {
  return wma_s.value();
}

// ===== ocls_kama =====
ocls_kama::ocls_kama(int period, int period_short, int period_long)
  : p(period),
    n(0),
    sdelta(0.0),
    kama(0.0) {
  a_short = 2.0 / (period_short + 1);
  a_long  = 2.0 / (period_long + 1);
  a_delta = a_short - a_long;
  a0      = (a_delta + a_long) * (a_delta + a_long);
}

double ocls_kama::update_one(double x) {
  if (n) {
    sdelta += fabs(x - buf.front());
  }
  buf.push_front(x);
  if (n < p) {
    // cumulative stage
    n += 1;
    kama = x;
  } else {
    // windowed stage
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

NumericVector ocls_kama::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_kama::value() {
  return kama;
}

// ===== ocls_vwma =====
ocls_vwma::ocls_vwma(int period)
  : p_(period),
    n(0),
    st(0.0),
    sv(0.0){
}

double ocls_vwma::update_one(double p, double v) {
  t = p * v;
  buft.push_front(t);
  bufv.push_front(v);
  if (n < p_) {
    // cumulative stage
    n  += 1;
    st += t;
    sv += v;
  } else {
    // windowed stage
    st += t - buft.back();
    sv += v - bufv.back();
    buft.pop_back();
    bufv.pop_back();
  }
  return value();
}

NumericVector ocls_vwma::update(NumericVector p, NumericVector v) {
  auto npt = p.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(p[i], v[i]);
  }
  return y;
}

double ocls_vwma::value() {
  return st / sv;
}

// ===== ocls_vidya =====
ocls_vidya::ocls_vidya(int period_short, int period_long, double alpha)
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

double ocls_vidya::update_one(double x) {
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
    // windowed stage: period short
    auto old = bufs.back();
    bufs.pop_back();
    d0s = old - ms;
    ms += (x - old) / ns;
    // cumulative stage: period long
    nl += 1;
    ml += dl / nl;
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

NumericVector ocls_vidya::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (decltype(npt) i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_vidya::value() {
  return vidya;
}

RCPP_MODULE(ocls_ma) {
  using namespace Rcpp;

  class_<ocls_ema>("ocls_ema")
    .constructor<int>()
    .method("update_one", &ocls_ema::update_one, "Update state by one value")
    .method("update", &ocls_ema::update, "Update state")
    .method("value", &ocls_ema::value, "Get last value")
  ;

  class_<ocls_ema>("ocls_dema")
    .constructor<int>()
    .method("update_one", &ocls_ema::update_one, "Update state by one value")
    .method("update", &ocls_ema::update, "Update state")
    .method("value", &ocls_ema::value, "Get last value")
  ;

  class_<ocls_ema>("ocls_tema")
    .constructor<int>()
    .method("update_one", &ocls_ema::update_one, "Update state by one value")
    .method("update", &ocls_ema::update, "Update state")
    .method("value", &ocls_ema::value, "Get last value")
  ;

  class_<ocls_ema>("ocls_zlema")
    .constructor<int>()
    .method("update_one", &ocls_ema::update_one, "Update state by one value")
    .method("update", &ocls_ema::update, "Update state")
    .method("value", &ocls_ema::value, "Get last value")
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

  class_<ocls_hma>("ocls_hma")
    .constructor<int>()
    .method("update_one", &ocls_hma::update_one, "Update state by one value")
    .method("update", &ocls_hma::update, "Update state")
    .method("value", &ocls_hma::value, "Get last value")
  ;

  class_<ocls_kama>("ocls_kama")
    .constructor<int, int, int>()
    .method("update_one", &ocls_kama::update_one, "Update state by one value")
    .method("update", &ocls_kama::update, "Update state")
    .method("value", &ocls_kama::value, "Get last value")
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
