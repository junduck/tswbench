#include "ocls_volclk.h"

using namespace Rcpp;

// ===== ocls_volclk_order =====
ocls_volclk_order::ocls_volclk_order(double bin_vol)
  : bin(bin_vol),
    init(false) {
  lead = lead_tnvr = open = high = low = close = 0.0;
}

NumericVector ocls_volclk_order::update_one(double price, double volume) {
  double o, h, l, c, a;
  int nbin;
  double tot = lead + volume;
  if (!init) {
    open = high = low = close = price;
    init = true;
  }
  o = open;
  h = high  = high > price ? high : price;
  l = low   = low  < price ? low  : price;
  c = close = price;
  if (tot < bin) {
    lead       = tot;
    lead_tnvr += volume * price;
    a          = lead_tnvr / lead;
    nbin       = 0;
  } else {
    lead_tnvr += (bin - lead) * price;
    a          = lead_tnvr / bin;
    nbin       = 1;
    lead = tot - bin;
    while (lead >= bin) {
      lead -= bin;
      ++nbin;
    }
    open = high = low = price;
    lead_tnvr = lead * price;
  }
  //Open, High, Low, Close, VWAP, nbin
  return NumericVector{o, h, l, c, a, double(nbin)};
}

NumericMatrix ocls_volclk_order::update(NumericVector price, NumericVector volume) {
  auto npt = price.length();
  auto ans = NumericMatrix(npt, 6);
  for (auto i = 0; i < npt; ++i) {
    ans(i, _) = update_one(price[i], volume[i]);
  }
  return ans;
}

NumericVector ocls_volclk_order::value() {
  return NumericVector{open, high, low, close, lead_tnvr / lead, 0.0};
}

// ===== ocls_volclk_tick =====
ocls_volclk_tick::ocls_volclk_tick(double bin_vol)
  : volclk(bin_vol),
    vol_last_tick(0.0) {
}

NumericVector ocls_volclk_tick::update_one(double price, double volume) {
  auto tmp = volume;
  volume -= vol_last_tick;
  if (volume < 0.0) {
    stop("Volume is not monotonic increasing.");
  }
  vol_last_tick = tmp;
  return volclk.update_one(price, volume);
}

NumericMatrix ocls_volclk_tick::update(NumericVector price, NumericVector volume) {
  auto npt = price.length();
  auto ans = NumericMatrix(npt, 6);
  for (auto i = 0; i < npt; ++i) {
    ans(i, _) = update_one(price[i], volume[i]);
  }
  return ans;
}

NumericVector ocls_volclk_tick::value() {
  return volclk.value();
}

RCPP_MODULE(ocls_volclk){
  using namespace Rcpp;

  class_<ocls_volclk_order>("ocls_volclk_order")
    .constructor<double>()
    .method("update_one", &ocls_volclk_order::update_one, "Update state by one value")
    .method("update", &ocls_volclk_order::update, "Update state")
    .method("value", &ocls_volclk_order::value, "Get last value")
    ;

  class_<ocls_volclk_tick>("ocls_volclk_tick")
    .constructor<double>()
    .method("update_one", &ocls_volclk_tick::update_one, "Update state by one value")
    .method("update", &ocls_volclk_tick::update, "Update state")
    .method("value", &ocls_volclk_tick::value, "Get last value")
    ;
}
