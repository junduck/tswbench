#include <Rcpp.h>
using namespace Rcpp;

typedef std::deque<double> deque;

class ocls_moving_mean {

  int n, w;
  deque buf;
  double m;

public:

  ocls_moving_mean(int window) {
    w = window;
    n = 0;
    m = 0.0;
  }

  double update_one(double x) {
    if (n < w) {
      // cumulative stage
      n += 1;
      m += (x - m) / n;
    } else {
      // windowed stage
      m += (x - buf.back()) / n;
      buf.pop_back();
    }
    buf.push_front(x);
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

class ocls_cumulative_mean {

  double n, m;

public:

  ocls_cumulative_mean() {
    n = m = 0.0;
  }

  double update_one(double x) {
    n += 1.0;
    m += (x - m) / n;
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

class ocls_moving_sd {

  int n, w;
  deque buf;
  double s2, m, d, d0, dd0;

public:

  ocls_moving_sd(int window) {
    w = window;
    n = 0;
    s2 = m = d = d0 = dd0 = 0.0;
  }

  NumericVector update_one(double x) {
    d = x - m;
    if (n < w) {
      // cumulative stage
      n += 1;
      m += d / n;
    } else {
      // windowed stage
      auto old = buf.back();
      buf.pop_back();
      d0 = old - m;
      m += (x - old) / n;
    }
    buf.push_front(x);

    dd0 = d - d0;
    s2 += d * d - d0 * d0 - dd0 * dd0 / n;
    return value();
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 2);
    for (auto i = 0; i < npt; ++i) {
      y(i, _) = update_one(x[i]);
    }
    return y;
  }

  NumericVector value() {
    NumericVector y = {m, sqrt(s2 / (n - 1))};
    return y;
  }

};

class ocls_cumulative_sd {

  double n;
  double s2, m;
  double d;

public:

  ocls_cumulative_sd() {
    n = 0.0;
    s2 = m = 0.0;
    d = 0.0;
  }

  NumericVector update_one(double x) {
    n += 1.0;
    d  = x - m;
    m += d / n;
    // TODO: investigate numeric stability
    s2 += (1.0 - 1.0 / n) * d * d;
    return value();
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 2);
    double d_2;
    for (auto i = 0; i < npt; ++i) {
      y(i, _) = update_one(x[i]);
    }
    return y;
  }

  NumericVector value() {
    NumericVector y = {m, sqrt(s2 / (n - 1.0))};
    return y;
  }

};

class ocls_moving_moment {

  int o;
  int w, n, n2, n3;
  deque buf;

  double s2, s3, s4, m;
  double d, d0, dd0, d_2, d0_2, dd0_2;

public:

  ocls_moving_moment(int window, int order = 4) {
    o = order;
    w = window;
    n = n2 = n3 = 0;
    s2 = s3 = s4 = m = 0.0;
    d = d0 = dd0 = 0.0;
  }

  NumericVector update_one(double x) {
    d = x - m;
    if (n < w) {
      // cumulative stage
      n += 1;
      m += d / n;
      n2 = n * n;
      n3 = n2 * n;
    } else {
      // windowed stage
      auto old = buf.back();
      buf.pop_back();
      d0 = old - m;
      m += (x - old) / n;
    }
    buf.push_front(x);

    dd0   = d - d0;
    d_2   = d * d;
    d0_2  = d0 * d0;
    dd0_2 = dd0 * dd0;
    switch (o) {
    // TODO: investigate numeric stability
    case 4:
      s4 += d_2 * d_2 - d0_2 * d0_2
      - 4.0 * dd0 * (s3 + d_2 * d - d0_2 * d0) / n
      + 6.0 * (s2 + d_2 - d0_2) * dd0_2 / n2
      - 3.0 * dd0_2 * dd0_2 / n3;
    case 3:
      s3 += d_2 * d - d0_2 * d0
      - 3.0 * dd0 * (s2 + d_2 - d0_2) / n
      + 2.0 * dd0_2 * dd0 / n2;
    case 2:
      s2 += d_2 - d0_2 - dd0_2 / n;
    }
    return value();
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 4);
    for (auto i = 0; i < npt; ++i) {
      y(i, _) = update_one(x[i]);
    }
    return y;
  }

  NumericVector value() {
    NumericVector y = {m, s2 / n, s3 / n, s4 / n};
    return y;
  }

};

class ocls_cumulative_moment {

  int o;
  double n;
  double s2, s3, s4, m;
  double d, d_2;

public:

  ocls_cumulative_moment(int order = 4) {
    o = order;
    n = 0.0;
    s2 = s3 = s4 = m = 0.0;
    d = 0.0;
  }

  NumericVector update_one(double x) {
    n += 1.0;
    d  = x - m;
    m += d / n;
    d_2 = d * d;
    switch (o) {
    // TODO: investigate numeric stability
    case 4:
      s4 += (1.0 - 3.0 / n / n / n) * d_2 * d_2
      - 4.0 * d * (s3 + d_2 * d) / n
      + 6.0 * (s2 + d_2) * d_2 / n / n;
    case 3:
      s3 += (1.0 + 2.0 / n / n) * d_2 * d
      - 3.0 * d * (s2 + d_2) / n;
    case 2:
      s2 += (1.0 - 1.0 / n) * d_2;
    }
    return value();
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 4);
    for (auto i = 0; i < npt; ++i) {
      y(i, _) = update_one(x[i]);
    }
    return y;
  }

  NumericVector value() {
    NumericVector y = {m, s2 / n, s3 / n, s4 / n};
    return y;
  }

};

class ocls_moving_cov {

  int w, n;
  deque bufx, bufy;

  double sxy, mx, my;
  double dx, dy, d0x, d0y;

public:

  ocls_moving_cov(int window) {
    w = window;
    n = 0;
    sxy = mx = my = 0.0;
    dx = dy = d0x = d0y = 0.0;
  }

  double update_one(double x, double y) {
    dx = x - mx;
    dy = y - my;
    if (n < w) {
      // cumulative stage
      n += 1;
      mx += dx / n;
      my += dy / n;
    } else {
      auto oldx = bufx.back();
      auto oldy = bufy.back();
      bufx.pop_back();
      bufy.pop_back();
      d0x = oldx - mx;
      d0y = oldy - my;
      mx += (x - oldx) / n;
      my += (y - oldy) / n;
    }
    bufx.push_front(x);
    bufy.push_front(y);

    sxy += dx * dy - d0x * d0y - (dx - d0x) * (dy - d0y) / n;
    return value();
  }

  NumericVector update(NumericVector x, NumericVector y) {
    auto npt = x.length();
    auto ans = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      ans[i] = update_one(x[i], y[i]);
    }
    return ans;
  }

  double value() {
    return sxy / (n - 1);
  }

};

class ocls_cumulative_cov {

  double n;
  double sxy;
  double mx, my, dx, dy;

public:

  ocls_cumulative_cov() {
    n = 0.0;
    sxy = 0.0;
    mx = my = dx = dy = 0.0;
  }

  double update_one(double x, double y) {
    dx   = x - mx;
    dy   = y - my;
    n   += 1.0;
    mx  += dx / n;
    my  += dy / n;

    sxy += (1.0 - 1.0 / n) * dx * dy;
    return value();
  }

  NumericVector update(NumericVector x, NumericVector y) {
    auto npt = x.length();
    auto ans = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      ans[i] = update_one(x[i], y[i]);
    }
    return ans;
  }

  double value() {
    return sxy / (n - 1.0);
  }

};

class ocls_moving_zscore {

  int n, w;
  deque buf;

  double s2, m, sd, z, r;
  double newpt, oldpt, d, d0, dd0;
  double signal;

public:

  ocls_moving_zscore(int window, double zscore, double attenu) {
    w = window;
    n = 0;
    s2 = m = sd = 0.0;
    d = d0 = dd0 = 0.0;
    signal = 0.0;
    z = zscore;
    r = attenu;
  }

  double update_one(double x) {
    if (n < w) {
      // cumulative stage
      newpt = x;
      n += 1;
      d  = newpt - m;
      m += d / n;
    } else {
      // windowed stage
      if (fabs(x - m) > z * sd) {
        signal = (x - m) / sd;
        // apply attenuation
        newpt = r * x + (1.0 - r) * buf.front();
      } else {
        signal = 0.0;
        newpt = x;
      }
      oldpt = buf.back();
      buf.pop_back();
      d  = newpt - m;
      d0 = oldpt - m;
      m += (newpt - oldpt) / n;
    }
    buf.push_front(newpt);

    // update stats
    dd0 = d - d0;
    s2 += d * d - d0 * d0 - dd0 * dd0 / n;
    sd  = sqrt(s2 / (n - 1));

    return signal;
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
    return signal;
  }

};

RCPP_MODULE(ocls_stats){
  using namespace Rcpp;

  class_<ocls_moving_mean>("ocls_moving_mean")

    .constructor<int>()

    .method("update_one", &ocls_moving_mean::update_one, "Update state by one value")
    .method("update", &ocls_moving_mean::update, "Update state")
    .method("value", &ocls_moving_mean::value, "Get last value")
    ;

  class_<ocls_cumulative_mean>("ocls_cumulative_mean")

    .constructor()

    .method("update_one", &ocls_cumulative_mean::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_mean::update, "Update state")
    .method("value", &ocls_cumulative_mean::value, "Get last value")
    ;

  class_<ocls_moving_sd>("ocls_moving_sd")

    .constructor<int>()

    .method("update_one", &ocls_moving_sd::update_one, "Update state by one value")
    .method("update", &ocls_moving_sd::update, "Update state")
    .method("value", &ocls_moving_sd::value, "Get last value")
    ;

  class_<ocls_cumulative_sd>("ocls_cumulative_sd")

    .constructor()

    .method("update_one", &ocls_cumulative_sd::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_sd::update, "Update state")
    .method("value", &ocls_cumulative_sd::value, "Get last value")
    ;

  class_<ocls_moving_moment>("ocls_moving_moment")

    .constructor<int, int>()

    .method("update_one", &ocls_moving_moment::update_one, "Update state by one value")
    .method("update", &ocls_moving_moment::update, "Update state")
    .method("value", &ocls_moving_moment::value, "Get last value")
    ;

  class_<ocls_cumulative_moment>("ocls_cumulative_moment")

    .constructor<int>()

    .method("update_one", &ocls_cumulative_moment::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_moment::update, "Update state")
    .method("value", &ocls_cumulative_moment::value, "Get last value")
    ;

  class_<ocls_moving_cov>("ocls_moving_cov")

    .constructor<int>()

    .method("update_one", &ocls_moving_cov::update_one, "Update state by one value")
    .method("update", &ocls_moving_cov::update, "Update state")
    .method("value", &ocls_moving_cov::value, "Get last value")
    ;

  class_<ocls_cumulative_cov>("ocls_cumulative_cov")

    .constructor()

    .method("update_one", &ocls_cumulative_cov::update_one, "Update state by one value")
    .method("update", &ocls_cumulative_cov::update, "Update state")
    .method("value", &ocls_cumulative_cov::value, "Get last value")
    ;

  class_<ocls_moving_zscore>("ocls_moving_zscore")

    .constructor<int, double, double>()

    .method("update_one", &ocls_moving_zscore::update_one, "Update state by one value")
    .method("update", &ocls_moving_zscore::update, "Update state")
    .method("value", &ocls_moving_zscore::value, "Get last value")
    ;
}
