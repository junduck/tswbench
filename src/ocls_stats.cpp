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

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      if (n < w) {
        // cumulative stage
        n += 1;
        m += (x[i] - m) / n;
      } else {
        // windowed stage
        m += (x[i] - buf.back()) / n;
        buf.pop_back();
      }
      buf.push_front(x[i]);
      y[i] = m;
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

  NumericVector update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      n += 1.0;
      m += (x[i] - m) / n;
      y[i] = m;
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

  void update_one(double x) {
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
  }

public:

  ocls_moving_sd(int window) {
    w = window;
    n = 0;
    s2 = m = d = d0 = dd0 = 0.0;
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 2);
    for (auto i = 0; i < npt; ++i) {
      update_one(x[i]);
      s2 += d * d - d0 * d0 - dd0 * dd0 / n;
      y(i, 0) = m;
      y(i, 1) = sqrt(s2 / (n - 1));
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

  void update_one(double x) {
    n += 1.0;
    d  = x - m;
    m += d / n;
  }

public:

  ocls_cumulative_sd() {
    n = 0.0;
    s2 = m = 0.0;
    d = 0.0;
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 2);
    double d_2;
    for (auto i = 0; i < npt; ++i) {
      update_one(x[i]);
      // TODO: investigate numeric stability
      s2 += (1.0 - 1.0 / n) * d * d;
      y(i, 0) = m;
      y(i, 1) = sqrt(s2 / (n - 1.0));
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

  void update_one(double x) {
    d = x - m;
    if (n < w) {
      // cumulative stage
      n += 1;
      m += d / n;
      n2 = n * n;
      n3 = n2 * n;
    } else {
      auto old = buf.back();
      buf.pop_back();
      d0 = old - m;
      m += (x - old) / n;
    }
    buf.push_front(x);
    // use SMA when order = 1 for better performance
    dd0   = d - d0;
    d_2   = d * d;
    d0_2  = d0 * d0;
    dd0_2 = dd0 * dd0;
  }

public:

  ocls_moving_moment(int window, int order = 4) {
    o = order;
    w = window;
    n = n2 = n3 = 0;
    s2 = s3 = s4 = m = 0.0;
    d = d0 = dd0 = 0.0;
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 4);
    for (auto i = 0; i < npt; ++i) {
      update_one(x[i]);
      switch (o) {
      // TODO: investigate numeric stability
      case 4:
        s4 += d_2 * d_2 - d0_2 * d0_2
          - 4.0 * dd0 * (s3 + d_2 * d - d0_2 * d0) / n
          + 6.0 * (s2 + d_2 - d0_2) * dd0_2 / n2
          - 3.0 * dd0_2 * dd0_2 / n3;
        y(i, 3) = s4 / n;
      case 3:
        s3 += d_2 * d - d0_2 * d0
          - 3.0 * dd0 * (s2 + d_2 - d0_2) / n
          + 2.0 * dd0_2 * dd0 / n2;
        y(i, 2) = s3 / n;
      case 2:
        s2 += d_2 - d0_2 - dd0_2 / n;
        y(i, 1) = s2 / n;
      case 1:
        y(i, 0) = m;
      }
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
  double d;

  void update_one(double x) {
    n += 1.0;
    d  = x - m;
    m += d / n;
  }

public:

  ocls_cumulative_moment(int order = 4) {
    o = order;
    n = 0.0;
    s2 = s3 = s4 = m = 0.0;
    d = 0.0;
  }

  NumericMatrix update(NumericVector x) {
    auto npt = x.length();
    auto y = NumericMatrix(npt, 4);
    double d_2;
    for (auto i = 0; i < npt; ++i) {
      update_one(x[i]);
      d_2 = d * d;
      switch (o) {
      // TODO: investigate numeric stability
      case 4:
        s4 += (1.0 - 3.0 / n / n / n) * d_2 * d_2
          - 4.0 * d * (s3 + d_2 * d) / n
          + 6.0 * (s2 + d_2) * d_2 / n / n;
        y(i, 3) = s4 / n;
      case 3:
        s3 += (1.0 + 2.0 / n / n) * d_2 * d
          - 3.0 * d * (s2 + d_2) / n;
        y(i, 2) = s3 / n;
      case 2:
        s2 += (1.0 - 1.0 / n) * d_2;
        y(i, 1) = s2 / n;
      case 1:
        y(i, 0) = m;
      }
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

  void update_one(double x, double y) {
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
  }

public:

  ocls_moving_cov(int window) {
    w = window;
    n = 0;
    sxy = mx = my = 0.0;
    dx = dy = d0x = d0y = 0.0;
  }

  NumericVector update(NumericVector x, NumericVector y) {
    auto npt = x.length();
    auto ans = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      update_one(x[i], y[i]);
      ans[i] = sxy / (n - 1);
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

  void update_one(double x, double y) {
    dx   = x - mx;
    dy   = y - my;
    n   += 1.0;
    mx  += dx / n;
    my  += dy / n;

    sxy += (1.0 - 1.0 / n) * dx * dy;
  }

public:

  ocls_cumulative_cov() {
    n = 0.0;
    sxy = 0.0;
    mx = my = dx = dy = 0.0;
  }

  NumericVector update(NumericVector x, NumericVector y) {
    auto npt = x.length();
    auto ans = NumericVector(npt);
    for (auto i = 0; i < npt; ++i) {
      update_one(x[i], y[i]);
      ans[i] = sxy / (n - 1.0);
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

public:

  ocls_moving_zscore(int window, double zscore, double attenu) {
    w = window;
    n = 0;
    s2 = m = sd = 0.0;
    z = zscore;
    r = attenu;
  }

  NumericVector update(NumericVector x) {

    auto npt = x.length();
    auto y = NumericVector(npt);
    double newpt, oldpt, d, d0, dd0;

    d = d0 = dd0 = 0.0;

    for (auto i = 0; i < npt; ++i) {

      if (n < w) {
        // initial window
        newpt = x[i];
        n += 1;
        d  = newpt - m;
        m += d / n;
      } else {
        if (fabs(x[i] - m) > z * sd) {
          y[i] = (x[i] - m) / sd;
          // apply attenuation
          newpt = r * x[i] + (1.0 - r) * buf.front();
        } else {
          newpt = x[i];
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
    }
    return y;
  }

};

RCPP_MODULE(ocls_stats){
  using namespace Rcpp;

  class_<ocls_moving_mean>("ocls_moving_mean")

    .constructor<int>()

    .method("update", &ocls_moving_mean::update, "Update state")
    .method("value", &ocls_moving_mean::value, "Get last value")
    ;

  class_<ocls_cumulative_mean>("ocls_cumulative_mean")

    .constructor()

    .method("update", &ocls_cumulative_mean::update, "Update state")
    .method("value", &ocls_cumulative_mean::value, "Get last value")
    ;

  class_<ocls_moving_sd>("ocls_moving_sd")

    .constructor<int>()

    .method("update", &ocls_moving_sd::update, "Update state")
    .method("value", &ocls_moving_sd::value, "Get last value")
    ;

  class_<ocls_cumulative_sd>("ocls_cumulative_sd")

    .constructor()

    .method("update", &ocls_cumulative_sd::update, "Update state")
    .method("value", &ocls_cumulative_sd::value, "Get last value")
    ;

  class_<ocls_moving_moment>("ocls_moving_moment")

    .constructor<int, int>()

    .method("update", &ocls_moving_moment::update, "Update state")
    ;

  class_<ocls_cumulative_moment>("ocls_cumulative_moment")

    .constructor<int>()

    .method("update", &ocls_cumulative_moment::update, "Update state")
    ;

  class_<ocls_moving_cov>("ocls_moving_cov")

    .constructor<int>()

    .method("update", &ocls_moving_cov::update, "Update state")
    ;

  class_<ocls_cumulative_cov>("ocls_cumulative_cov")

    .constructor()

    .method("update", &ocls_cumulative_cov::update, "Update state")
    ;

  class_<ocls_moving_zscore>("ocls_moving_zscore")

    .constructor<int, double, double>()

    .method("update", &ocls_moving_zscore::update, "Update state")
    ;
}
