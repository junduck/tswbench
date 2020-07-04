#include <Rcpp.h>
using namespace Rcpp;

typedef std::deque<double> deque;
typedef std::deque<int>   ideque;

class ocls_moving_min {

  int w, n;
  deque buf, deq;

  void enQ(double x) {
    while (!deq.empty() && deq.back() > x) {
      deq.pop_back();
    }
    deq.push_back(x);
  }

  void deQ(double x) {
    if (deq.front() == x) {
      deq.pop_front();
    }
  }

public:

  ocls_moving_min(int window) {
    n = 0;
    w = window;
  }

  double update_one(double x) {
    buf.push_front(x);
    enQ(x);
    if (n < w) {
      n += 1;
    } else {
      deQ(buf.back());
      buf.pop_back();
    }
    return deq.front();
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
    return deq.front();
  }

};

class ocls_moving_max {

  int w, n;
  deque buf, deq;

  void enQ(double x) {
    while (!deq.empty() && deq.back() < x) {
      deq.pop_back();
    }
    deq.push_back(x);
  }

  void deQ(double x) {
    if (deq.front() == x) {
      deq.pop_front();
    }
  }

public:

  ocls_moving_max(int window) {
    n = 0;
    w = window;
  }

  double update_one(double x) {
    buf.push_front(x);
    enQ(x);
    if (n < w) {
      n += 1;
    } else {
      deQ(buf.back());
      buf.pop_back();
    }
    return deq.front();
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
    return deq.front();
  }

};

class ocls_moving_argmin {

  int w, n;
  deque buf, deq;
  // argdeq tracks "survival counter" of each element in deq, not sure if is the
  // most efficient way to implement argmin/argmax
  ideque argdeq;

  void enQ(double x) {
    while (!deq.empty() && deq.back() > x) {
      deq.pop_back();
      argdeq.pop_back();
    }
    deq.push_back(x);
    // 0 instead of -1 is pushed since R index from 1
    argdeq.push_back(0);
    // update counters of all remaining elements
    for (auto& it : argdeq) {
      it += 1;
    }

  }

  void deQ(double x) {
    if (deq.front() == x) {
      deq.pop_front();
      argdeq.pop_front();
    }
  }

public:

  ocls_moving_argmin(int window) {
    n = 0;
    w = window;
  }

  int update_one(double x) {
    buf.push_front(x);
    enQ(x);
    if (n < w) {
      n += 1;
    } else {
      deQ(buf.back());
      buf.pop_back();
    }
    return argdeq.front();
  }

  IntegerVector update(NumericVector x) {
    auto npt = x.length();
    auto y = IntegerVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  int value() {
    return argdeq.front();
  }

};

class ocls_moving_argmax {

  int w, n;
  deque buf, deq;
  ideque argdeq;

  void enQ(double x) {
    while (!deq.empty() && deq.back() < x) {
      deq.pop_back();
      argdeq.pop_back();
    }
    deq.push_back(x);
    argdeq.push_back(0);
    for (auto& it : argdeq) {
      it += 1;
    }
  }

  void deQ(double x) {
    if (deq.front() == x) {
      deq.pop_front();
      argdeq.pop_front();
    }
  }

public:

  ocls_moving_argmax(int window) {
    n = 0;
    w = window;
  }

  int update_one(double x) {
    buf.push_front(x);
    enQ(x);
    if (n < w) {
      n += 1;
    } else {
      deQ(buf.back());
      buf.pop_back();
    }
    return argdeq.front();
  }

  IntegerVector update(NumericVector x) {
    auto npt = x.length();
    auto y = IntegerVector(npt);
    for (auto i = 0; i < npt; ++i) {
      y[i] = update_one(x[i]);
    }
    return y;
  }

  int value() {
    return argdeq.front();
  }

};

RCPP_MODULE(ocls_moving_minmax){
  using namespace Rcpp;

  class_<ocls_moving_min>("ocls_moving_min")

    .constructor<int>()

    .method("update_one", &ocls_moving_min::update_one, "Update state by one value")
    .method("update", &ocls_moving_min::update, "Update state")
    .method("value", &ocls_moving_min::value, "Get last value")
    ;

  class_<ocls_moving_max>("ocls_moving_max")

    .constructor<int>()

    .method("update_one", &ocls_moving_max::update_one, "Update state by one value")
    .method("update", &ocls_moving_max::update, "Update state")
    .method("value", &ocls_moving_max::value, "Get last value")
    ;

  class_<ocls_moving_argmin>("ocls_moving_argmin")

    .constructor<int>()

    .method("update_one", &ocls_moving_argmin::update_one, "Update state by one value")
    .method("update", &ocls_moving_argmin::update, "Update state")
    .method("value", &ocls_moving_argmin::value, "Get last value")
    ;

  class_<ocls_moving_argmax>("ocls_moving_argmax")

    .constructor<int>()

    .method("update_one", &ocls_moving_argmax::update_one, "Update state by one value")
    .method("update", &ocls_moving_argmax::update, "Update state")
    .method("value", &ocls_moving_argmax::value, "Get last value")
    ;
}
