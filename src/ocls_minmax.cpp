#include "ocls_minmax.h"

using namespace Rcpp;

// ===== ocls_moving_min =====
ocls_moving_min::ocls_moving_min(int window)
  : n(0),
    w(window){
}

void ocls_moving_min::enQ(double x) {
  while (!deq.empty() && deq.back() > x) {
    deq.pop_back();
  }
  deq.push_back(x);
}

void ocls_moving_min::deQ(double x) {
  if (deq.front() == x) {
    deq.pop_front();
  }
}

double ocls_moving_min::update_one(double x) {
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

NumericVector ocls_moving_min::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_min::value() {
  return deq.front();
}

// ===== ocls_moving_max =====
ocls_moving_max::ocls_moving_max(int window)
  : n(0),
    w(window){
}

void ocls_moving_max::enQ(double x) {
  while (!deq.empty() && deq.back() < x) {
    deq.pop_back();
  }
  deq.push_back(x);
}

void ocls_moving_max::deQ(double x) {
  if (deq.front() == x) {
    deq.pop_front();
  }
}

double ocls_moving_max::update_one(double x) {
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

NumericVector ocls_moving_max::update(NumericVector x) {
  auto npt = x.length();
  auto y = NumericVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

double ocls_moving_max::value() {
  return deq.front();
}

// ===== ocls_moving_argmin =====
ocls_moving_argmin::ocls_moving_argmin(int window)
  : n(0),
    w(window){
}

void ocls_moving_argmin::enQ(double x) {
  while (!deq.empty() && deq.back() > x) {
    deq.pop_back();
    argdeq.pop_back();
  }
  deq.push_back(x);
  argdeq.push_back(0);
  // update counters
  for (auto& it : argdeq) {
    it += 1;
  }
}

void ocls_moving_argmin::deQ(double x) {
  if (deq.front() == x) {
    deq.pop_front();
    argdeq.pop_front();
  }
}

int ocls_moving_argmin::update_one(double x) {
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

IntegerVector ocls_moving_argmin::update(NumericVector x) {
  auto npt = x.length();
  auto y = IntegerVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

int ocls_moving_argmin::value() {
  return argdeq.front();
}

// ===== ocls_moving_argmax =====
ocls_moving_argmax::ocls_moving_argmax(int window)
  : n(0),
    w(window){
}

void ocls_moving_argmax::enQ(double x) {
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

void ocls_moving_argmax::deQ(double x) {
  if (deq.front() == x) {
    deq.pop_front();
    argdeq.pop_front();
  }
}

int ocls_moving_argmax::update_one(double x) {
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

IntegerVector ocls_moving_argmax::update(NumericVector x) {
  auto npt = x.length();
  auto y = IntegerVector(npt);
  for (auto i = 0; i < npt; ++i) {
    y[i] = update_one(x[i]);
  }
  return y;
}

int ocls_moving_argmax::value() {
  return argdeq.front();
}

RCPP_MODULE(ocls_minmax) {
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
