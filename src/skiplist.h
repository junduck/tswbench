#include <cmath>
#include <vector>

#ifndef __skiplist_h__
#define __skiplist_h__

template <class T>
struct SkiplistNode {
  // value
  T v;
  // depth
  int d;
  // forward pointers
  std::vector<SkiplistNode<T>*> f;
  // widths
  std::vector<size_t> w;

  SkiplistNode(T value, int depth, int maxlevel) {
    v = value;
    d = depth;
    f = std::vector<SkiplistNode<T>*>(maxlevel, nullptr);
    w = std::vector<size_t>(maxlevel, 0);
  }

};

template <class T, class Compare = std::less<T> >
class IndexableSkiplist {

  size_t size;
  int mxlv;
  SkiplistNode<T> *head;
  // reused working vars
  std::vector<SkiplistNode<T>*> found;

  Compare lt;

  int random_depth() {
    int depth = 1;
    // flip coins
    while(rand() % 2 && depth < mxlv) {
      ++depth;
    }
    return depth;
  }

public:
  IndexableSkiplist(int window): size(0), mxlv(1 + int(log(window))), lt() {
    head    = new SkiplistNode<T>(T(), mxlv, mxlv);
    found   = std::vector<SkiplistNode<T>*>(mxlv, nullptr);
  }

  T operator[](const size_t& i) {

    if (i < 0 || i >= size) {
      throw std::out_of_range("Index out of range.");
    }

    auto idx  = i + 1;
    auto node = head;
    for (auto lv = mxlv - 1; lv >= 0; --lv) {
      while (node->f[lv] != nullptr && idx >= node->w[lv]) {
        idx -= node->w[lv];
        node = node->f[lv];
      }
    }

    return node->v;
  }

  void insert(T value) {

    // traversed distance each level
    std::vector<size_t> dist(mxlv, 0);
    // find first node where node->f[0]->v >= value, insert before node->f[0]
    auto node = head;
    for (auto lv = mxlv - 1; lv >=0; --lv) {
      while (node->f[lv] != nullptr && lt(node->f[lv]->v, value)) {
        dist[lv] += node->w[lv];
        node      = node->f[lv];
      }
      found[lv] = node;
    }

    auto depth = random_depth();
    size_t width = 0;
    SkiplistNode<T> *newnode = new SkiplistNode<T>(value, depth, mxlv);
    for (auto lv = 0; lv < depth; ++lv) {
      auto prev = found[lv];
      // create link
      newnode->f[lv] = prev->f[lv];
      prev->f[lv]    = newnode;
      // adjust width
      newnode->w[lv] = prev->w[lv] - width;
      prev->w[lv]    = width + 1L;
      width         += dist[lv];
    }
    // adjust widths for skipped levels
    for (auto lv = depth; lv < mxlv; ++lv) {
      found[lv]->w[lv] += 1;
    }

    size += 1;
  }

  void remove(T value) {

    // find first node where node->f[0]->v >= value, remove node->f[0] if equal
    auto node = head;
    for (auto lv = mxlv - 1; lv >= 0; --lv) {
      while (node->f[lv] != nullptr && lt(node->f[lv]->v, value)) {
        node = node->f[lv];
      }
      found[lv] = node;
    }
    if (found[0]->f[0] == nullptr || lt(value, found[0]->f[0]->v)) {
      // not found, just return
      return;
    }

    node = found[0]->f[0];
    auto depth = node->d;
    for (auto lv = 0; lv < depth; ++lv) {
      auto prev = found[lv];
      // adjust width
      prev->w[lv] += prev->f[lv]->w[lv] - 1L;
      // adjust link
      prev->f[lv]  = prev->f[lv]->f[lv];
    }
    // adjust width for skipped levels
    for (auto lv = depth; lv < mxlv; ++lv) {
      found[lv]->w[lv] -= 1;
    }
    delete node;

    size -= 1;
  }

  size_t get_size() {
    return size;
  }

  std::vector<T> as_vector() {
    auto ans = std::vector<T>(size, T());
    auto node = head->f[0];
    for (auto i = 0; i < size; ++i) {
      ans[i] = node->v;
      node = node->f[0];
    }
    return ans;
  }

};
#endif
