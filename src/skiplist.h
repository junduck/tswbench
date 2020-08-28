#pragma once

#include <cmath>
#include <vector>

template <class T, class Compare = std::less<T>>
class IndexableSkiplist
{

  struct SkiplistNode
  {
    // value
    T v;
    // depth
    int d;
    // forward pointers
    std::vector<SkiplistNode *> f;
    // widths
    std::vector<size_t> w;

    SkiplistNode(T value, int depth, int maxlevel)
        : v(std::move(value)),
          d(depth),
          f(maxlevel, nullptr),
          w(maxlevel, 0)
    {
    }
  };

  size_t _size;
  int _cap, mxlv;
  SkiplistNode *head;
  // reused working vars
  std::vector<SkiplistNode *> found;

  Compare lt;

  int random_depth()
  {
    int depth = 1;
    // flip coins
    while (rand() % 2 && depth < mxlv)
    {
      ++depth;
    }
    return depth;
  }

public:
  IndexableSkiplist(int window)
      : _size(0),
        _cap(window),
        mxlv(1 + int(log(window))),
        found(mxlv, nullptr),
        lt(),
        head(new SkiplistNode(T(), mxlv, mxlv))
  {
  }

  IndexableSkiplist(const IndexableSkiplist &other)
      : IndexableSkiplist(other._cap)
  {
    auto next = other.head->f[0];
    while (next != nullptr)
    {
      insert(next->v);
    }
  }

  IndexableSkiplist(IndexableSkiplist &&other)
      : _size(other._size),
        _cap(other._cap),
        mxlv(other.mxlv),
        found(mxlv, nullptr),
        lt(),
        head(other.head)
  {
    other.head = nullptr;
  }

  ~IndexableSkiplist()
  {
    auto next = head;
    while (head != nullptr)
    {
      next = head->f[0];
      delete head;
      head = next;
    }
  }

  IndexableSkiplist &operator=(IndexableSkiplist rhs)
  {
    swap(*this, rhs);
    return *this;
  }

  friend void swap(IndexableSkiplist &lhs, IndexableSkiplist &rhs)
  {
    using std::swap;

    swap(lhs._size, rhs._size);
    swap(lhs._cap, rhs._cap);
    swap(lhs.mxlv, rhs.mxlv);
    swap(lhs.head, rhs.head);
    swap(lhs.found, rhs.found);
    swap(lhs.lt, rhs.lt);
  }

  T operator[](size_t i)
  {
    if (i < 0 || i >= _size)
    {
      throw std::out_of_range("Index out of range.");
    }

    auto idx = i + 1;
    auto node = head;
    for (auto lv = mxlv - 1; lv >= 0; --lv)
    {
      while (node->f[lv] != nullptr && idx >= node->w[lv])
      {
        idx -= node->w[lv];
        node = node->f[lv];
      }
    }

    return node->v;
  }

  void insert(T value)
  {
    // traversed distance each level
    std::vector<size_t> dist(mxlv, 0);
    // find first node where node->f[0]->v >= value, insert before node->f[0]
    auto node = head;
    for (auto lv = mxlv - 1; lv >= 0; --lv)
    {
      while (node->f[lv] != nullptr && lt(node->f[lv]->v, value))
      {
        dist[lv] += node->w[lv];
        node = node->f[lv];
      }
      found[lv] = node;
    }

    auto depth = random_depth();
    size_t width = 0;
    SkiplistNode *newnode = new SkiplistNode(value, depth, mxlv);
    for (auto lv = 0; lv < depth; ++lv)
    {
      auto prev = found[lv];
      // create link
      newnode->f[lv] = prev->f[lv];
      prev->f[lv] = newnode;
      // adjust width
      newnode->w[lv] = prev->w[lv] - width;
      prev->w[lv] = width + 1L;
      width += dist[lv];
    }
    // adjust widths for skipped levels
    for (auto lv = depth; lv < mxlv; ++lv)
    {
      found[lv]->w[lv] += 1;
    }

    _size += 1;
  }

  void merge(const IndexableSkiplist &rhs)
  {
    // naiive merge of Skiplist
    auto node = rhs.head;
    while (node != nullptr)
    {
      insert(node->v);
      node = node->f[0];
    }
  }

  void remove(T value)
  {
    // find first node where node->f[0]->v >= value, remove node->f[0] if equal
    auto node = head;
    for (auto lv = mxlv - 1; lv >= 0; --lv)
    {
      while (node->f[lv] != nullptr && lt(node->f[lv]->v, value))
      {
        node = node->f[lv];
      }
      found[lv] = node;
    }
    if (found[0]->f[0] == nullptr || lt(value, found[0]->f[0]->v))
    {
      // not found, just return
      return;
    }

    node = found[0]->f[0];
    auto depth = node->d;
    for (auto lv = 0; lv < depth; ++lv)
    {
      auto prev = found[lv];
      // adjust width
      prev->w[lv] += prev->f[lv]->w[lv] - 1L;
      // adjust link
      prev->f[lv] = prev->f[lv]->f[lv];
    }
    // adjust width for skipped levels
    for (auto lv = depth; lv < mxlv; ++lv)
    {
      found[lv]->w[lv] -= 1;
    }
    delete node;

    _size -= 1;
  }

  size_t size() const
  {
    return _size;
  }

  size_t capacity() const
  {
    return _cap;
  }

  bool empty() const
  {
    return _size == 0;
  }

  std::vector<T> as_vector() const
  {
    auto ans = std::vector<T>(_size);
    auto node = head->f[0];
    for (auto i = 0; i < _size; ++i)
    {
      ans[i] = node->v;
      node = node->f[0];
    }
    return ans;
  }
};
