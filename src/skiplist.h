#pragma once

#include <type_traits> // std::enable_if, std::is_default_constructible
#include <vector>      //std::vector
#include <cmath>       //std::log2
#include <functional>  //std::less
#include <stdexcept>   //std::out_of_range

template <class T, class C = std::less<T>, typename std::enable_if<std::is_default_constructible<T>::value>::type * = nullptr>
class IndexableSkiplist
{
public:
  IndexableSkiplist(size_t window)
      : _size(0),
        _cap(window),
        _mxlv(1 + static_cast<size_t>(std::log2(window))),
        found(_mxlv, nullptr),
        head(new SkiplistNode(T(), _mxlv, _mxlv))
  {
  }

  IndexableSkiplist(const IndexableSkiplist &other)
      : IndexableSkiplist(other._cap)
  {
    merge(other);
  }

  IndexableSkiplist(IndexableSkiplist &&other)
      : _size(other._size),
        _cap(other._cap),
        _mxlv(other._mxlv),
        found(_mxlv, nullptr),
        head(other.head)
  {
    other.head = nullptr;
  }

  IndexableSkiplist(size_t window, std::vector<T> from_state)
      : IndexableSkiplist(window)
  {
    for (const auto &val : from_state)
    {
      insert(val);
    }
  }

  std::vector<T> state() const
  {
    std::vector<T> s;
    s.reserve(_size);
    auto node = head->f[0];
    while (node != nullptr)
    {
      s.push_back(node->v);
      node = node->f[0];
    }
    return s;
  }

  std::vector<T> to_vector() const
  {
    return state();
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
    swap(lhs._mxlv, rhs._mxlv);
    swap(lhs.head, rhs.head);
    swap(lhs.found, rhs.found);
  }

  T at(size_t i) const
  {
    if (!i < _size)
    {
      throw std::out_of_range("Index out of range.");
    }
    return this->operator[](i);
  }

  T operator[](size_t i) const
  {
    // total span, including head
    auto span = i + 1;
    auto node = head;
    for (size_t lv = _mxlv; lv--; )
    {
      while (node->f[lv] != nullptr && span >= node->w[lv])
      {
        span -= node->w[lv];
        node = node->f[lv];
      }
    }
    return node->v;
  }

  size_t rank(T value) const
  {
    // find first node where node->f[0]->v >= value, return node->f[0]'s total span
    size_t rank = 0;
    auto node = head;
    for (size_t lv = _mxlv; lv--; )
    {
      while (node->f[lv] != nullptr && C()(node->f[lv]->v, value))
      {
        rank += node->w[lv];
        node = node->f[lv];
      }
    }
    return rank;
  }

  void insert(T value) {
    // traversed distance on each level
    std::vector<size_t> span(_mxlv, 0);
    // find first node where node->f[0]->v >= value
    auto node = head;
    for (size_t lv = _mxlv; lv--; )
    {
      while (node->f[lv] != nullptr && C()(node->f[lv]->v, value))
      {
        span[lv] += node->w[lv];
        node = node->f[lv];
      }
      found[lv] = node;
    }
    // insert between node and node->f[0]
    auto depth = random_depth();
    size_t width = 0;
    SkiplistNode *newnode = new SkiplistNode(value, depth, _mxlv);
    for (size_t lv = 0; lv < depth; ++lv)
    {
      auto prev = found[lv];
      // create link
      newnode->f[lv] = prev->f[lv];
      prev->f[lv] = newnode;
      // adjust width
      newnode->w[lv] = prev->w[lv] - width;
      prev->w[lv] = width + 1L;
      width += span[lv];
    }
    // adjust widths for skipped levels
    for (size_t lv = depth; lv < _mxlv; ++lv)
    {
      found[lv]->w[lv] += 1;
    }

    _size += 1;
  }

  void merge(const IndexableSkiplist &rhs)
  {
    // naiive merge of Skiplist
    auto node = rhs.head->f[0];
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
    for (size_t lv = _mxlv; lv--; )
    {
      while (node->f[lv] != nullptr && C()(node->f[lv]->v, value))
      {
        node = node->f[lv];
      }
      found[lv] = node;
    }
    if (found[0]->f[0] == nullptr || C()(value, found[0]->f[0]->v))
    {
      // not found, just return
      return;
    }

    node = found[0]->f[0];
    auto depth = node->d;
    for (size_t lv = 0; lv < depth; ++lv)
    {
      auto prev = found[lv];
      // adjust width
      prev->w[lv] += prev->f[lv]->w[lv] - 1L;
      // adjust link
      prev->f[lv] = prev->f[lv]->f[lv];
    }
    // adjust width for skipped levels
    for (size_t lv = depth; lv < _mxlv; ++lv)
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

private:
  struct SkiplistNode
  {
    // value
    T v;
    // depth
    size_t d;
    // forward pointers
    std::vector<SkiplistNode *> f;
    // widths
    std::vector<size_t> w;

    SkiplistNode(T value, size_t depth, size_t maxlevel)
        : v(value),
          d(depth),
          f(maxlevel, nullptr),
          w(maxlevel, 0)
    {
    }
  };

  size_t _size, _cap, _mxlv;
  SkiplistNode *head;
  // reused working vars
  std::vector<SkiplistNode *> found;

  size_t random_depth() const
  {
    size_t depth = 1;
    // flip coins
    while (std::rand() % 2 && depth < _mxlv)
    {
      ++depth;
    }
    return depth;
  }
};