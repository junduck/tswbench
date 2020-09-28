#pragma once

#include <type_traits> // std::enable_if, std::is_default_constructible
#include <vector>      // std::vector
#include <cmath>       // std::log2
#include <functional>  // std::less
#include <stdexcept>   // std::out_of_range
#include <iterator>    // std::input_iterator_tag
#include <memory>      // std::allocator, std::allocator_traits

template <class T, class C = std::less<T>, class A = std::allocator<T>, typename std::enable_if<std::is_default_constructible<T>::value>::type * = nullptr>
class IndexableSkiplist
{
private:
  struct node_type;

  struct level_type
  {
    node_type *f;
    size_t s;
  };
  using level_alloc_type = typename std::allocator_traits<A>::rebind_alloc<level_type>;

  struct node_type
  {
    std::vector<level_type, level_alloc_type> skl;
    T v;
    node_type(T value, size_t depth)
        : skl(depth, {nullptr, 0}),
          v(value)
    {
    }
  };
  using node_alloc_type = typename std::allocator_traits<A>::rebind_alloc<node_type>;
  using node_alloc_traits = typename std::allocator_traits<node_alloc_type>;

  using fptr_type = std::vector<node_type *>;
  using fspan_type = std::vector<size_t>;

  node_alloc_type _alloc;
  size_t _size, _mxlv;
  node_type *_head;
  fptr_type front;
  fspan_type fspan;

  size_t random_depth() const
  {
    size_t depth = 1;
    // flip coins
    while (std::rand() % 2)
    {
      ++depth;
    }
    return std::min(depth, _mxlv);
  }

  void remove_next(const fptr_type &fp)
  {
    auto node = fp[0]->skl[0].f;
    auto depth = node->skl.size();
    for (size_t lv = 0; lv < depth; ++lv)
    {
      fp[lv]->skl[lv].s += node->skl[lv].s - 1;
      fp[lv]->skl[lv].f = node->skl[lv].f;
    }
    for (size_t lv = depth; lv < _mxlv; ++lv)
    {
      fp[lv]->skl[lv].s -= 1;
    }
    del_node(node);
    _size -= 1;
  }

  node_type *new_node(T value, size_t depth)
  {
    auto node = node_alloc_traits::allocate(_alloc, 1);
    if (node)
    {
      node_alloc_traits::construct(_alloc, node, value, depth);
    }
    else
    {
      throw std::bad_alloc();
    }
    return node;
  }

  void del_node(node_type *node)
  {
    node_alloc_traits::destroy(_alloc, node);
    node_alloc_traits::deallocate(_alloc, node, 1);
  }

  void _move_skiplist(IndexableSkiplist &&other, std::true_type)
  {
    _alloc = std::move(other._alloc);
    _size = other._size;
    _mxlv = other._mxlv;
    _head = other._head;
    front = fptr_type(_mxlv, nullptr);
    fspan = fspan_type(_mxlv, 0U);
    other._head = nullptr;
  }

  void _move_skiplist(IndexableSkiplist &&other, std::false_type)
  {
    merge(other);
  }

  friend void _swap_skiplist(IndexableSkiplist &lhs, IndexableSkiplist &rhs, std::true_type)
  {
    std::swap(lhs._alloc, rhs._alloc);
    std::swap(lhs._size, rhs._size);
    std::swap(lhs._mxlv, rhs._mxlv);
    std::swap(lhs._head, rhs._head);
    std::swap(lhs.front, rhs.front);
    std::swap(lhs.fspan, rhs.fspan);
  }

  // undefined behaviour??? if called from std::swap
  friend void _swap_skiplist(IndexableSkiplist &lhs, IndexableSkiplist &rhs, std::false_type)
  {
    // _alloc is not swapped
    std::swap(lhs._size, rhs._size);
    std::swap(lhs._mxlv, rhs._mxlv);
    std::swap(lhs._head, rhs._head);
    std::swap(lhs.front, rhs.front);
    std::swap(lhs.fspan, rhs.fspan);
  }

public:
  explicit IndexableSkiplist(size_t target_size, const A &alloc = A())
      : _alloc(node_alloc_type(alloc)),
        _size(0),
        _mxlv(1 + static_cast<size_t>(std::log2(target_size))),
        _head(new_node(T(), _mxlv)),
        front(_mxlv, nullptr),
        fspan(_mxlv, 0)
  {
  }

  // O(N log N): Insert (unsorted) elements from vector
  IndexableSkiplist(size_t target_size, const std::vector<T> &from_vector, const A &alloc = A())
      : IndexableSkiplist(target_size, alloc)
  {
    for (auto val : from_vector)
    {
      insert(val);
    }
  }

  ~IndexableSkiplist()
  {
    while (_head != nullptr)
    {
      auto node = _head;
      _head = _head->skl[0].f;
      del_node(node);
    }
  }

  // resource management

  // O(N): Instead of copying, we initialise a dummy obj, then merge and swap
  IndexableSkiplist(const IndexableSkiplist &other)
      : IndexableSkiplist(1U, node_alloc_traits::select_on_container_copy_construction(other._alloc))
  {
    merge(other);
  }

  // O(1) if we can move, fallback to O(N) merge
  IndexableSkiplist(IndexableSkiplist &&other) noexcept
  {
    constexpr bool movable = node_alloc_traits::is_always_equal::value || node_alloc_traits::propagate_on_container_move_assignment::value;
    _move_skiplist(std::move(other), std::integral_constant<bool, movable>());
  }

  IndexableSkiplist &operator=(IndexableSkiplist rhs) noexcept
  {
    swap(*this, rhs);
    return *this;
  }

  friend void swap(IndexableSkiplist &lhs, IndexableSkiplist &rhs) noexcept
  {
    using std::swap;
    constexpr bool swappable = node_alloc_traits::is_always_equal::value || node_alloc_traits::propagate_on_container_swap::value;
    _swap_skiplist(lhs, rhs, std::integral_constant<bool, swappable>());
  }

  // Implements: O(log N) search, insert, remove O(N + M) merge

  T at(size_t i) const
  {
    if (i >= _size)
    {
      throw std::out_of_range("Index out of range.");
    }
    return this->operator[](i);
  }

  T operator[](size_t i) const
  {
    auto span = i + 1;
    auto node = _head;
    for (size_t lv = _mxlv; lv--;)
    {
      while (node->skl[lv].f && span >= node->skl[lv].s)
      {
        span -= node->skl[lv].s;
        node = node->skl[lv].f;
      }
    }
    return node->v;
  }

  size_t rank(T value) const
  {
    size_t rank = 0;
    auto node = _head;
    for (size_t lv = _mxlv; lv--;)
    {
      while (node->skl[lv].f && C()(node->skl[lv].f->v, value))
      {
        rank += node->skl[lv].s;
        node = node->skl[lv].f;
      }
    }
    return rank;
  }

  size_t insert(T value)
  {
    std::fill(fspan.begin(), fspan.end(), 0U);
    auto node = _head;
    for (size_t lv = _mxlv; lv--;)
    {
      while (node->skl[lv].f && C()(node->skl[lv].f->v, value))
      {
        fspan[lv] += node->skl[lv].s;
        node = node->skl[lv].f;
      }
      front[lv] = node;
    }
    auto depth = random_depth();
    auto newnode = new_node(value, depth);
    // backward traversed
    size_t bspan = 0;
    for (size_t lv = 0; lv < depth; ++lv)
    {
      auto prev = front[lv];
      newnode->skl[lv].f = prev->skl[lv].f;
      prev->skl[lv].f = newnode;
      newnode->skl[lv].s = prev->skl[lv].s - bspan;
      prev->skl[lv].s = bspan + 1;
      bspan += fspan[lv];
    }
    for (size_t lv = depth; lv < _mxlv; ++lv)
    {
      front[lv]->skl[lv].s += 1;
      bspan += fspan[lv];
    }
    _size += 1;
    return bspan;
  }

  void merge(const IndexableSkiplist &other)
  {
    size_t new_tsize = std::max(std::max(this->target_size(), other.target_size()) - 1, this->_size + other._size);
    IndexableSkiplist newlist(new_tsize, this->_alloc);
    std::fill(newlist.front.begin(), newlist.front.end(), newlist._head);

    auto nodeL = this->_head->skl[0].f;
    auto nodeR = other._head->skl[0].f;
    while (nodeL || nodeR)
    {
      if (!nodeL)
      {
        std::swap(nodeR, nodeL);
      }
      if (nodeR && C()(nodeR->v, nodeL->v))
      {
        std::swap(nodeR, nodeL);
      }
      auto depth = newlist.random_depth();
      auto newnode = newlist.new_node(nodeL->v, depth);
      for (size_t lv = 0; lv < _mxlv; ++lv)
      {
        newlist.front[lv]->skl[lv].s += 1;
      }
      for (size_t lv = 0; lv < depth; ++lv)
      {
        newlist.front[lv]->skl[lv].f = newnode;
        newlist.front[lv] = newnode;
      }
      newlist._size += 1;
      nodeL = nodeL->skl[0].f;
    }

    // swap data only
    _swap_skiplist(*this, newlist, std::integral_constant<bool, false>());
  }

  void remove(T value)
  {
    auto node = _head;
    for (size_t lv = _mxlv; lv--;)
    {
      while (node->skl[lv].f && C()(node->skl[lv].f->v, value))
      {
        node = node->skl[lv].f;
      }
      front[lv] = node;
    }
    if ((front[0]->skl[0].f) && !C()(value, front[0]->skl[0].f->v))
    {
      remove_next(front);
    }
  }

  void remove_rank(size_t rank)
  {
    auto node = _head;
    for (size_t lv = _mxlv; lv--;)
    {
      while (node->skl[lv].f && rank >= node->skl[lv].s)
      {
        rank -= node->skl[lv].s;
        node = node->skl[lv].f;
      }
      front[lv] = node;
    }
    remove_next(front);
  }

  size_t size() const
  {
    return _size;
  }

  bool empty() const
  {
    return _size == 0;
  }

  size_t target_size() const
  {
    return 1UL << _mxlv;
  }

  // Iterator
  class Iterator
  {
    friend class IndexableSkiplist;

  public:
    using value_type = T;
    using pointer = T const *;
    using reference = const T &;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::input_iterator_tag;

    reference operator*() const
    {
      return node->v;
    }

    pointer operator->() const
    {
      return &(node->v);
    }

    Iterator &operator++()
    {
      node = node->skl[0].f;
      return *this;
    }

    Iterator operator++(int)
    {
      Iterator it(*this);
      node = node->skl[0].f;
      return it;
    }

    bool operator==(const Iterator &rhs) const
    {
      return node == rhs.node;
    }

    bool operator!=(const Iterator &rhs) const
    {
      return node != rhs.node;
    }

  private:
    node_type *node;
  };

  Iterator begin()
  {
    Iterator it;
    it.node = _head->skl[0].f;
    return it;
  }

  Iterator end()
  {
    Iterator it;
    it.node = nullptr;
    return it;
  }
};
