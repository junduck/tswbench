#pragma once

#include <vector>     // std::vector
#include <cmath>      // std::ceil, std::pow
#include <functional> // std::less
#include <algorithm>  // std::sort
#include <utility>    // std::pair
#include <numeric>    // std::partial_sum

// Karnin, Z., Lang, K., & Liberty, E. (2016, October). Optimal quantile approximation in streams. In 2016 ieee 57th annual symposium on foundations of computer science (focs) (pp. 71-78). IEEE.
template <class T, class C = std::less<T>>
class KLL
{

  size_t _k, _size, _max_size;
  double _c;
  bool _lazy;

  std::vector<std::vector<T>> compact;

  size_t capacity(size_t lv) const
  {
    auto d = compact.size() - lv - 1;
    return static_cast<size_t>(std::ceil(std::pow(_c, d) * _k) + 1);
  }

  void update_size()
  {
    _size = 0;
    for (const auto &cpt : compact)
    {
      _size += cpt.size();
    }
  }

  void update_max_size()
  {
    _max_size = 0;
    for (size_t lv = 0; lv < compact.size(); ++lv)
    {
      _max_size += capacity(lv);
    }
  }

  void grow()
  {
    compact.push_back({});
    update_max_size();
  }

  void compress()
  {
    auto maxlv = compact.size();
    for (size_t lv = 0; lv < maxlv; ++lv)
    {
      if (compact[lv].size() >= capacity(lv))
      {
        if (lv + 1 == maxlv)
        {
          grow();
        }
        std::sort(compact[lv].begin(), compact[lv].end(), C());
        // save last element
        auto last_pop = false;
        auto last = compact[lv].back();
        if (compact[lv].size() % 2)
        {
          last_pop = true;
          compact[lv].pop_back();
        }
        // compact to next level
        size_t offset = std::rand() % 2 ? 1 : 0;
        for (size_t i = offset; i < compact[lv].size(); i += 2)
        {
          compact[lv + 1].push_back(compact[lv][i]);
        }
        compact[lv].clear();
        // recover last element if popped
        if (last_pop)
        {
          compact[lv].push_back(last);
        }
        // update size
        update_size();
        if (_lazy)
        {
          break;
        }
      }
    }
  }

  static size_t bin_search_vdbl(const std::vector<double> &v, double x, size_t l, size_t r)
  {
    size_t m;
    while (l <= r)
    {
      m = l + (r - l) / 2;
      if (v[m] == x)
      {
        break;
      }
      else if (v[m] < x)
      {
        l = m + 1;
      }
      else
      {
        r = m - 1;
      }
    }
    return m;
  }

public:
  KLL(size_t k, double c = 2.0 / 3.0, bool lazy = true)
      : _k(k),
        _size(0),
        _max_size(0),
        _c(c),
        _lazy(lazy),
        compact({})
  {
    grow();
  }

  KLL(size_t k, double c, bool lazy, std::pair<std::vector<size_t>, std::vector<T>> from_state)
      : _k(k),
        _c(c),
        _lazy(lazy)
  {
    size_t cp_lv = from_state.first.size();
    compact.reserve(cp_lv);
    auto st_start = from_state.second.begin();
    for (size_t i = 0; i < cp_lv; ++i) {
      compact.emplace_back(std::vector<T>(st_start, st_start + from_state.first[i]));
      st_start += from_state.first[i];
    }
    update_size();
    update_max_size();
  }

  std::pair<std::vector<size_t>, std::vector<T>> state() const {
    std::vector<size_t> cp_lv;
    cp_lv.reserve(compact.size());
    std::vector<T> cp;
    cp.reserve(_size);
    for (const auto &cpt : compact) {
      cp_lv.push_back(cpt.size());
      cp.insert(cp.end(), cpt.begin(), cpt.end());
    }
    return std::make_pair(std::move(cp_lv), std::move(cp));
  }

  void insert(T x)
  {
    compact[0].push_back(x);
    if (++_size >= _max_size)
    {
      compress();
    }
  }

  void merge(const KLL &rhs)
  {
    // grow to match size, _max_size is updated
    while (compact.size() < rhs.compact.size())
    {
      grow();
    }
    // concatenate compactors
    for (size_t lv = 0; lv < rhs.compact.size(); ++lv)
    {
      compact[lv].insert(compact[lv].end(), rhs.compact[lv].begin(), rhs.compact[lv].end());
    }
    // compress, _size is updated
    update_size();
    while (_size >= _max_size)
    {
      compress();
    }
  }

  size_t size() const
  {
    return _size;
  }

  std::pair<std::vector<T>, std::vector<double>> pmf() const
  {
    // collect values and weights
    size_t lv = 0;
    double cum_w, tot_w = 0.0;
    std::vector<std::pair<T, double>> weighted;
    for (const auto &cpt : compact)
    {
      const double w = 1 << lv;
      for (const auto &v : cpt)
      {
        weighted.emplace_back(std::make_pair(v, w));
        tot_w += w;
      }
      ++lv;
    }
    std::sort(weighted.begin(), weighted.end(), [](const auto &lhs, const auto &rhs) {
      return C()(lhs.first, rhs.first);
    });

    // prob mass distr
    std::vector<T> vals;
    std::vector<double> dens;
    for (const auto &item : weighted)
    {
      vals.push_back(item.first);
      dens.push_back(item.second / tot_w);
    }

    return std::make_pair(std::move(vals), std::move(dens));
  }

  std::pair<std::vector<T>, std::vector<double>> cdf() const
  {
    auto p = pmf();
    std::partial_sum(p.second.begin(), p.second.end(), p.second.begin());
    return p;
  }

  std::vector<T> quantile(const std::vector<double> &probs) const
  {
    // quantiles
    std::vector<T> q;
    q.reserve(probs.size());

    auto qvals = cdf();
    size_t len = qvals.first.size();
    for (const auto &p : probs)
    {
      const auto idx = bin_search_vdbl(qvals.second, p, 0, len - 1);
      q.push_back(qvals.first[idx]);
    }
    return q;
  }
};
