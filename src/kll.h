#pragma once

#include <vector>
#include <utility>
#include <cmath>
#include <algorithm>

// Karnin, Z., Lang, K., & Liberty, E. (2016, October). Optimal quantile approximation in streams. In 2016 ieee 57th annual symposium on foundations of computer science (focs) (pp. 71-78). IEEE.
class KLL
{

  int _k, _size, max_size;
  double _c;
  bool _lazy;

  std::vector<std::vector<double>> compact;

  int capacity(int lv) const
  {
    auto d = compact.size() - lv - 1;
    return static_cast<int>(std::ceil(std::pow(_c, d) * _k) + 1);
  }

  void update_size()
  {
    _size = 0;
    for (const auto &cpt : compact)
    {
      _size += cpt.size();
    }
  }

  void grow()
  {
    compact.push_back({});
    max_size = 0;
    for (auto lv = 0; lv < compact.size(); ++lv)
    {
      max_size += capacity(lv);
    }
  }

  void compress()
  {
    auto maxlv = compact.size();
    for (auto lv = 0; lv < maxlv; ++lv)
    {
      if (compact[lv].size() >= capacity(lv))
      {
        if (lv + 1 >= maxlv)
        {
          grow();
        }
        std::sort(compact[lv].begin(), compact[lv].end());
        // save last element
        auto last_pop = false;
        auto last = compact[lv].back();
        if (compact[lv].size() % 2)
        {
          last_pop = true;
          compact[lv].pop_back();
        }
        // compact to next level
        auto offset = std::rand() % 2 ? 1 : 0;
        for (auto i = offset; i < compact[lv].size(); i += 2)
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

public:
  KLL(int k, double c = 2.0 / 3.0, bool lazy = true)
      : _k(k),
        _size(0),
        max_size(0),
        _c(c),
        _lazy(lazy),
        compact{}
  {
    grow();
  }

  void insert(double x)
  {
    compact[0].push_back(x);
    if (++_size >= max_size)
    {
      compress();
    }
  }

  void merge(const KLL &rhs)
  {
    // grow to match size
    while (compact.size() < rhs.compact.size())
    {
      grow();
    }
    // concatenate compactors
    for (auto lv = 0; lv < rhs.compact.size(); ++lv)
    {
      compact[lv].insert(compact[lv].end(), rhs.compact[lv].begin(), rhs.compact[lv].end());
    }
    // compress
    update_size();
    while (_size >= max_size)
    {
      compress();
    }
  }

  int size() const
  {
    return _size;
  }

  std::pair<std::vector<double>, std::vector<double>> cdf() const
  {
    // collect values and weights
    int lv = 0;
    double cum_w, tot_w = 0.0;
    std::vector<std::pair<double, double>> weighted;
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
    std::sort(weighted.begin(), weighted.end());

    // calculate cumulative density
    std::vector<double> vals, dens;
    for (const auto &item : weighted)
    {
      cum_w += item.second;
      vals.push_back(item.first);
      dens.push_back(cum_w / tot_w);
    }

    return std::make_pair(std::move(vals), std::move(dens));
  }
};