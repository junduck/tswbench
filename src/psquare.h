#pragma once

#include <vector>    // std::vector
#include <cmath>     // NAN
#include <algorithm> // std::sort

// Jain, R., & Chlamtac, I. (1985). The P2 algorithm for dynamic calculation of quantiles and histograms without storing observations. Communications of the ACM, 28(10), 1076-1085.
class psquare
{

  bool init;
  std::vector<double> height, mpos, dpos, incr;

public:
  psquare(double p)
      : init(false),
        height(),
        mpos({1.0, 2.0, 3.0, 4.0, 5.0}),
        dpos({1.0, 1.0 + 2.0 * p, 1.0 + 4.0 * p, 3.0 + 2.0 * p, 5.0}),
        incr({0.0, p / 2.0, p, (1.0 + p) / 2.0, 1.0})

  {
  }

  psquare(double p, std::vector<double> from_state)
      : incr({0.0, p / 2.0, p, (1.0 + p) / 2.0, 1.0})
  {
    size_t height_size = from_state.size() - 10;
    init = height_size == 5;
    auto it = from_state.begin();
    height = std::vector<double>(it, it + height_size);
    it += height_size;
    mpos = std::vector<double>(it, it + 5);
    it += 5;
    dpos = std::vector<double>(it, it + 5);
  }

  std::vector<double> state() const
  {
    size_t size = height.size() + mpos.size() + dpos.size();
    std::vector<double> st;
    st.reserve(size);
    st.insert(st.end(), height.begin(), height.end());
    st.insert(st.end(), mpos.begin(), mpos.end());
    st.insert(st.end(), dpos.begin(), dpos.end());
    return st;
  }

  void insert(double x)
  {
    if (!init)
    {
      height.push_back(x);
      if (height.size() == 5)
      {
        init = true;
        std::sort(height.begin(), height.end());
      }
    }
    else
    {
      int k;
      if (x < height[0])
      {
        k = 0;
        height[0] = x;
      }
      else if (x < height[1])
      {
        k = 0;
      }
      else if (x < height[2])
      {
        k = 1;
      }
      else if (x < height[3])
      {
        k = 2;
      }
      else if (x <= height[4])
      {
        k = 3;
      }
      else
      {
        k = 3;
        height[4] = x;
      }
      for (auto i = k + 1; i < 5; ++i)
      {
        mpos[i] += 1.0;
      }
      for (auto i = 0; i < 5; ++i)
      {
        dpos[i] += incr[i];
      }
      for (auto i = 1; i < 4; ++i)
      {
        // delta desire position
        const double delta_dpos = dpos[i] - mpos[i];
        if ((delta_dpos >= 1.0 && mpos[i + 1] - mpos[i] > 1.0) || (delta_dpos <= -1.0 && mpos[i - 1] - mpos[i] < -1.0))
        {
          const double sgn = delta_dpos < 0 ? -1.0 : 1.0;
          const int isgn = static_cast<int>(sgn);
          // try parabolic
          auto t1 = sgn / (mpos[i + 1] - mpos[i - 1]);
          auto t2 = (mpos[i] - mpos[i - 1] + sgn) * (height[i + 1] - height[i]) / (mpos[i + 1] - mpos[i]);
          auto t3 = (mpos[i + 1] - mpos[i] - sgn) * (height[i] - height[i - 1]) / (mpos[i] - mpos[i - 1]);
          auto adj = height[i] + t1 * (t2 + t3);
          if (height[i - 1] < adj && height[i + 1] > adj)
          {
            height[i] = adj;
          }
          else
          {
            // linear
            height[i] += sgn * (height[i + isgn] - height[i]) / (mpos[i + isgn] - mpos[i]);
          }
          // update pos
          mpos[i] += sgn;
        }
      }
    }
  }

  double value() const
  {
    if (height.size() > 2)
    {
      return height[2];
    }
    else
    {
      return NAN;
    }
  }
};
