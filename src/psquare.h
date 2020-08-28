#pragma once

#include <cmath>
#include <vector>
#include <algorithm>

// Jain, R., & Chlamtac, I. (1985). The P2 algorithm for dynamic calculation of quantiles and histograms without storing observations. Communications of the ACM, 28(10), 1076-1085.
class psquare
{

  bool init;
  std::vector<double> height, pos, dpos, incr;

public:
  psquare(double p) : init(false),
                      height(),
                      pos({1.0, 2.0, 3.0, 4.0, 5.0}),
                      dpos({1.0, 1.0 + 2.0 * p, 1.0 + 4.0 * p, 3.0 + 2.0 * p, 5.0}),
                      incr({0.0, p / 2.0, p, (1.0 + p) / 2.0, 1.0})

  {
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
      else if (x < height[4])
      {
        k = 3;
      }
      else
      {
        k = 4;
        height[4] = x;
      }
      for (auto i = k; i < 5; ++i)
      {
        pos[i] += 1.0;
      }
      for (auto i = 0; i < 5; ++i)
      {
        dpos[i] += incr[i];
      }
      for (auto i = 1; i < 4; ++i)
      {
        // delta desire/position
        auto delta_dpos = dpos[i]    - pos[i];
        auto delta_pos  = pos[i + 1] - pos[i];
        // sign
        double sgn = 0.0;
        int isgn = 0;
        if ((delta_dpos >= 1.0 && delta_pos > 1.0) || (delta_dpos <= -1.0 && delta_pos < -1.0))
        {
          if (delta_dpos < 0)
          {
            sgn = -1.0;
            isgn = -1;
          }
          else
          {
            sgn = 1.0;
            isgn = 1;
          }
          // try parabolic
          auto t1 = sgn / (pos[i + 1] - pos[i - 1]);
          auto t2 = (pos[i]     - pos[i - 1] + sgn) * (height[i + 1] - height[i])     / (pos[i + 1] - pos[i]);
          auto t3 = (pos[i + 1] - pos[i]     - sgn) * (height[i]     - height[i - 1]) / (pos[i]     - pos[i - 1]);
          auto adj = height[i] + t1 * (t2 + t3);
          if (height[i - 1] < adj && height[i + 1] > adj)
          {
            height[i] = adj;
          }
          else
          {
            // linear
            height[i] += sgn / (pos[i + isgn] - pos[i]) * (height[i + isgn] - height[i]) ;
          }
          // update pos
          pos[i] += sgn;
        }
      }
    }
  }

  double value()
  {
    if (height.size() > 2) {
      return height[2];
    } else {
      return NAN;
    }
  }
};
