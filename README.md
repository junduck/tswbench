Basic usage
-----------

tswbench provides quick access to Tushare Pro
(<a href="https://tushare.pro/" class="uri">https://tushare.pro/</a>)
data. In order to query data from Tushare, simply create an tsapi object
and access Tushare api functions using “$”.

``` r
# set token first
# SetToken("YOUR TUSHARE TOKEN")
api <- TushareApi()

# argument formats are same as official documents
eod <- api$daily(trade_date = "20200701")
eod[]
```

    ##         ts_code trade_date  open  high   low close pre_close change
    ##    1: 000001.SZ 2020-07-01 12.79 13.15 12.74 13.12     12.80   0.32
    ##    2: 000002.SZ 2020-07-01 26.18 28.50 26.17 28.09     26.14   1.95
    ##    3: 000004.SZ 2020-07-01 29.80 30.82 29.60 30.20     29.84   0.36
    ##    4: 000005.SZ 2020-07-01  2.63  2.69  2.62  2.67      2.63   0.04
    ##    5: 000006.SZ 2020-07-01  6.69  7.49  6.66  7.49      6.81   0.68
    ##   ---                                                              
    ## 3842: 688558.SH 2020-07-01 44.37 45.14 42.88 44.66     44.30   0.36
    ## 3843: 688566.SH 2020-07-01 45.30 46.25 44.11 44.93     45.50  -0.57
    ## 3844: 688588.SH 2020-07-01 45.60 45.86 43.52 44.26     44.97  -0.71
    ## 3845: 688598.SH 2020-07-01 92.56 94.50 88.91 90.65     92.35  -1.70
    ## 3846: 688599.SH 2020-07-01 17.53 17.70 16.52 16.84     16.80   0.04
    ##       pct_chg        vol     amount
    ##    1:  2.5000 1697390.01 2202800.84
    ##    2:  7.4598 2602748.54 7124087.37
    ##    3:  1.2064   56386.06  170215.40
    ##    4:  1.5209   69514.04   18485.44
    ##    5:  9.9853 1057800.82  777350.08
    ##   ---                              
    ## 3842:  0.8126  136725.92  599341.90
    ## 3843: -1.2527   46786.19  211711.52
    ## 3844: -1.5788   58240.48  260164.85
    ## 3845: -1.8408   26758.46  242859.04
    ## 3846:  0.2381  473675.89  806802.19

API function usage
------------------

You can get help from API usage functions. To query all supported
functions:

``` r
tsapi_func()
```

    ##   [1] "adj_factor"         "anns"               "balancesheet"      
    ##   [4] "block_trade"        "bo_cinema"          "bo_daily"          
    ##   [7] "bo_monthly"         "bo_weekly"          "cashflow"          
    ##  [10] "cb_basic"           "cb_daily"           "cb_issue"          
    ##  [13] "cctv_news"          "cn_cpi"             "cn_gdp"            
    ##  [16] "cn_m"               "concept_detail"     "concept"           
    ##  [19] "daily_basic"        "daily_info"         "daily"             
    ##  [22] "disclosure_date"    "dividend"           "eco_cal"           
    ##  [25] "express"            "film_record"        "fina_audit"        
    ##  [28] "fina_indicator"     "fina_mainbz"        "forecast"          
    ##  [31] "ft_tick"            "fund_adj"           "fund_basic"        
    ##  [34] "fund_company"       "fund_daily"         "fund_div"          
    ##  [37] "fund_manager"       "fund_nav"           "fund_portfolio"    
    ##  [40] "fund_share"         "fut_basic"          "fut_daily"         
    ##  [43] "fut_holding"        "fut_mapping"        "fut_settle"        
    ##  [46] "fut_weekly_detail"  "fut_wsr"            "fx_daily"          
    ##  [49] "fx_obasic"          "ggt_daily"          "ggt_monthly"       
    ##  [52] "ggt_top10"          "gz_index"           "hibor"             
    ##  [55] "hk_basic"           "hk_daily"           "hk_hold"           
    ##  [58] "hs_const"           "hsgt_top10"         "income"            
    ##  [61] "index_basic"        "index_classify"     "index_daily"       
    ##  [64] "index_dailybasic"   "index_global"       "index_member"      
    ##  [67] "index_monthly"      "index_weekly"       "index_weight"      
    ##  [70] "libor"              "limit_list"         "major_news"        
    ##  [73] "margin_detail"      "margin"             "moneyflow_hsgt"    
    ##  [76] "moneyflow"          "monthly"            "namechange"        
    ##  [79] "ncov_global"        "ncov_num"           "new_share"         
    ##  [82] "news"               "opt_basic"          "opt_daily"         
    ##  [85] "pledge_detail"      "pledge_stat"        "repurchase"        
    ##  [88] "share_float"        "shibor_lpr"         "shibor_quote"      
    ##  [91] "stk_account"        "stk_holdernumber"   "stk_holdertrade"   
    ##  [94] "stk_limit"          "stk_managers"       "stk_rewards"       
    ##  [97] "stock_basic"        "stock_company"      "suspend_d"         
    ## [100] "teleplay_record"    "tmt_twincome"       "tmt_twincomedetail"
    ## [103] "top_inst"           "top_list"           "top10_floatholders"
    ## [106] "top10_holders"      "trade_cal"          "us_tbr"            
    ## [109] "us_tltr"            "us_trltr"           "us_trycr"          
    ## [112] "us_tycr"            "weekly"             "wz_index"

Find specific funcion usage:

``` r
tsapi_usage("daily", what = "arg")
```

    ##           arg dtype mandatory                                       desc
    ## 1:    ts_code   str     FALSE 股票代码（支持多个股票同时提取，逗号分隔）
    ## 2: trade_date   str     FALSE                       交易日期（YYYYMMDD）
    ## 3: start_date   str     FALSE                         开始日期(YYYYMMDD)
    ## 4:   end_date   str     FALSE                         结束日期(YYYYMMDD)

``` r
tsapi_usage("daily", what = "field")
```

    ##          field dtype default
    ##  1:    ts_code   str    TRUE
    ##  2: trade_date   str    TRUE
    ##  3:       open float    TRUE
    ##  4:       high float    TRUE
    ##  5:        low float    TRUE
    ##  6:      close float    TRUE
    ##  7:  pre_close float    TRUE
    ##  8:     change float    TRUE
    ##  9:    pct_chg float    TRUE
    ## 10:        vol float    TRUE
    ## 11:     amount float    TRUE
    ##                                                desc
    ##  1:                                        股票代码
    ##  2:                                        交易日期
    ##  3:                                          开盘价
    ##  4:                                          最高价
    ##  5:                                          最低价
    ##  6:                                          收盘价
    ##  7:                                          昨收价
    ##  8:                                          涨跌额
    ##  9: 涨跌幅 （未复权，如果是复权请用 通用行情接口 ）
    ## 10:                                   成交量 （手）
    ## 11:                                 成交额 （千元）

End-of-day data
---------------

tswbench wraps various end-of-day data functions to market\_eod() to
simplify data acquisition. market\_eod() supports most of Tushare APIs.

### Query end-of-day intraday OHLC:

Intraday data is not officially documented. However, data can be queried
by special “intraday” func.

``` r
ohlc <- market_eod(func = "intraday", date = "20200701", freq = 60)
ohlc[]
```

    ##          ts_code          trade_time     open    close     high      low
    ##     1: 000001.SH 2020-07-01 09:30:00 2991.181 2991.181 2991.181 2991.181
    ##     2: 000001.SH 2020-07-01 10:30:00 2991.033 2997.764 2999.245 2985.122
    ##     3: 000001.SH 2020-07-01 11:30:00 2997.550 3011.772 3011.795 2996.562
    ##     4: 000001.SH 2020-07-01 14:00:00 3011.772 3004.120 3019.532 2995.487
    ##     5: 000001.SH 2020-07-01 15:00:00 3004.574 3025.981 3026.186 3000.736
    ##    ---                                                                  
    ## 26336: 980023.SZ 2020-07-01 09:30:00 2869.496 2869.496 2869.496 2869.496
    ## 26337: 980023.SZ 2020-07-01 10:30:00 2868.555 2862.471 2871.374 2859.839
    ## 26338: 980023.SZ 2020-07-01 11:30:00 2862.713 2868.214 2870.863 2859.811
    ## 26339: 980023.SZ 2020-07-01 14:00:00 2868.300 2851.340 2869.975 2842.801
    ## 26340: 980023.SZ 2020-07-01 15:00:00 2851.257 2869.548 2869.548 2849.669
    ##               vol       amount
    ##     1:  205423300   2651727275
    ##     2: 9587979700 135104030933
    ##     3: 5268883500  69928252328
    ##     4: 6585416500  87287000947
    ##     5: 5683611000  74912790814
    ##    ---                        
    ## 26336:    3829659     80215949
    ## 26337:  349502239   5926241661
    ## 26338:  221219267   3727458840
    ## 26339:  334077494   6014484837
    ## 26340:  217022787   4081421046

### Query end-of-day index performance:

``` r
index <- market_eod(func = "index_daily", date = "20200701")
index[]
```

    ##       ts_code trade_date     close      open      high       low pre_close
    ##  1: 000001.SH 2020-07-01  3025.981  2991.181  3026.186  2984.984  2984.674
    ##  2: 000005.SH 2020-07-01  3031.658  3017.223  3040.640  3000.894  3015.678
    ##  3: 000006.SH 2020-07-01  6499.924  6223.192  6569.703  6204.929  6215.632
    ##  4: 000016.SH 2020-07-01  3009.861  2950.492  3010.362  2941.380  2942.074
    ##  5: 000300.SH 2020-07-01  4247.783  4172.642  4247.783  4163.495  4163.964
    ##  6: 000905.SH 2020-07-01  5881.887  5884.540  5885.874  5814.755  5864.416
    ##  7: 399001.SZ 2020-07-01 12112.965 12024.840 12130.302 11945.892 11992.347
    ##  8: 399005.SZ 2020-07-01  8095.735  8041.640  8095.735  7965.207  8015.505
    ##  9: 399006.SZ 2020-07-01  2419.629  2449.865  2450.842  2381.593  2438.197
    ## 10: 399016.SZ 2020-07-01  3888.551  3905.802  3907.847  3826.733  3890.729
    ## 11: 399300.SZ 2020-07-01  4247.783  4172.642  4247.783  4163.495  4163.964
    ## 12: 399905.SZ 2020-07-01  5881.887  5884.540  5885.874  5814.755  5864.416
    ##       change pct_chg       vol    amount
    ##  1:  41.3069  1.3840 273313140 369883802
    ##  2:  15.9797  0.5299  30638848  40450024
    ##  3: 284.2918  4.5738   9868412   9326447
    ##  4:  67.7870  2.3041  42668069  83084456
    ##  5:  83.8198  2.0130 169990456 305711800
    ##  6:  17.4711  0.2979 142832982 161437632
    ##  7: 120.6177  1.0058 393967637 537386447
    ##  8:  80.2300  1.0009 179746268 232923070
    ##  9: -18.5673 -0.7615 102164492 179376368
    ## 10:  -2.1775 -0.0560  28310693  93054017
    ## 11:  83.8198  2.0130 169990456 305711800
    ## 12:  17.4711  0.2979 142832982 161437632

Fundamental data
----------------

tswbench also wraps various fundamental reports to report\_market() to
query whole market per year and quarter, and report\_quarter() to query
quaterly reports per ts\_code. These functions handle update\_flag and
always return latest updated data.

``` r
income2020q1 <- report_market(type = "income", y = 2020, q = 1)
income2020q1[]
```

    ##         ts_code   end_date   ann_date f_ann_date report_type comp_type
    ##    1: 000001.SZ 2020-03-31 2020-04-21 2020-04-21           1         2
    ##    2: 000002.SZ 2020-03-31 2020-04-28 2020-04-28           1         1
    ##    3: 000004.SZ 2020-03-31 2020-04-28 2020-04-28           1         1
    ##    4: 000005.SZ 2020-03-31 2020-04-30 2020-04-30           1         1
    ##    5: 000006.SZ 2020-03-31 2020-04-30 2020-04-30           1         1
    ##   ---                                                                 
    ## 3785: 688396.SH 2020-03-31 2020-04-23 2020-04-23           1         1
    ## 3786: 688398.SH 2020-03-31 2020-04-29 2020-04-29           1         1
    ## 3787: 688399.SH 2020-03-31 2020-04-24 2020-04-24           1         1
    ## 3788: 688466.SH 2020-03-31 2020-05-07 2020-05-07           1         1
    ## 3789: 688588.SH 2020-03-31 2020-05-08 2020-05-08           1         1
    ##       basic_eps diluted_eps total_revenue     revenue int_income
    ##    1:    0.4000      0.4000   37926000000 37926000000 4.7877e+10
    ##    2:    0.1110      0.1110   47774342785 47774342785         NA
    ##    3:    0.0231      0.0231      23253019    23253019         NA
    ##    4:   -0.0198     -0.0198      45702946    45702946         NA
    ##    5:    0.0948      0.0948     652209471   652209471         NA
    ##   ---                                                           
    ## 3785:    0.0935      0.0935    1382423958  1382423958         NA
    ## 3786:    0.4000      0.4000      88765034    88765034         NA
    ## 3787:    1.7700      1.7700     200370471   200370471         NA
    ## 3788:    0.0500          NA      57357651    57357651         NA
    ## 3789:    0.0933      0.0933     136176287   136176287         NA
    ##       prem_earned comm_income n_commis_income n_oth_income n_oth_b_income
    ##    1:          NA  1.1581e+10       9.688e+09    3.468e+09        2.5e+07
    ##    2:          NA          NA              NA           NA             NA
    ##    3:          NA          NA              NA           NA             NA
    ##    4:          NA          NA              NA           NA             NA
    ##    5:          NA          NA              NA           NA             NA
    ##   ---                                                                    
    ## 3785:          NA          NA              NA           NA             NA
    ## 3786:          NA          NA              NA           NA             NA
    ## 3787:          NA          NA              NA           NA             NA
    ## 3788:          NA          NA              NA           NA             NA
    ## 3789:          NA          NA              NA           NA             NA
    ##       prem_income out_prem une_prem_reser reins_income n_sec_tb_income
    ##    1:          NA       NA             NA           NA              NA
    ##    2:          NA       NA             NA           NA              NA
    ##    3:          NA       NA             NA           NA              NA
    ##    4:          NA       NA             NA           NA              NA
    ##    5:          NA       NA             NA           NA              NA
    ##   ---                                                                 
    ## 3785:          NA       NA             NA           NA              NA
    ## 3786:          NA       NA             NA           NA              NA
    ## 3787:          NA       NA             NA           NA              NA
    ## 3788:          NA       NA             NA           NA              NA
    ## 3789:          NA       NA             NA           NA              NA
    ##       n_sec_uw_income n_asset_mg_income oth_b_income fv_value_chg_gain
    ##    1:              NA                NA      2.5e+07     -949000000.00
    ##    2:              NA                NA           NA        7139004.66
    ##    3:              NA                NA           NA                NA
    ##    4:              NA                NA           NA                NA
    ##    5:              NA                NA           NA        1854345.04
    ##   ---                                                                 
    ## 3785:              NA                NA           NA          28116.77
    ## 3786:              NA                NA           NA         -83155.00
    ## 3787:              NA                NA           NA        5966314.35
    ## 3788:              NA                NA           NA                NA
    ## 3789:              NA                NA           NA                NA
    ##       invest_income ass_invest_income forex_gain  total_cogs   oper_cost
    ##    1:  3802000000.0                NA    5.1e+08 26928000000          NA
    ##    2:   109588131.7      -212546872.2         NA 43070811323 32832719447
    ##    3:            NA                NA         NA    19466633     3936257
    ##    4:    -2564101.9                NA         NA    66751180    31296351
    ##    5:    16968669.9         7779680.5         NA   478073174   340353211
    ##   ---                                                                   
    ## 3785:      799847.8          780911.8         NA  1284442276  1037943794
    ## 3786:      889136.5                NA         NA    72324823    57151951
    ## 3787:            NA                NA         NA    89262535    23819608
    ## 3788:            NA                NA         NA    52126142    39575020
    ## 3789:      128547.0         -143337.7         NA   105591600    79270471
    ##          int_exp  comm_exp biz_tax_surchg   sell_exp   admin_exp
    ##    1: 2.3107e+10 1.893e+09   4.020000e+08         NA 10598000000
    ##    2:         NA        NA   4.148904e+09 1241137605  2490235508
    ##    3:         NA        NA   5.229339e+04    6316247     3463214
    ##    4:         NA        NA   6.324942e+05    2477821    18637852
    ##    5:         NA        NA   7.348830e+07    3338143    24746161
    ##   ---                                                           
    ## 3785:         NA        NA   1.514715e+07   23682508    86958020
    ## 3786:         NA        NA   1.144182e+06    5497871     5085657
    ## 3787:         NA        NA   9.668708e+05   40708316    10843608
    ## 3788:         NA        NA   1.107816e+05    2035366     8468971
    ## 3789:         NA        NA   5.854142e+05    7065715    10680880
    ##            fin_exp assets_impair_loss prem_refund compens_payout
    ##    1:           NA                 NA          NA             NA
    ##    2: 2259880934.5                 NA          NA             NA
    ##    3:    -176531.5                 NA          NA             NA
    ##    4:    6369405.0                 NA          NA             NA
    ##    5:   34438048.4                 NA          NA             NA
    ##   ---                                                           
    ## 3785:    7264054.9        -5602327.86          NA             NA
    ## 3786:   -1351735.5          -55368.80          NA             NA
    ## 3787:    -926052.5          -10979.72          NA             NA
    ## 3788:     141102.8                 NA          NA             NA
    ## 3789:   -5079899.3                 NA          NA             NA
    ##       reser_insur_liab div_payt reins_exp   oper_exp compens_payout_refu
    ##    1:               NA       NA        NA 2.6928e+10                  NA
    ##    2:               NA       NA        NA         NA                  NA
    ##    3:               NA       NA        NA         NA                  NA
    ##    4:               NA       NA        NA         NA                  NA
    ##    5:               NA       NA        NA         NA                  NA
    ##   ---                                                                   
    ## 3785:               NA       NA        NA         NA                  NA
    ## 3786:               NA       NA        NA         NA                  NA
    ## 3787:               NA       NA        NA         NA                  NA
    ## 3788:               NA       NA        NA         NA                  NA
    ## 3789:               NA       NA        NA         NA                  NA
    ##       insur_reser_refu reins_cost_refund other_bus_cost operate_profit
    ##    1:               NA                NA             NA    10998000000
    ##    2:               NA                NA             NA     4823237263
    ##    3:               NA                NA             NA        4435078
    ##    4:               NA                NA             NA      -23370995
    ##    5:               NA                NA             NA      192954007
    ##   ---                                                                 
    ## 3785:               NA                NA             NA      121485182
    ## 3786:               NA                NA             NA       31363687
    ## 3787:               NA                NA             NA      122043516
    ## 3788:               NA                NA             NA        5240110
    ## 3789:               NA                NA             NA       37437570
    ##       non_oper_income non_oper_exp nca_disploss total_profit   income_tax
    ##    1:     18000000.00  58000000.00           NA  10958000000 2.410000e+09
    ##    2:    187303357.69 214861413.06           NA   4795679208 2.365404e+09
    ##    3:         4406.93     35513.65           NA      4403971 8.286899e+05
    ##    4:      1079102.43      9951.81           NA    -22301845 5.615115e+04
    ##    5:        60610.50  10070601.93           NA    182944015 4.233467e+07
    ##   ---                                                                    
    ## 3785:      7297343.95    207732.27           NA    128574793 2.964807e+06
    ## 3786:         2256.17    288453.07           NA     31077490 4.616060e+06
    ## 3787:            5.01    281260.25           NA    121762260 1.817131e+07
    ## 3788:        75216.61    200000.00           NA      5115327 1.556762e+06
    ## 3789:         5851.70      6172.22           NA     37437250 4.012597e+06
    ##         n_income n_income_attr_p minority_gain oth_compr_income
    ##    1: 8548000000      8548000000            NA    1702000000.00
    ##    2: 2430274853      1249359313  1180915539.9    -586446019.93
    ##    3:    3575281         3816023     -240742.6               NA
    ##    4:  -22357996       -20988032    -1369963.2               NA
    ##    5:  140609341       127952312    12657028.9               NA
    ##   ---                                                          
    ## 3785:  125609986       113658557    11951429.0      15485071.14
    ## 3786:   26461430        26461430            NA               NA
    ## 3787:  103590955       103590955            NA               NA
    ## 3788:    3558565         3798730     -240165.8        -21912.60
    ## 3789:   33424653        33596201     -171548.7         77465.71
    ##       t_compr_income compr_inc_attr_p compr_inc_attr_m_s       ebit ebitda
    ##    1:    10250000000      10250000000                 NA         NA     NA
    ##    2:     1843828833        650085687       1193743146.0 6969842963     NA
    ##    3:        3575281          3816023          -240742.6    4258546     NA
    ##    4:      -22357996        -20988032         -1369963.2  -14437488     NA
    ##    5:      140609341        127952312         12657028.9  210283653     NA
    ##   ---                                                                     
    ## 3785:      141095057        129143629         11951429.0  134577469     NA
    ## 3786:       26461430         26461430                 NA   28937696     NA
    ## 3787:      103590955        103590955                 NA  116813180     NA
    ## 3788:        3536652          3776818          -240165.8         NA     NA
    ## 3789:       33502118         33673667          -171548.7         NA     NA
    ##       insurance_exp undist_profit distable_profit update_flag
    ##    1:            NA            NA              NA        TRUE
    ##    2:            NA            NA              NA        TRUE
    ##    3:            NA            NA              NA        TRUE
    ##    4:            NA            NA              NA        TRUE
    ##    5:            NA            NA              NA        TRUE
    ##   ---                                                        
    ## 3785:            NA            NA              NA        TRUE
    ## 3786:            NA            NA              NA        TRUE
    ## 3787:            NA            NA              NA        TRUE
    ## 3788:            NA            NA              NA       FALSE
    ## 3789:            NA            NA              NA       FALSE

Standardise A-share symbols
---------------------------

Different vendors may use different coding schemes for A-share traded
securities. These symbols can be standardised to code and traded
exchange by parse\_ashare\_code(). Or convert to Tushare standard by
norm\_ashare\_code().

``` r
ts_code <- norm_ashare_code(code = c("000001.sz", "sh600000", "601989"), type = "stock")
ts_code
```

    ## [1] "000001.SZ" "600000.SH" "601989.SH"

Realtime data
-------------

Realtime data provided by Tushare Pro is not officially documented.
Experimental support is provided by tushare\_realtime\_websocket(). Data
handling is provided by user defined callback function, which should
accept at least three arguments: - topic : a character vector of length
1 - code: a character vector of length 1 - record: a character vector of
various lengths depending on the topic

Here is an example simple prints received data to console:

``` r
# define a callback function to handle realtime data
# the call back function should accept at least three arguments:
#   

simple_callback <- function(topic, code, record, ...) {
  cat("topic:  ", topic, "\n")
  cat("code:   ", code, "\n")
  cat("record: ")
  str(record)

  TRUE
}

# connect to Tushare service
ws <- tushare_realtime_websocket(topic = "some_topic", code = "000001.SZ", callback = simple_callback)
ws$connect()
```

Third party realtime data vendors
---------------------------------

Realtime data provided by other vendors are provided, namely by Sina and
Tencent. Data can be easily fetched calling sina\_realtime\_quote(),
tencent\_realtime\_quote() and tencent\_realtime\_moneyflow().

An example of querying all stocks traded from Sina:

``` r
# fetch all traded stocks from Tushare:
api <- TushareApi()
stocks <- api$stock_basic()
# convert ts_code to Sina format
codes <- sina_ashare_code(stocks$ts_code)
# query from Sina
quotes <- sina_realtime_quote(codes)
quotes[]
```

    ##           Name  Open PreClose Price  High   Low   Bid   Ask       Vol
    ##    1: 平安银行 13.57    13.43 14.25 14.32 13.56 14.25 14.26 376833363
    ##    2:  万 科Ａ 28.65    28.39 28.65 29.12 28.14 28.65 28.66 174435560
    ##    3: 国农科技 29.45    29.45 28.46 29.49 28.38 28.46 28.47   5627413
    ##    4: 世纪星源  2.69     2.69  2.72  2.74  2.69  2.71  2.72   9756879
    ##    5: 深振业Ａ  7.48     7.48  7.51  7.65  7.26  7.51  7.52  88168721
    ##   ---                                                                
    ## 3866:   吉贝尔 44.30    44.34 44.95 45.24 44.11 44.94 44.95   4073785
    ## 3867: 凌志软件 45.90    45.81 48.07 50.20 45.15 48.06 48.07  13103432
    ## 3868: 金博股份 90.86    89.99 90.42 92.30 87.98 90.41 90.42   2131819
    ## 3869: 天合光能 17.08    17.07 16.81 17.09 16.70 16.81 16.82  39427041
    ## 3870:    N皖仪 35.50    15.50 35.90 40.00 35.20 35.90 35.93  21816313
    ##             Tnvr Bid_V1 Bid_P1 Bid_V2 Bid_P2  Bid_V3 Bid_P3 Bid_V4 Bid_P4
    ##    1: 5280918011   6224  14.25 598900  14.24 1038802  14.23 231500  14.22
    ##    2: 4993062505 562333  28.65  22800  28.64  334300  28.63 133500  28.62
    ##    3:  161436906  34900  28.46  18400  28.45   20300  28.44  30500  28.43
    ##    4:   26440407 217705   2.71 330100   2.70  395300   2.69 168000   2.68
    ##    5:  656871650  79640   7.51 363500   7.50   57300   7.49  54600   7.48
    ##   ---                                                                    
    ## 3866:  182096786  12322  44.94   7200  44.93    5779  44.92   3636  44.90
    ## 3867:  625339824    600  48.06    200  48.05     400  48.04   5910  48.03
    ## 3868:  192086026    800  90.41    700  90.40    2725  90.38    700  90.36
    ## 3869:  664759874 223678  16.81 378826  16.80   71982  16.79  86283  16.78
    ## 3870:  797248900  69870  35.90   1000  35.89    8556  35.88    200  35.87
    ##       Bid_V5 Bid_P5 Ask_V1 Ask_P1 Ask_V2 Ask_P2 Ask_V3 Ask_P3 Ask_V4
    ##    1: 238500  14.21 390914  14.26 221833  14.27 693567  14.28 390920
    ##    2: 448000  28.61  16200  28.66    600  28.67   7800  28.68    100
    ##    3:   4600  28.42   4300  28.47   1800  28.49   4800  28.50   2500
    ##    4:  75900   2.67 416238   2.72 359200   2.73 479400   2.74 467530
    ##    5:  14000   7.47  72982   7.52  81400   7.53  91500   7.54 143835
    ##   ---                                                               
    ## 3866:   1892  44.89  25496  44.95  12336  44.96   3966  44.97   9783
    ## 3867:   5130  48.02  46506  48.07   2974  48.08  12012  48.09  21606
    ## 3868:    200  90.35    450  90.42   3000  90.45   1764  90.51    900
    ## 3869:  23359  16.77  47759  16.82  24499  16.83  41278  16.84  40038
    ## 3870:    300  35.86    500  35.93   2650  35.94   6707  35.95   2100
    ##       Ask_P4  Ask_V5 Ask_P5                Time sina_code
    ##    1:  14.29 3707952  14.30 2020-07-03 15:00:03  sz000001
    ##    2:  28.69   36200  28.70 2020-07-03 15:00:03  sz000002
    ##    3:  28.51     300  28.53 2020-07-03 15:00:03  sz000004
    ##    4:   2.75  248300   2.76 2020-07-03 15:00:03  sz000005
    ##    5:   7.55  160554   7.56 2020-07-03 15:00:03  sz000006
    ##   ---                                                    
    ## 3866:  44.98    1923  44.99 2020-07-03 15:29:59  sh688566
    ## 3867:  48.10    4356  48.11 2020-07-03 15:29:59  sh688588
    ## 3868:  90.54     200  90.57 2020-07-03 15:29:59  sh688598
    ## 3869:  16.85   23301  16.86 2020-07-03 15:29:59  sh688599
    ## 3870:  35.96    7885  35.97 2020-07-03 15:29:59  sh688600

### Keeping track of Sina realtime data

A simple incremental model is created to record and track Sina realtime
data. Due to the single-thread nature of R, we need to query Sina data
in a separate R process:

``` r
# define an sqlite database file
db <- "/path/to/data.db"
# run data query loop
sina_realtime_loop(db)
```

On our working R process, a incremental data loader can be created and
used to fetch newly arrived data:

``` r
db <- "/path/to/data.db"
ld <- sina_realtime_loader(db)

# get new data
quotes <- ld()
# consume data
quotes[, spread := Bid - Ask]
summary(quotes$spread)
# do other thins with quotes

# get updated data
quotes <- ld()
```

Please notice that PRAGMA WAL is used on the sqlite database, so data R
process and working R process must be on same machine.

Fast online algorithms
----------------------

tswbench provides a set of stateful online functions for realtime data
processing. All online functions are created by calling corresponding
make\_\*() functions. Please refer to manual to find full list of
supported algorithms.

``` r
# calculate triple exponential moving average
tema <- make_tema(period = 12)
data <- runif(10, max = 10)
value <- tema(data)
# the generated function tema() is stateful, thus suitable for incremental calculation
new_data <- runif(2, max = 10)
new_value <- tema(new_data)
```

``` r
# supported algorithm are usually pretty fast

  n = 10000
  w = 120

  # moving quantile
  x <- runif(n)
  probs <- seq(0, 1, 0.25)
  f <- make_moving_quantile(window = w, probs = probs)

  t1 <- system.time({y1 <- f(x)})
  t2 <- system.time({y2 <- zoo::rollapply(x, 120, quantile, probs = probs, type = 3, fill = NA, align = "right")})
  
  #speed-up
  t2 / t1
```

    ##     user   system  elapsed 
    ##  21.8169  34.0000 137.4444
