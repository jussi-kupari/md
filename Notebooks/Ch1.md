---
title: "ModernDive Ch 1: Getting Started with Data in R"
output: html_notebook
---

#### 1.4 Explore your first datasets

```r
# Load libraries
library(nycflights13)
```

```
## Warning: package 'nycflights13' was built under R version 4.1.3
```

```r
library(tidyverse)
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 4.1.3
```


```r
# (LC1.3) What does any ONE row in this flights dataset refer to?

  #  A. Data on an airline
  #  B. Data on a flight
  #  C. Data on an airport
  #  D. Data on multiple flights

# Answer: B
```


```r
glimpse(flights)
```

```
## Rows: 336,776
## Columns: 19
## $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 201~
## $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
## $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
## $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, 558, 558, 558, 559, 559, 559, 60~
## $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, 600, 600, 600, 600, 559, 600, 60~
## $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1, 0, -1, 0, 0, 1, -8, -3, -4, -4~
## $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849, 853, 924, 923, 941, 702, 854, 8~
## $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851, 856, 917, 937, 910, 706, 902, 8~
## $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -14, 31, -4, -8, -7, 12, -6, -8, 1~
## $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "AA", "B6", "B6", "UA", "UA", "AA~
## $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 49, 71, 194, 1124, 707, 1806, 118~
## $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N39463", "N516JB", "N829AS", "N593JB~
## $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA", "JFK", "LGA", "JFK", "JFK", "JF~
## $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD", "MCO", "ORD", "PBI", "TPA", "LA~
## $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149, 158, 345, 361, 257, 44, 337, 152,~
## $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733, 1028, 1005, 2475, 2565, 1389, 18~
## $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, ~
## $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0, 0, 0, 0, 10, 5, 10, 10, 7, 0, ~
## $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 05:00:00, 20~
```


```r
# LC1.4) What are some other examples in this dataset of categorical variables? 
# What makes them different than quantitative variables?

# A: tailnum, dest; these can't be used for numerical computations
```


```r
# This is another way to look at the data (base function)
str(flights)
```

```
## tibble [336,776 x 19] (S3: tbl_df/tbl/data.frame)
##  $ year          : int [1:336776] 2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
##  $ month         : int [1:336776] 1 1 1 1 1 1 1 1 1 1 ...
##  $ day           : int [1:336776] 1 1 1 1 1 1 1 1 1 1 ...
##  $ dep_time      : int [1:336776] 517 533 542 544 554 554 555 557 557 558 ...
##  $ sched_dep_time: int [1:336776] 515 529 540 545 600 558 600 600 600 600 ...
##  $ dep_delay     : num [1:336776] 2 4 2 -1 -6 -4 -5 -3 -3 -2 ...
##  $ arr_time      : int [1:336776] 830 850 923 1004 812 740 913 709 838 753 ...
##  $ sched_arr_time: int [1:336776] 819 830 850 1022 837 728 854 723 846 745 ...
##  $ arr_delay     : num [1:336776] 11 20 33 -18 -25 12 19 -14 -8 8 ...
##  $ carrier       : chr [1:336776] "UA" "UA" "AA" "B6" ...
##  $ flight        : int [1:336776] 1545 1714 1141 725 461 1696 507 5708 79 301 ...
##  $ tailnum       : chr [1:336776] "N14228" "N24211" "N619AA" "N804JB" ...
##  $ origin        : chr [1:336776] "EWR" "LGA" "JFK" "JFK" ...
##  $ dest          : chr [1:336776] "IAH" "IAH" "MIA" "BQN" ...
##  $ air_time      : num [1:336776] 227 227 160 183 116 150 158 53 140 138 ...
##  $ distance      : num [1:336776] 1400 1416 1089 1576 762 ...
##  $ hour          : num [1:336776] 5 5 5 5 6 5 6 6 6 6 ...
##  $ minute        : num [1:336776] 15 29 40 45 0 58 0 0 0 0 ...
##  $ time_hour     : POSIXct[1:336776], format: "2013-01-01 05:00:00" "2013-01-01 05:00:00" "2013-01-01 05:00:00" "2013-01-01 05:00:00" ...
```


```r
kable(airlines)
```



|carrier |name                        |
|:-------|:---------------------------|
|9E      |Endeavor Air Inc.           |
|AA      |American Airlines Inc.      |
|AS      |Alaska Airlines Inc.        |
|B6      |JetBlue Airways             |
|DL      |Delta Air Lines Inc.        |
|EV      |ExpressJet Airlines Inc.    |
|F9      |Frontier Airlines Inc.      |
|FL      |AirTran Airways Corporation |
|HA      |Hawaiian Airlines Inc.      |
|MQ      |Envoy Air                   |
|OO      |SkyWest Airlines Inc.       |
|UA      |United Air Lines Inc.       |
|US      |US Airways Inc.             |
|VX      |Virgin America              |
|WN      |Southwest Airlines Co.      |
|YV      |Mesa Airlines Inc.          |


```r
airlines$name
```

```
##  [1] "Endeavor Air Inc."           "American Airlines Inc."      "Alaska Airlines Inc."       
##  [4] "JetBlue Airways"             "Delta Air Lines Inc."        "ExpressJet Airlines Inc."   
##  [7] "Frontier Airlines Inc."      "AirTran Airways Corporation" "Hawaiian Airlines Inc."     
## [10] "Envoy Air"                   "SkyWest Airlines Inc."       "United Air Lines Inc."      
## [13] "US Airways Inc."             "Virgin America"              "Southwest Airlines Co."     
## [16] "Mesa Airlines Inc."
```


```r
# The skimr::skim gives a nice overlook of the data
skimr::skim(flights)
```


Table: Data summary

|                         |        |
|:------------------------|:-------|
|Name                     |flights |
|Number of rows           |336776  |
|Number of columns        |19      |
|_______________________  |        |
|Column type frequency:   |        |
|character                |4       |
|numeric                  |14      |
|POSIXct                  |1       |
|________________________ |        |
|Group variables          |None    |


**Variable type: character**

|skim_variable | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:-------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|carrier       |         0|          1.00|   2|   2|     0|       16|          0|
|tailnum       |      2512|          0.99|   5|   6|     0|     4043|          0|
|origin        |         0|          1.00|   3|   3|     0|        3|          0|
|dest          |         0|          1.00|   3|   3|     0|      105|          0|


**Variable type: numeric**

|skim_variable  | n_missing| complete_rate|    mean|      sd|   p0|  p25|  p50|  p75| p100|hist                                     |
|:--------------|---------:|-------------:|-------:|-------:|----:|----:|----:|----:|----:|:----------------------------------------|
|year           |         0|          1.00| 2013.00|    0.00| 2013| 2013| 2013| 2013| 2013|▁▁▇▁▁ |
|month          |         0|          1.00|    6.55|    3.41|    1|    4|    7|   10|   12|▇▆▆▆▇ |
|day            |         0|          1.00|   15.71|    8.77|    1|    8|   16|   23|   31|▇▇▇▇▆ |
|dep_time       |      8255|          0.98| 1349.11|  488.28|    1|  907| 1401| 1744| 2400|▁▇▆▇▃ |
|sched_dep_time |         0|          1.00| 1344.25|  467.34|  106|  906| 1359| 1729| 2359|▁▇▇▇▃ |
|dep_delay      |      8255|          0.98|   12.64|   40.21|  -43|   -5|   -2|   11| 1301|▇▁▁▁▁ |
|arr_time       |      8713|          0.97| 1502.05|  533.26|    1| 1104| 1535| 1940| 2400|▁▃▇▇▇ |
|sched_arr_time |         0|          1.00| 1536.38|  497.46|    1| 1124| 1556| 1945| 2359|▁▃▇▇▇ |
|arr_delay      |      9430|          0.97|    6.90|   44.63|  -86|  -17|   -5|   14| 1272|▇▁▁▁▁ |
|flight         |         0|          1.00| 1971.92| 1632.47|    1|  553| 1496| 3465| 8500|▇▃▃▁▁ |
|air_time       |      9430|          0.97|  150.69|   93.69|   20|   82|  129|  192|  695|▇▂▂▁▁ |
|distance       |         0|          1.00| 1039.91|  733.23|   17|  502|  872| 1389| 4983|▇▃▂▁▁ |
|hour           |         0|          1.00|   13.18|    4.66|    1|    9|   13|   17|   23|▁▇▇▇▅ |
|minute         |         0|          1.00|   26.23|   19.30|    0|    8|   29|   44|   59|▇▃▆▃▅ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|time_hour     |         0|             1|2013-01-01 05:00:00 |2013-12-31 23:00:00 |2013-07-03 10:00:00 |     6936|


```r
glimpse(airports)
```

```
## Rows: 1,458
## Columns: 8
## $ faa   <chr> "04G", "06A", "06C", "06N", "09J", "0A9", "0G6", "0G7", "0P2", "0S9", "0W3", "10C", "17G", "19A"~
## $ name  <chr> "Lansdowne Airport", "Moton Field Municipal Airport", "Schaumburg Regional", "Randall Airport", ~
## $ lat   <dbl> 41.13047, 32.46057, 41.98934, 41.43191, 31.07447, 36.37122, 41.46731, 42.88356, 39.79482, 48.053~
## $ lon   <dbl> -80.61958, -85.68003, -88.10124, -74.39156, -81.42778, -82.17342, -84.50678, -76.78123, -76.6471~
## $ alt   <dbl> 1044, 264, 801, 523, 11, 1593, 730, 492, 1000, 108, 409, 875, 1003, 951, 1789, 122, 152, 670, 11~
## $ tz    <dbl> -5, -6, -6, -5, -5, -5, -5, -5, -5, -8, -5, -6, -5, -5, -5, -5, -8, -6, -5, -7, -6, -5, -8, -6, ~
## $ dst   <chr> "A", "A", "A", "A", "A", "A", "A", "A", "U", "A", "A", "U", "A", "U", "A", "A", "A", "U", "A", "~
## $ tzone <chr> "America/New_York", "America/Chicago", "America/Chicago", "America/New_York", "America/New_York"~
```


```r
# (LC1.5) What properties of each airport do the variables 
# lat, lon, alt, tz, dst, and tzone describe in the airports data frame? 
# Take your best guess.

# A: lat = latitude, lon = longitude, tzone = time zone, dst = ??

# (LC1.6) Provide the names of variables in a data frame with at least three 
# variables where one of them is an identification variable and the other two are not. 
# Further, create your own tidy data frame that matches these conditions.

# A: 
planes %>% 
  select(tailnum, manufacturer, engines) %>% 
  glimpse()
```

```
## Rows: 3,322
## Columns: 3
## $ tailnum      <chr> "N10156", "N102UW", "N103US", "N104UW", "N10575", "N105UW", "N107US", "N108UW", "N109UW",~
## $ manufacturer <chr> "EMBRAER", "AIRBUS INDUSTRIE", "AIRBUS INDUSTRIE", "AIRBUS INDUSTRIE", "EMBRAER", "AIRBUS~
## $ engines      <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,~
```

