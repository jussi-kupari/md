---
title: "ModernDive Ch8: Bootstrapping and Confidence Intervals"
output: html_notebook
---


```r
# Load libraries
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------------------ tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.6     v dplyr   1.0.8
## v tidyr   1.2.0     v stringr 1.4.0
## v readr   2.1.2     v forcats 0.5.1
```

```
## -- Conflicts --------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(moderndive)
```

```
## Warning: package 'moderndive' was built under R version 4.1.3
```

```r
library(infer)
```

```
## Warning: package 'infer' was built under R version 4.1.3
```

```r
library(patchwork)

# Load function to clear libraries
source(here::here("clear_libraries.R"))
```

### 8.1 Pennies activity
#### 8.1.1 What is the average year on US pennies in 2019?


```r
pennies_sample
```

```
## # A tibble: 50 x 2
##       ID  year
##    <int> <dbl>
##  1     1  2002
##  2     2  1986
##  3     3  2017
##  4     4  1988
##  5     5  2008
##  6     6  1983
##  7     7  2008
##  8     8  1996
##  9     9  2004
## 10    10  2000
## # ... with 40 more rows
```


```r
# Exploratory visualization

pennies_sample %>% 
  ggplot(aes(year)) +
  geom_histogram(binwidth = 10, color = "white")
```

![plot of chunk unnamed-chunk-3](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-3-1.png)


```r
# Mean year
x_bar <-
  pennies_sample %>% 
  summarise(mean_year = mean(year))
x_bar
```

```
## # A tibble: 1 x 1
##   mean_year
##       <dbl>
## 1     1995.
```

#### 8.1.2 Resampling once

```r
pennies_resample <-
  tibble(
    year = c(
      1976,
      1962,
      1976,
      1983,
      2017,
      2015,
      2015,
      1962,
      2016,
      1976,
      2006,
      1997,
      1988,
      2015,
      2015,
      1988,
      2016,
      1978,
      1979,
      1997,
      1974,
      2013,
      1978,
      2015,
      2008,
      1982,
      1986,
      1979,
      1981,
      2004,
      2000,
      1995,
      1999,
      2006,
      1979,
      2015,
      1979,
      1998,
      1981,
      2015,
      2000,
      1999,
      1988,
      2017,
      1992,
      1997,
      1990,
      1988,
      2006,
      2000
    )
  )
```


```r
pennies_resample %>% 
  ggplot(aes(year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Resample of 50 pennies") +
  # This doesn't reproduce the figure from the book

pennies_sample %>% 
  ggplot(aes(year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Original sample of 50 pennies")
```

![plot of chunk unnamed-chunk-6](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-6-1.png)


```r
# Resample mean
pennies_resample %>% 
  summarise(mean_year = mean(year))
```

```
## # A tibble: 1 x 1
##   mean_year
##       <dbl>
## 1     1995.
```

```r
# This is not the same result as in the book
```

#### 8.1.3 Resampling 35 times


```r
resampled_means <-
  pennies_resamples %>% 
  group_by(name) %>% 
  summarise(mean_year = mean(year))
resampled_means
```

```
## # A tibble: 35 x 2
##    name      mean_year
##    <chr>         <dbl>
##  1 Arianna       1992.
##  2 Artemis       1996.
##  3 Bea           1996.
##  4 Camryn        1997.
##  5 Cassandra     1991.
##  6 Cindy         1995.
##  7 Claire        1996.
##  8 Dahlia        1998.
##  9 Dan           1994.
## 10 Eindra        1994.
## # ... with 25 more rows
```


```r
# Visualize resampling (bootstrap) distribution
resampled_means %>% 
  ggplot(aes(mean_year)) +
  geom_histogram(binwidth = 1, boundary = 1990, color = "white") +
  labs(x = "Sampled mean year")
```

![plot of chunk unnamed-chunk-9](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-9-1.png)

#### 8.1.4 What did we just do?

### 8.2 Computer simulation of resampling
#### 8.2.1 Virtually resampling once


```r
virtual_resample <-
  pennies_sample %>% 
  rep_slice_sample(n = 50, replace = TRUE)
virtual_resample
```

```
## # A tibble: 50 x 3
## # Groups:   replicate [1]
##    replicate    ID  year
##        <int> <int> <dbl>
##  1         1    38  1999
##  2         1    16  2015
##  3         1    31  2013
##  4         1    31  2013
##  5         1    27  1993
##  6         1    10  2000
##  7         1    35  1985
##  8         1    42  1997
##  9         1    44  2015
## 10         1    36  2015
## # ... with 40 more rows
```


```r
virtual_resample %>% 
  summarise(resample_mean = mean(year))
```

```
## # A tibble: 1 x 2
##   replicate resample_mean
##       <int>         <dbl>
## 1         1         1998.
```

#### 8.2.2 Virtually resampling 35 times

```r
# Create virtual resamples
virtual_resamples <-
  pennies_sample %>% 
  rep_slice_sample(n = 50, replace = TRUE, reps = 35)
virtual_resamples
```

```
## # A tibble: 1,750 x 3
## # Groups:   replicate [35]
##    replicate    ID  year
##        <int> <int> <dbl>
##  1         1    15  1974
##  2         1    48  1988
##  3         1    12  1995
##  4         1     5  2008
##  5         1    34  1985
##  6         1     2  1986
##  7         1    37  1962
##  8         1    22  1976
##  9         1     1  2002
## 10         1    15  1974
## # ... with 1,740 more rows
```


```r
# Count means for each replicate
virtual_resampled_means <-
  virtual_resamples %>% 
  group_by(replicate) %>% 
  summarise(mean_year = mean(year))
virtual_resampled_means
```

```
## # A tibble: 35 x 2
##    replicate mean_year
##        <int>     <dbl>
##  1         1     1996.
##  2         2     1993.
##  3         3     1995.
##  4         4     1995.
##  5         5     1994.
##  6         6     1996.
##  7         7     1995.
##  8         8     1996.
##  9         9     2000.
## 10        10     1997.
## # ... with 25 more rows
```


```r
# Visualize bootstrap distribution
virtual_resampled_means %>% 
  ggplot(aes(mean_year)) +
  geom_histogram(binwidth = 1, boundary = 1990, color = "white") +
  labs(x = "Resample mean year")
```

![plot of chunk unnamed-chunk-14](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-14-1.png)

#### 8.2.3 Virtually resampling 1000 times


```r
# Repeat resampling 1000 times
virtual_resamples <-
  pennies_sample %>% 
  rep_slice_sample(n = 50, replace = TRUE, reps = 1000)
virtual_resamples
```

```
## # A tibble: 50,000 x 3
## # Groups:   replicate [1,000]
##    replicate    ID  year
##        <int> <int> <dbl>
##  1         1    29  1988
##  2         1    30  1978
##  3         1    42  1997
##  4         1    35  1985
##  5         1     6  1983
##  6         1    50  2017
##  7         1     9  2004
##  8         1    28  2006
##  9         1    14  1978
## 10         1    42  1997
## # ... with 49,990 more rows
```


```r
# Compute 1000 sample means
virtual_resampled_means <-
  virtual_resamples %>% 
  group_by(replicate) %>% 
  summarise(mean_year = mean(year))
virtual_resampled_means
```

```
## # A tibble: 1,000 x 2
##    replicate mean_year
##        <int>     <dbl>
##  1         1     1995.
##  2         2     1993.
##  3         3     1994.
##  4         4     1995.
##  5         5     1998.
##  6         6     1995.
##  7         7     1994.
##  8         8     1998.
##  9         9     1995.
## 10        10     1997.
## # ... with 990 more rows
```


```r
# Combined to one pipeline
virtual_resampled_means <-
  pennies_sample %>% 
  rep_slice_sample(n = 50, replace = TRUE, reps = 1000) %>% 
  group_by(replicate) %>% 
  summarise(mean_year = mean(year))
virtual_resampled_means
```

```
## # A tibble: 1,000 x 2
##    replicate mean_year
##        <int>     <dbl>
##  1         1     1999.
##  2         2     1998.
##  3         3     1993.
##  4         4     1996.
##  5         5     1994.
##  6         6     1997.
##  7         7     1993.
##  8         8     1995.
##  9         9     1996.
## 10        10     1998.
## # ... with 990 more rows
```


```r
# Visualize bootstrap distibution from the 1000 replicates
virtual_resampled_means %>% 
  ggplot(aes(mean_year)) +
  geom_histogram(binwidth = 1, boundary = 1990, color = "white") +
  labs(x = "sample mean")
```

![plot of chunk unnamed-chunk-18](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-18-1.png)


```r
virtual_resampled_means %>% 
  summarise(mean_of_means = mean(mean_year))
```

```
## # A tibble: 1 x 1
##   mean_of_means
##           <dbl>
## 1         1995.
```


```r
# (LC8.1) What is the chief difference between a bootstrap distribution and a sampling distribution?

# A: A Sampling distribution is derived from individual samples all drawn from the actual population. A Bootstrap distribution is derived from one sample drawn from the population that is then sampled with replacement to get an approximation of the sampling distribution

# (LC8.2) Looking at the bootstrap distribution for the sample mean in Figure 8.14, between what two values would you say most values lie?

# A: 1991-2000
```

### 8.3 Understanding confidence intervals
#### 8.3.1 Percentile method

#### 8.3.2 Standard error method

```r
# Compute SE of bootstrap distribution
virtual_resampled_means %>% 
  summarise(SE = sd(mean_year))
```

```
## # A tibble: 1 x 1
##      SE
##   <dbl>
## 1  2.08
```


```r
# (LC8.3) What condition about the bootstrap distribution must be met for us to be able to construct confidence intervals using the standard error method?

# A: The bootstrap distribution must approximate a normal distribution

# (LC8.4) Say we wanted to construct a 68% confidence interval instead of a 95% confidence interval for μ. Describe what changes are needed to make this happen. Hint: we suggest you look at Appendix A.2 on the normal distribution.

# A: ~68% of the values will be inside +- 1SD in a normal distribution, so we would use mean +- SE as our confidence interval
```

### 8.4 Constructing confidence intervals
#### 8.4.1 Original workflow
#### 8.4.2 infer package workflow


```r
# Using dplyr
pennies_sample %>% 
  summarise(stat = mean(year))
```

```
## # A tibble: 1 x 1
##    stat
##   <dbl>
## 1 1995.
```


```r
# Using infer
pennies_sample %>% 
  specify(response = year) %>% 
  calculate(stat = "mean")
```

```
## Response: year (numeric)
## # A tibble: 1 x 1
##    stat
##   <dbl>
## 1 1995.
```


```r
# 1. SPECIFY - variables
pennies_sample %>% 
  specify(response = year) # or (response = yera ~ NULL)
```

```
## Response: year (numeric)
## # A tibble: 50 x 1
##     year
##    <dbl>
##  1  2002
##  2  1986
##  3  2017
##  4  1988
##  5  2008
##  6  1983
##  7  2008
##  8  1996
##  9  2004
## 10  2000
## # ... with 40 more rows
```


```r
# 2. GENERATE - data/replicates
pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = 1000, type = "bootstrap")
```

```
## Response: year (numeric)
## # A tibble: 50,000 x 2
## # Groups:   replicate [1,000]
##    replicate  year
##        <int> <dbl>
##  1         1  2002
##  2         1  2015
##  3         1  1979
##  4         1  1981
##  5         1  1999
##  6         1  2015
##  7         1  2017
##  8         1  2015
##  9         1  1988
## 10         1  2002
## # ... with 49,990 more rows
```


```r
# Compare to original workflow
pennies_sample %>% 
  rep_slice_sample(n = 50, replace = TRUE, reps = 1000)
```

```
## # A tibble: 50,000 x 3
## # Groups:   replicate [1,000]
##    replicate    ID  year
##        <int> <int> <dbl>
##  1         1     6  1983
##  2         1    19  1983
##  3         1    21  1981
##  4         1    23  1998
##  5         1    24  2017
##  6         1    29  1988
##  7         1    14  1978
##  8         1    30  1978
##  9         1    23  1998
## 10         1    12  1995
## # ... with 49,990 more rows
```


```r
# 3. CALCULATE - summary statistics
bootstrap_distribution <-
  pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")
bootstrap_distribution
```

```
## Response: year (numeric)
## # A tibble: 1,000 x 2
##    replicate  stat
##        <int> <dbl>
##  1         1 1994.
##  2         2 1993.
##  3         3 1993.
##  4         4 1994.
##  5         5 1995.
##  6         6 1995.
##  7         7 1997.
##  8         8 1992.
##  9         9 1996.
## 10        10 1994.
## # ... with 990 more rows
```


```r
# Compare to original workflow
pennies_sample %>% 
  rep_slice_sample(n = 50, replace = TRUE, reps = 1000) %>% 
  group_by(replicate) %>% 
  summarise(stat = mean(year))
```

```
## # A tibble: 1,000 x 2
##    replicate  stat
##        <int> <dbl>
##  1         1 1994.
##  2         2 1997.
##  3         3 1993.
##  4         4 1993.
##  5         5 1996.
##  6         6 1996 
##  7         7 1996.
##  8         8 1992.
##  9         9 1998.
## 10        10 1995.
## # ... with 990 more rows
```


```r
# 4. VISUALIZE - the results
  bootstrap_distribution %>% 
  visualize() 
```

![plot of chunk unnamed-chunk-30](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-30-1.png)


```r
# Compare to original workflow
bootstrap_distribution %>% 
  ggplot(aes(stat)) +
  geom_histogram(binwidth = 1, color = "white")
```

![plot of chunk unnamed-chunk-31](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-31-1.png)

#### 8.4.3 Percentile method with infer


```r
# 95% CI with the percentile method
percentile_ci <-
  bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci
```

```
## # A tibble: 1 x 2
##   lower_ci upper_ci
##      <dbl>    <dbl>
## 1    1991.    2000.
```


```r
# Visualize 95% CI (using percentile_ci)
bootstrap_distribution %>% 
  visualize() +
  shade_confidence_interval(endpoints = percentile_ci) 
```

![plot of chunk unnamed-chunk-33](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-33-1.png)


```r
# Shorter version (shade_ci)
bootstrap_distribution %>%
  visualize() +
  shade_ci(
    endpoints = percentile_ci,
    color = "hotpink",
    fill = "khaki",
    size = 1,
    alpha = 0.5
  )
```

![plot of chunk unnamed-chunk-34](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-34-1.png)


```r
# 95% CI with the SE method
standard_error_ci <-
  bootstrap_distribution %>% 
  get_ci(type = "se", point_estimate = x_bar) # Note abbreviated function name
standard_error_ci
```

```
## # A tibble: 1 x 2
##   lower_ci upper_ci
##      <dbl>    <dbl>
## 1    1991.    2000.
```


```r
# Visualize 95% CI using the SE method
bootstrap_distribution %>% 
  visualize() +
  shade_ci(endpoints = standard_error_ci)
```

![plot of chunk unnamed-chunk-36](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-36-1.png)


```r
# (LC8.5) Construct a 95% confidence interval for the median year of minting of all US pennies. Use the percentile method and, if appropriate, then use the standard-error method.

# Note: Book says SE method is not possible b/c the distribution is not bell-shaped

bootstrap_distribution_median <-
  pennies_sample %>%
  specify(response = year) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "median")

ci_95_percentile <-
  bootstrap_distribution_median %>% 
  get_ci(level = 0.95, type = "percentile")

ci_95_percentile
```

```
## # A tibble: 1 x 2
##   lower_ci upper_ci
##      <dbl>    <dbl>
## 1     1988     2001
```


```r
bootstrap_distribution_median %>%
  visualize() +
  shade_ci(endpoints = ci_95_percentile)
```

![plot of chunk unnamed-chunk-38](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-38-1.png)

### 8.5 Interpreting confidence intervals


```r
# Revisit the bowl sampling from Ch7; find true pop. proportion
bowl %>% 
  summarise(p_red = mean(color == "red"))
```

```
## # A tibble: 1 x 1
##   p_red
##   <dbl>
## 1 0.375
```

#### 8.5.1 Did the net capture the fish?


```r
# Ilyas and Yohan’s sample
bowl_sample_1
```

```
## # A tibble: 50 x 1
##    color
##    <chr>
##  1 white
##  2 white
##  3 red  
##  4 red  
##  5 white
##  6 white
##  7 red  
##  8 white
##  9 white
## 10 white
## # ... with 40 more rows
```


```r
# Specify
bowl_sample_1 %>% 
  specify(response = color, success = "red")
```

```
## Response: color (factor)
## # A tibble: 50 x 1
##    color
##    <fct>
##  1 white
##  2 white
##  3 red  
##  4 red  
##  5 white
##  6 white
##  7 red  
##  8 white
##  9 white
## 10 white
## # ... with 40 more rows
```


```r
# Generate
bowl_sample_1 %>% 
  specify(response = color, success = "red") %>% 
  generate(reps = 1000, type = "bootstrap")
```

```
## Response: color (factor)
## # A tibble: 50,000 x 2
## # Groups:   replicate [1,000]
##    replicate color
##        <int> <fct>
##  1         1 white
##  2         1 white
##  3         1 red  
##  4         1 white
##  5         1 white
##  6         1 red  
##  7         1 red  
##  8         1 white
##  9         1 red  
## 10         1 white
## # ... with 49,990 more rows
```


```r
# Calculate
sample_1_bootstrap <-
  bowl_sample_1 %>% 
  specify(response = color, success = "red") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
sample_1_bootstrap
```

```
## Response: color (factor)
## # A tibble: 1,000 x 2
##    replicate  stat
##        <int> <dbl>
##  1         1  0.38
##  2         2  0.42
##  3         3  0.5 
##  4         4  0.44
##  5         5  0.24
##  6         6  0.4 
##  7         7  0.32
##  8         8  0.3 
##  9         9  0.48
## 10        10  0.44
## # ... with 990 more rows
```


```r
# Get 95% CI
percentile_ci_1 <-
  sample_1_bootstrap %>% 
  get_ci(level = 0.95, type = "percentile")
percentile_ci_1
```

```
## # A tibble: 1 x 2
##   lower_ci upper_ci
##      <dbl>    <dbl>
## 1     0.28    0.560
```

```r
# This CI contains the true population parameter, 0.375
```


```r
# Visualize

sample_1_bootstrap %>% 
  visualize(bins = 15) +
  shade_ci(endpoints = percentile_ci_1) +
  geom_vline(xintercept = 0.42, linetype = "dashed")
```

![plot of chunk unnamed-chunk-45](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-45-1.png)


```r
# Will every 95% confidence interval capture the true pop. parameter
bowl_sample_2 <-
  bowl %>% 
  rep_slice_sample(n = 50)
bowl_sample_2
```

```
## # A tibble: 50 x 3
## # Groups:   replicate [1]
##    replicate ball_ID color
##        <int>   <int> <chr>
##  1         1     209 red  
##  2         1     596 red  
##  3         1    1197 white
##  4         1     933 white
##  5         1    1242 red  
##  6         1     438 white
##  7         1     194 white
##  8         1    1302 white
##  9         1    1418 red  
## 10         1    1364 white
## # ... with 40 more rows
```


```r
sample_2_bootstrap <-
  bowl_sample_2 %>% 
  specify(response = color, success = "red") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
sample_2_bootstrap
```

```
## Response: color (factor)
## # A tibble: 1,000 x 2
##    replicate  stat
##        <int> <dbl>
##  1         1  0.38
##  2         2  0.3 
##  3         3  0.46
##  4         4  0.4 
##  5         5  0.46
##  6         6  0.48
##  7         7  0.46
##  8         8  0.44
##  9         9  0.44
## 10        10  0.44
## # ... with 990 more rows
```


```r
# The real population proportion is again within the CI
percentile_ci_2 <-
  sample_2_bootstrap %>% 
  get_ci(level = 0.95, type = "percentile")
percentile_ci_2
```

```
## # A tibble: 1 x 2
##   lower_ci upper_ci
##      <dbl>    <dbl>
## 1      0.3     0.58
```

#### 8.5.2 Precise and shorthand interpretation

```r
# Precise interpretation: If we repeated our sampling procedure a large number of times, we expect about 95% of the resulting confidence intervals to capture the value of the population parameter.

# incorrect interpretation is: There is a 95% probability that the confidence interval contains p.

# Short-hand interpretation: We are 95% “confident” that a 95% confidence interval captures the value of the population parameter.
```

#### 8.5.3 Width of confidence intervals

### 8.6 Case study: Is yawning contagious?
#### 8.6.1 Mythbusters study data


```r
mythbusters_yawn
```

```
## # A tibble: 50 x 3
##     subj group   yawn 
##    <int> <chr>   <chr>
##  1     1 seed    yes  
##  2     2 control yes  
##  3     3 seed    no   
##  4     4 seed    yes  
##  5     5 seed    no   
##  6     6 control no   
##  7     7 seed    yes  
##  8     8 control no   
##  9     9 control no   
## 10    10 seed    no   
## # ... with 40 more rows
```


```r
mythbusters_yawn %>% count(group, yawn)
```

```
## # A tibble: 4 x 3
##   group   yawn      n
##   <chr>   <chr> <int>
## 1 control no       12
## 2 control yes       4
## 3 seed    no       24
## 4 seed    yes      10
```

#### 8.6.2 Sampling scenario
#### 8.6.3 Constructing the confidence interval


```r
# Specify
mythbusters_yawn %>% 
  specify(formula = yawn ~ group, success = "yes")
```

```
## Response: yawn (factor)
## Explanatory: group (factor)
## # A tibble: 50 x 2
##    yawn  group  
##    <fct> <fct>  
##  1 yes   seed   
##  2 yes   control
##  3 no    seed   
##  4 yes   seed   
##  5 no    seed   
##  6 no    control
##  7 yes   seed   
##  8 no    control
##  9 no    control
## 10 no    seed   
## # ... with 40 more rows
```


```r
# Generate
mythbusters_yawn %>% 
  specify(formula = yawn ~ group, success = "yes") %>% 
  generate(reps = 1000, type = "bootstrap")
```

```
## Response: yawn (factor)
## Explanatory: group (factor)
## # A tibble: 50,000 x 3
## # Groups:   replicate [1,000]
##    replicate yawn  group  
##        <int> <fct> <fct>  
##  1         1 no    seed   
##  2         1 no    seed   
##  3         1 no    seed   
##  4         1 no    seed   
##  5         1 no    control
##  6         1 no    seed   
##  7         1 no    seed   
##  8         1 no    seed   
##  9         1 no    control
## 10         1 no    control
## # ... with 49,990 more rows
```


```r
# Calculate
bootstrap_distribution_yawning <-
  mythbusters_yawn %>% 
  specify(formula = yawn ~ group, success = "yes") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = c("seed", "control"))
bootstrap_distribution_yawning 
```

```
## Response: yawn (factor)
## Explanatory: group (factor)
## # A tibble: 1,000 x 2
##    replicate     stat
##        <int>    <dbl>
##  1         1 -0.131  
##  2         2  0.112  
##  3         3 -0.0478 
##  4         4  0.0963 
##  5         5  0.141  
##  6         6  0.168  
##  7         7  0.00952
##  8         8  0.101  
##  9         9  0.118  
## 10        10 -0.0286 
## # ... with 990 more rows
```


```r
# Visualize
bootstrap_distribution_yawning %>% 
  visualize() +
  geom_vline(xintercept = 0)
```

![plot of chunk unnamed-chunk-55](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-55-1.png)


```r
# Compute 95% CI using the percentile method
myth_ci_percentile <-
  bootstrap_distribution_yawning %>% 
  get_ci(level = 0.95, type = "percentile")
myth_ci_percentile
```

```
## # A tibble: 1 x 2
##   lower_ci upper_ci
##      <dbl>    <dbl>
## 1   -0.214    0.286
```


```r
# Compute 95% CI using the SE method
obs_diff_in_props <- mythbusters_yawn %>% 
  specify(formula = yawn ~ group, success = "yes") %>% 
  # generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("seed", "control"))

myth_ci_se <-
  bootstrap_distribution_yawning %>% 
  get_ci(type = "se", point_estimate = obs_diff_in_props)
```


```r
# Visualize
bootstrap_distribution_yawning %>% 
  visualize() +
  shade_ci(endpoints = myth_ci_percentile, color = "black") +
  shade_ci(endpoints = myth_ci_se, color = "gray")
```

![plot of chunk unnamed-chunk-58](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-58-1.png)

#### 8.6.4 Interpreting the confidence interval

### 8.7 Conclusion
#### 8.7.1 Comparing bootstrap and sampling distributions


```r
# Take 1000 virtual samples of size 50 from the bowl:
virtual_samples <- 
  bowl %>% 
  rep_slice_sample(n = 50, reps = 1000)

# Compute the sampling distribution of 1000 values of p-hat
sampling_distribution <- 
  virtual_samples %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 50)

# Visualize sampling distribution of p-hat
sampling_distribution %>% 
  ggplot(aes(prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", 
       title = "Sampling distribution")
```

![plot of chunk unnamed-chunk-59](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-59-1.png)


```r
sampling_distribution %>% 
  summarise(se = sd(prop_red))
```

```
## # A tibble: 1 x 1
##       se
##    <dbl>
## 1 0.0670
```

Bootstrap distribution

```r
bootstrap_distribution  <-
  bowl_sample_1 %>% 
  specify(response = color, success = "red") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
bootstrap_distribution
```

```
## Response: color (factor)
## # A tibble: 1,000 x 2
##    replicate  stat
##        <int> <dbl>
##  1         1  0.44
##  2         2  0.36
##  3         3  0.44
##  4         4  0.38
##  5         5  0.44
##  6         6  0.46
##  7         7  0.48
##  8         8  0.36
##  9         9  0.42
## 10        10  0.36
## # ... with 990 more rows
```


```r
bootstrap_distribution %>% 
  visualize()
```

![plot of chunk unnamed-chunk-62](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-62-1.png)


```r
bootstrap_distribution %>% 
  summarise(se = sd(stat))
```

```
## # A tibble: 1 x 1
##       se
##    <dbl>
## 1 0.0699
```

Confidence intervals based on 33 tactile samples

```r
conf_ints <- 
  tactile_prop_red %>% 
  rename(p_hat = prop_red) %>% 
  mutate(
    n = 50,
    SE = sqrt(p_hat * (1 - p_hat) / n),
    MoE = 1.96 * SE,
    lower_ci = p_hat - MoE,
    upper_ci = p_hat + MoE
  )
conf_ints
```

```
## # A tibble: 33 x 9
##    group            replicate red_balls p_hat     n     SE   MoE lower_ci upper_ci
##    <chr>                <int>     <int> <dbl> <dbl>  <dbl> <dbl>    <dbl>    <dbl>
##  1 Ilyas, Yohan             1        21  0.42    50 0.0698 0.137    0.283    0.557
##  2 Morgan, Terrance         2        17  0.34    50 0.0670 0.131    0.209    0.471
##  3 Martin, Thomas           3        21  0.42    50 0.0698 0.137    0.283    0.557
##  4 Clark, Frank             4        21  0.42    50 0.0698 0.137    0.283    0.557
##  5 Riddhi, Karina           5        18  0.36    50 0.0679 0.133    0.227    0.493
##  6 Andrew, Tyler            6        19  0.38    50 0.0686 0.135    0.245    0.515
##  7 Julia                    7        19  0.38    50 0.0686 0.135    0.245    0.515
##  8 Rachel, Lauren           8        11  0.22    50 0.0586 0.115    0.105    0.335
##  9 Daniel, Caroline         9        15  0.3     50 0.0648 0.127    0.173    0.427
## 10 Josh, Maeve             10        17  0.34    50 0.0670 0.131    0.209    0.471
## # ... with 23 more rows
```

```r
# Did the net capture the fish 95% of the time
conf_ints <-
  conf_ints %>% 
  mutate(captured_pop_prop = lower_ci <= 0.375 & 0.375 <= upper_ci)
mean(conf_ints$captured_pop_prop)
```

```
## [1] 0.9393939
```

```r
# Almost
```


```r
clear_libraries()
```

```
## Warning: 'infer' namespace cannot be unloaded:
##   namespace 'infer' is imported by 'moderndive' so cannot be unloaded
```

```
## Warning: 'forcats' namespace cannot be unloaded:
##   namespace 'forcats' is imported by 'tidyverse', 'haven' so cannot be unloaded
```

```
## Warning: 'stringr' namespace cannot be unloaded:
##   namespace 'stringr' is imported by 'tidyverse', 'janitor' so cannot be unloaded
```

```
## Warning: 'dplyr' namespace cannot be unloaded:
##   namespace 'dplyr' is imported by 'broom', 'janitor', 'tidyr', 'dbplyr', 'infer' so cannot be unloaded
```

```
## Warning: 'purrr' namespace cannot be unloaded:
##   namespace 'purrr' is imported by 'broom', 'tidyr', 'modelr', 'styler', 'tidyselect', 'infer' so cannot be unloaded
```

```
## Warning: 'readr' namespace cannot be unloaded:
##   namespace 'readr' is imported by 'tidyverse' so cannot be unloaded
```

```
## Warning: 'tidyr' namespace cannot be unloaded:
##   namespace 'tidyr' is imported by 'tidyverse', 'broom' so cannot be unloaded
```

```
## Warning: 'tibble' namespace cannot be unloaded:
##   namespace 'tibble' is imported by 'broom', 'ggplot2', 'tidyr', 'modelr', 'haven', 'dplyr', 'dbplyr', 'styler', 'readr', 'infer' so cannot be unloaded
```

```
## Warning: 'ggplot2' namespace cannot be unloaded:
##   namespace 'ggplot2' is imported by 'tidyverse', 'infer' so cannot be unloaded
```
