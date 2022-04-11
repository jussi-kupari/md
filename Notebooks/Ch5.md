---
title: "ModernDive Ch 5: Basic Regression"
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
library(skimr)
```

```
## Warning: package 'skimr' was built under R version 4.1.3
```

```r
library(gapminder)
```

```
## Warning: package 'gapminder' was built under R version 4.1.3
```

```r
# Load function to clear libraries
source(here::here("clear_libraries.R"))
```

### 5.1 One numerical explanatory variable
#### 5.1.1 Exploratory data analysis


```r
# Select columns from evals
evals_ch5 <-
  evals %>% 
  select(ID, score, bty_avg, age)

evals_ch5
```

```
## # A tibble: 463 x 4
##       ID score bty_avg   age
##    <int> <dbl>   <dbl> <int>
##  1     1   4.7    5       36
##  2     2   4.1    5       36
##  3     3   3.9    5       36
##  4     4   4.8    5       36
##  5     5   4.6    3       59
##  6     6   4.3    3       59
##  7     7   2.8    3       59
##  8     8   4.1    3.33    51
##  9     9   3.4    3.33    51
## 10    10   4.5    3.17    40
## # ... with 453 more rows
```


```r
 # Here are three common steps in an EDA:

  #   Most crucially, looking at the raw data values.
  #   Computing summary statistics, such as means, medians, and interquartile ranges.
  #   Creating data visualizations.
```


```r
# Look at the raw data
glimpse(evals_ch5)
```

```
## Rows: 463
## Columns: 4
## $ ID      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, ~
## $ score   <dbl> 4.7, 4.1, 3.9, 4.8, 4.6, 4.3, 2.8, 4.1, 3.4, 4.5, 3.8, 4.5, 4.6, 3.9, 3.9, 4.3, 4.5, 4.8, 4.6, ~
## $ bty_avg <dbl> 5.000, 5.000, 5.000, 5.000, 3.000, 3.000, 3.000, 3.333, 3.333, 3.167, 3.167, 3.167, 3.167, 3.16~
## $ age     <int> 36, 36, 36, 36, 59, 59, 59, 51, 51, 40, 40, 40, 40, 40, 40, 40, 40, 31, 31, 31, 31, 31, 31, 62,~
```


```r
# Random sample
evals_ch5 %>% 
  slice_sample(n = 5)
```

```
## # A tibble: 5 x 4
##      ID score bty_avg   age
##   <int> <dbl>   <dbl> <int>
## 1     6   4.3    3       59
## 2   100   4.4    4.33    48
## 3   164   4.4    4.33    63
## 4   389   3.7    3       57
## 5   287   4.1    1.67    34
```


```r
# Create summaries
evals_ch5 %>% 
  summarise(
    mean_bty_average = mean(bty_avg),
    mean_score = mean(score),
    median_bty_average = median(bty_avg),
    median_score = median(score)
  )
```

```
## # A tibble: 1 x 4
##   mean_bty_average mean_score median_bty_average median_score
##              <dbl>      <dbl>              <dbl>        <dbl>
## 1             4.42       4.17               4.33          4.3
```


```r
# skimr:skim
evals_ch5 %>% 
  select(score, bty_avg) %>% 
  skimr::skim()
```


Table: Data summary

|                         |           |
|:------------------------|:----------|
|Name                     |Piped data |
|Number of rows           |463        |
|Number of columns        |2          |
|_______________________  |           |
|Column type frequency:   |           |
|numeric                  |2          |
|________________________ |           |
|Group variables          |None       |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate| mean|   sd|   p0|  p25|  p50| p75| p100|hist                                     |
|:-------------|---------:|-------------:|----:|----:|----:|----:|----:|---:|----:|:----------------------------------------|
|score         |         0|             1| 4.17| 0.54| 2.30| 3.80| 4.30| 4.6| 5.00|▁▁▅▇▇ |
|bty_avg       |         0|             1| 4.42| 1.53| 1.67| 3.17| 4.33| 5.5| 8.17|▃▇▇▃▂ |


```r
# Correlation coeff
evals_ch5 %>% 
get_correlation(score ~ bty_avg)
```

```
## # A tibble: 1 x 1
##     cor
##   <dbl>
## 1 0.187
```


```r
# Alternative way to correlate
evals_ch5 %>% 
  summarise(correlation = cor(score, bty_avg))
```

```
## # A tibble: 1 x 1
##   correlation
##         <dbl>
## 1       0.187
```


```r
# Exploratory visualization
evals_ch5 %>%
  ggplot(aes(bty_avg, score)) +
  geom_point() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Relationship of teaching and beauty scores")
```

![plot of chunk unnamed-chunk-10](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-10-1.png)


```r
# Remove overplotting
evals_ch5 %>%
  ggplot(aes(bty_avg, score)) +
  geom_jitter() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Relationship of teaching and beauty scores")
```

![plot of chunk unnamed-chunk-11](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-11-1.png)


```r
# Add best fitting line
evals_ch5 %>%
  ggplot(aes(bty_avg, score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Relationship of teaching and beauty scores")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![plot of chunk unnamed-chunk-12](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-12-1.png)


```r
# (LC5.1) Conduct a new exploratory data analysis with the same outcome 
# variable y being score but with age as the new explanatory variable x. 

# Remember, this involves three things:

  #  Looking at the raw data values.
  #  Computing summary statistics.
  #  Creating data visualizations.

# What can you say about the relationship between age and teaching scores based 
# on this exploration?

# Answer: There appears to be a very small negative correlation between score and
# the age of the teacher (score reduces with age)

glimpse(evals_ch5)
```

```
## Rows: 463
## Columns: 4
## $ ID      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, ~
## $ score   <dbl> 4.7, 4.1, 3.9, 4.8, 4.6, 4.3, 2.8, 4.1, 3.4, 4.5, 3.8, 4.5, 4.6, 3.9, 3.9, 4.3, 4.5, 4.8, 4.6, ~
## $ bty_avg <dbl> 5.000, 5.000, 5.000, 5.000, 3.000, 3.000, 3.000, 3.333, 3.333, 3.167, 3.167, 3.167, 3.167, 3.16~
## $ age     <int> 36, 36, 36, 36, 59, 59, 59, 51, 51, 40, 40, 40, 40, 40, 40, 40, 40, 31, 31, 31, 31, 31, 31, 62,~
```


```r
# Summary stats
evals_ch5 %>% 
  select(score, age) %>% 
  skimr::skim()
```


Table: Data summary

|                         |           |
|:------------------------|:----------|
|Name                     |Piped data |
|Number of rows           |463        |
|Number of columns        |2          |
|_______________________  |           |
|Column type frequency:   |           |
|numeric                  |2          |
|________________________ |           |
|Group variables          |None       |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|   sd|   p0|  p25|  p50|  p75| p100|hist                                     |
|:-------------|---------:|-------------:|-----:|----:|----:|----:|----:|----:|----:|:----------------------------------------|
|score         |         0|             1|  4.17| 0.54|  2.3|  3.8|  4.3|  4.6|    5|▁▁▅▇▇ |
|age           |         0|             1| 48.37| 9.80| 29.0| 42.0| 48.0| 57.0|   73|▅▆▇▆▁ |


```r
# Correlation
evals_ch5 %>% 
  get_correlation(score ~ age)
```

```
## # A tibble: 1 x 1
##      cor
##    <dbl>
## 1 -0.107
```



```r
# Plotting
evals_ch5 %>% 
  ggplot(aes(age, score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![plot of chunk unnamed-chunk-16](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-16-1.png)

#### 5.1.2 Simple linear regression

```r
# Fit regrssion model
score_model <- lm(score ~ bty_avg, data = evals_ch5)

# Get regression table
get_regression_table(score_model)
```

```
## # A tibble: 2 x 7
##   term      estimate std_error statistic p_value lower_ci upper_ci
##   <chr>        <dbl>     <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
## 1 intercept    3.88      0.076     51.0        0    3.73     4.03 
## 2 bty_avg      0.067     0.016      4.09       0    0.035    0.099
```


```r
# (LC5.2) 
# Fit a new simple linear regression using lm(score ~ age, data = evals_ch5) 
# where age is the new explanatory variable x. Get information about the “best-fitting” 
# line from the regression table by applying the get_regression_table() function. 
# How do the regression results match up with the results from your earlier exploratory data analysis?

evals_ch5 %>% 
  lm(score ~ age, data = .) %>% 
  get_regression_table()
```

```
## # A tibble: 2 x 7
##   term      estimate std_error statistic p_value lower_ci upper_ci
##   <chr>        <dbl>     <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
## 1 intercept    4.46      0.127     35.2    0        4.21     4.71 
## 2 age         -0.006     0.003     -2.31   0.021   -0.011   -0.001
```

```r
# Answer: The slope is very slightly negative (-0.006). This fist with the correaltion 
  # that is slightly negative (-0.1). The slope intercept seems equal to the trend line.
```

#### 5.1.3 Observed/fitted values and residuals


```r
# Get regression points
regression_points <- get_regression_points(score_model)
regression_points
```

```
## # A tibble: 463 x 5
##       ID score bty_avg score_hat residual
##    <int> <dbl>   <dbl>     <dbl>    <dbl>
##  1     1   4.7    5         4.21    0.486
##  2     2   4.1    5         4.21   -0.114
##  3     3   3.9    5         4.21   -0.314
##  4     4   4.8    5         4.21    0.586
##  5     5   4.6    3         4.08    0.52 
##  6     6   4.3    3         4.08    0.22 
##  7     7   2.8    3         4.08   -1.28 
##  8     8   4.1    3.33      4.10   -0.002
##  9     9   3.4    3.33      4.10   -0.702
## 10    10   4.5    3.17      4.09    0.409
## # ... with 453 more rows
```


```r
# (LC5.3) Generate a data frame of the residuals of the model where 
# you used age as the explanatory x variable.

evals_ch5 %>% 
  lm(score ~ age, data = .) %>% 
  get_regression_points()
```

```
## # A tibble: 463 x 5
##       ID score   age score_hat residual
##    <int> <dbl> <int>     <dbl>    <dbl>
##  1     1   4.7    36      4.25    0.452
##  2     2   4.1    36      4.25   -0.148
##  3     3   3.9    36      4.25   -0.348
##  4     4   4.8    36      4.25    0.552
##  5     5   4.6    59      4.11    0.488
##  6     6   4.3    59      4.11    0.188
##  7     7   2.8    59      4.11   -1.31 
##  8     8   4.1    51      4.16   -0.059
##  9     9   3.4    51      4.16   -0.759
## 10    10   4.5    40      4.22    0.276
## # ... with 453 more rows
```

#### 5.2 One categorical explanatory variable


```r
# filter and select from gapminder
gapminder_2007 <-
  gapminder %>% 
  filter(year == 2007) %>% 
  select(country, lifeExp, continent, gdpPercap)

glimpse(gapminder_2007)
```

```
## Rows: 142
## Columns: 4
## $ country   <fct> "Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Australia", "Austria", "Bahrain"~
## $ lifeExp   <dbl> 43.828, 76.423, 72.301, 42.731, 75.320, 81.235, 79.829, 75.635, 64.062, 79.441, 56.728, 65.55~
## $ continent <fct> Asia, Europe, Africa, Africa, Americas, Oceania, Europe, Asia, Asia, Europe, Africa, Americas~
## $ gdpPercap <dbl> 974.5803, 5937.0295, 6223.3675, 4797.2313, 12779.3796, 34435.3674, 36126.4927, 29796.0483, 13~
```


```r
# random sample

gapminder_2007 %>% 
  slice_sample(n = 5)
```

```
## # A tibble: 5 x 4
##   country   lifeExp continent gdpPercap
##   <fct>       <dbl> <fct>         <dbl>
## 1 Haiti        60.9 Americas      1202.
## 2 Hungary      73.3 Europe       18009.
## 3 Argentina    75.3 Americas     12779.
## 4 Somalia      48.2 Africa         926.
## 5 Iraq         59.5 Asia          4471.
```


```r
# skim

gapminder_2007 %>% 
  select(lifeExp, continent) %>% 
  skimr::skim()
```


Table: Data summary

|                         |           |
|:------------------------|:----------|
|Name                     |Piped data |
|Number of rows           |142        |
|Number of columns        |2          |
|_______________________  |           |
|Column type frequency:   |           |
|factor                   |1          |
|numeric                  |1          |
|________________________ |           |
|Group variables          |None       |


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                         |
|:-------------|---------:|-------------:|:-------|--------:|:----------------------------------|
|continent     |         0|             1|FALSE   |        5|Afr: 52, Asi: 33, Eur: 30, Ame: 25 |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|    sd|    p0|   p25|   p50|   p75| p100|hist                                     |
|:-------------|---------:|-------------:|-----:|-----:|-----:|-----:|-----:|-----:|----:|:----------------------------------------|
|lifeExp       |         0|             1| 67.01| 12.07| 39.61| 57.16| 71.94| 76.41| 82.6|▂▃▃▆▇ |


```r
# Visualize
gapminder_2007 %>% 
  ggplot(aes(lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") + 
  labs(x = "Life Expectancy", y = "Number of countries",
       title = "Histogram of distributions of worldwide life expectancies")
```

![plot of chunk unnamed-chunk-24](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-24-1.png)


```r
# Visualize with facets
gapminder_2007 %>% 
  ggplot(aes(lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") + 
  labs(
    x = "Life Expectancy", y = "Number of countries",
    title = "Histogram of distributions of worldwide life expectancies"
  ) +
  facet_wrap(~ continent, nrow = 2)
```

![plot of chunk unnamed-chunk-25](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-25-1.png)


```r
# Boxplot

gapminder_2007 %>% 
  ggplot(aes(continent, lifeExp)) +
  geom_boxplot() +
  labs(
    x = "Continent", 
    y = "Life expectancy", 
    title = "Life expectancy by continent"
  )
```

![plot of chunk unnamed-chunk-26](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-26-1.png)


```r
# Compute median and mean
lifeExp_by_continent <-
  gapminder_2007 %>% 
  group_by(continent) %>% 
  summarise(median = median(lifeExp),
            mean = mean(lifeExp))
lifeExp_by_continent
```

```
## # A tibble: 5 x 3
##   continent median  mean
##   <fct>      <dbl> <dbl>
## 1 Africa      52.9  54.8
## 2 Americas    72.9  73.6
## 3 Asia        72.4  70.7
## 4 Europe      78.6  77.6
## 5 Oceania     80.7  80.7
```


```r
# Mean lifeExp by continent and relative diff from mean for Africa
lifeExp_by_continent %>% 
  mutate(`Difference versus Africa` = mean - mean[1])
```

```
## # A tibble: 5 x 4
##   continent median  mean `Difference versus Africa`
##   <fct>      <dbl> <dbl>                      <dbl>
## 1 Africa      52.9  54.8                        0  
## 2 Americas    72.9  73.6                       18.8
## 3 Asia        72.4  70.7                       15.9
## 4 Europe      78.6  77.6                       22.8
## 5 Oceania     80.7  80.7                       25.9
```


```r
# (LC5.4) Conduct a new exploratory data analysis with the same explanatory # variable x being continent but with gdpPercap as the new outcome variable # y. What can you say about the differences in GDP per capita 
# between continents based on this exploration?

gapminder_2007 %>% 
  ggplot(aes(continent, gdpPercap)) +
  geom_boxplot() +
  geom_point()
```

![plot of chunk unnamed-chunk-29](C:/Users/juskup/OneDrive - Karolinska Institutet/Dokument/ModernDive/notebooks/figures/unnamed-chunk-29-1.png)

```r
# Answer: Europe and Oceania have clearly the highest gdpPercap,
  # Asia has a low median, but with some countries with high gdpPercap,
  # Americas is low, but two countries (Canada and USA?) have high values,
  # African countries score very low
```


#### 5.2.2 Linear regression


```r
# with categorical explanatory variable

lifeExp_model <-
  gapminder_2007 %>% 
  lm(lifeExp ~ continent, data = .)

lifeExp_model %>% get_regression_table()
```

```
## # A tibble: 5 x 7
##   term                estimate std_error statistic p_value lower_ci upper_ci
##   <chr>                  <dbl>     <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
## 1 intercept               54.8      1.02     53.4        0     52.8     56.8
## 2 continent: Americas     18.8      1.8      10.4        0     15.2     22.4
## 3 continent: Asia         15.9      1.65      9.68       0     12.7     19.2
## 4 continent: Europe       22.8      1.70     13.5        0     19.5     26.2
## 5 continent: Oceania      25.9      5.33      4.86       0     15.4     36.4
```


```r
# (LC5.5) Fit a new linear regression using lm(gdpPercap ~ continent, 
# data = gapminder2007) where gdpPercap is the new outcome variable y. 
# Get information about the “best-fitting” line from the regression 
# table by applying the get_regression_table() function. How do the 
# regression results match up with the results from your previous 
# exploratory data analysis?

gapminder_2007 %>% 
  lm(gdpPercap ~ continent, data = .) %>% 
  get_regression_table()
```

```
## # A tibble: 5 x 7
##   term                estimate std_error statistic p_value lower_ci upper_ci
##   <chr>                  <dbl>     <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
## 1 intercept              3089.     1373.      2.25   0.026     375.    5804.
## 2 continent: Americas    7914.     2409.      3.28   0.001    3150.   12678.
## 3 continent: Asia        9384.     2203.      4.26   0        5027.   13741.
## 4 continent: Europe     21965.     2270.      9.68   0       17478.   26453.
## 5 continent: Oceania    26721.     7133.      3.75   0       12616.   40826.
```

```r
# Answer: They mach as expected. Africa is the baseline; however,
  # The mean values are quite different from the median in some cases.
```

#### 5.2.3 Observed/fitted values and residuals

```r
regression_points <- get_regression_points(lifeExp_model, ID = "country")
regression_points
```

```
## # A tibble: 142 x 5
##    country     lifeExp continent lifeExp_hat residual
##    <fct>         <dbl> <fct>           <dbl>    <dbl>
##  1 Afghanistan    43.8 Asia             70.7  -26.9  
##  2 Albania        76.4 Europe           77.6   -1.23 
##  3 Algeria        72.3 Africa           54.8   17.5  
##  4 Angola         42.7 Africa           54.8  -12.1  
##  5 Argentina      75.3 Americas         73.6    1.71 
##  6 Australia      81.2 Oceania          80.7    0.516
##  7 Austria        79.8 Europe           77.6    2.18 
##  8 Bahrain        75.6 Asia             70.7    4.91 
##  9 Bangladesh     64.1 Asia             70.7   -6.67 
## 10 Belgium        79.4 Europe           77.6    1.79 
## # ... with 132 more rows
```


```r
# (LC5.6) Using either the sorting functionality of RStudio’s spreadsheet viewer or using the data wrangling tools you learned in Chapter 3, identify the five countries with the five smallest (most negative) residuals? What do these negative residuals say about their life expectancy relative to their continents’ life expectancy?

regression_points %>% 
  slice_min(residual, n = 5)
```

```
## # A tibble: 5 x 5
##   country     lifeExp continent lifeExp_hat residual
##   <fct>         <dbl> <fct>           <dbl>    <dbl>
## 1 Afghanistan    43.8 Asia             70.7    -26.9
## 2 Swaziland      39.6 Africa           54.8    -15.2
## 3 Mozambique     42.1 Africa           54.8    -12.7
## 4 Haiti          60.9 Americas         73.6    -12.7
## 5 Zambia         42.4 Africa           54.8    -12.4
```

```r
# A: These countries all have negative residuals meaning a lower lifeExp
  # than the average lifeExp of the corresponding continent
```


```r
# (LC5.7) Repeat this process, but identify the five countries with the five largest (most positive) residuals. What do these positive residuals say about their life expectancy relative to their continents’ life expectancy?

regression_points %>% 
  slice_max(residual, n = 5)
```

```
## # A tibble: 5 x 5
##   country   lifeExp continent lifeExp_hat residual
##   <fct>       <dbl> <fct>           <dbl>    <dbl>
## 1 Reunion      76.4 Africa           54.8     21.6
## 2 Libya        74.0 Africa           54.8     19.1
## 3 Tunisia      73.9 Africa           54.8     19.1
## 4 Mauritius    72.8 Africa           54.8     18.0
## 5 Algeria      72.3 Africa           54.8     17.5
```

```r
# A: These countries all have positive residuals meaning a higher lifeExp
  # than the average lifeExp of the corresponding continent
```

### 5.3 Related topics
#### 5.3.1 Correlation is not necessarily causation


```r
# Compute the sum of squared residuals
score_model %>% 
  get_regression_points() %>% 
  mutate(squared_residual = residual^2 ) %>% 
  summarise(sum_of_squared_residuals = sum(squared_residual))
```

```
## # A tibble: 1 x 1
##   sum_of_squared_residuals
##                      <dbl>
## 1                     132.
```


```r
# (LC5.8) Note in Figure 5.13 there are 3 points marked with dots and:

  #  The “best” fitting solid regression line in blue
  #  An arbitrarily chosen dotted red line
  #  Another arbitrarily chosen dashed green line

# Compute the sum of squared residuals by hand for each line and show that of these three lines, the regression line in blue has the smallest value.

red_dotted <- (c((2 - 2.5), (1 - 2.5), (3 - 2.5))^2) %>% sum()
green_dashed <- (c((2 - 2), (1 - 1), (3 - 1))^2) %>% sum()
best_fit <- (c((2 - 1.5), (1 - 2), (3 - 2.5))^2) %>% sum()

best_fit < red_dotted & green_dashed
```

```
## [1] TRUE
```


```r
clear_libraries()
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
