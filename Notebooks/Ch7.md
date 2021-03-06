---
title: "ModernDive Ch7: Sampling"
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
library(patchwork)

# Load function to clear libraries
source(here::here("clear_libraries.R"))
```
### 7.1 Sampling bowl activity
#### 7.1.1 What proportion of this bowl’s balls are red?


```r
# Results from the '33 friends' experiment
tactile_prop_red
```

```
## # A tibble: 33 x 4
##    group            replicate red_balls prop_red
##    <chr>                <int>     <int>    <dbl>
##  1 Ilyas, Yohan             1        21     0.42
##  2 Morgan, Terrance         2        17     0.34
##  3 Martin, Thomas           3        21     0.42
##  4 Clark, Frank             4        21     0.42
##  5 Riddhi, Karina           5        18     0.36
##  6 Andrew, Tyler            6        19     0.38
##  7 Julia                    7        19     0.38
##  8 Rachel, Lauren           8        11     0.22
##  9 Daniel, Caroline         9        15     0.3 
## 10 Josh, Maeve             10        17     0.34
## # ... with 23 more rows
```


```r
# Histogram
tactile_prop_red %>% 
  ggplot(aes(prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", 
       title = "Distribution of 33 proportions red")
```

![plot of chunk unnamed-chunk-3](Ch7/unnamed-chunk-3-1.png)

#### 7.1.4 What did we just do?


```r
# (LC7.1) Why was it important to mix the bowl before we sampled the balls?

# A: Randomization. So that each ball has an equal chance to be picked by the shovel. The sampled balls are put back every time to replicate the environment of the first the sampling event.

# (LC7.2) Why is it that our 33 groups of friends did not all have the same numbers of balls that were red out of 50, and hence different proportions red?

# A: Because each sample is smaller in size than the total number of balls and each ball is selected randomly, the set of selected balls will be different every time (it is possible to get exactly the same balls on several trials). This is sampling variation in action.

# (LC7.3) Why couldn’t we study the effects of sampling variation when we used the virtual shovel only once? Why did we need to take more than one virtual sample (in our case 33 virtual samples)?

# A: Sampling variation measures the difference between samples, so for that reason, more than one sample is needed.

# (LC7.4) Why did we not take 1000 “tactile” samples of 50 balls by hand?

# A: This would give a better sampling distribution but would be a very tedious job to do.

# (LC7.5) Looking at Figure 7.9, would you say that sampling 50 balls where 30% of them were red is likely or not? What about sampling 50 balls where 10% of them were red?

# A: 200/1000 had 30-35% red and ~140/1000 25-30% red. 30% is very likely.
  # ~5/1000 samples had <20% red, so 10% red seems unlikely.
```

### 7.2 Virtual sampling
#### 7.2.1 Using the virtual shovel once


```r
# Virtual bowl
bowl
```

```
## # A tibble: 2,400 x 2
##    ball_ID color
##      <int> <chr>
##  1       1 white
##  2       2 white
##  3       3 white
##  4       4 red  
##  5       5 white
##  6       6 white
##  7       7 red  
##  8       8 white
##  9       9 red  
## 10      10 white
## # ... with 2,390 more rows
```


```r
# Run each of the following code segments individually and then compare the three resulting histograms.

# Segment 1: sample n = 25 ------------------------------
# 1.a) Virtually use shovel 1000 times
virtual_samples_25 <- bowl %>% 
  rep_slice_sample(n = 25, reps = 1000)
```

```
## Error in rep_slice_sample(., n = 25, reps = 1000): could not find function "rep_slice_sample"
```

```r
# 1.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_25 <- virtual_samples_25 %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 25)
```

```
## Error in group_by(., replicate): object 'virtual_samples_25' not found
```

```r
# 1.c) Plot distribution via a histogram
ggplot(virtual_prop_red_25, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 25 balls that were red", title = "25") 
```

```
## Error in ggplot(virtual_prop_red_25, aes(x = prop_red)): object 'virtual_prop_red_25' not found
```


```r
# Segment 2: sample n = 50 ------------------------------
# 2.a) Virtually use shovel 1000 times
virtual_samples_50 <- bowl %>% 
  rep_slice_sample(n = 50, reps = 1000)
```

```
## Error in rep_slice_sample(., n = 50, reps = 1000): could not find function "rep_slice_sample"
```

```r
# 2.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_50 <- virtual_samples_50 %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 50)
```

```
## Error in group_by(., replicate): object 'virtual_samples_50' not found
```

```r
# 2.c) Plot distribution via a histogram
ggplot(virtual_prop_red_50, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", title = "50")  
```

```
## Error in ggplot(virtual_prop_red_50, aes(x = prop_red)): object 'virtual_prop_red_50' not found
```


```r
# Segment 3: sample n = 100 ------------------------------
# 3.a) Virtually using shovel with 100 slots 1000 times
virtual_samples_100 <- bowl %>% 
  rep_slice_sample(n = 100, reps = 1000)
```

```
## Error in rep_slice_sample(., n = 100, reps = 1000): could not find function "rep_slice_sample"
```

```r
# 3.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_100 <- virtual_samples_100 %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 100)
```

```
## Error in group_by(., replicate): object 'virtual_samples_100' not found
```

```r
# 3.c) Plot distribution via a histogram
ggplot(virtual_prop_red_100, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 100 balls that were red", title = "100") 
```

```
## Error in ggplot(virtual_prop_red_100, aes(x = prop_red)): object 'virtual_prop_red_100' not found
```


```r
# Standard deviations

# n = 25
virtual_prop_red_25 %>% 
  summarize(sd = sd(prop_red))
```

```
## Error in summarize(., sd = sd(prop_red)): object 'virtual_prop_red_25' not found
```

```r
# n = 50
virtual_prop_red_50 %>% 
  summarize(sd = sd(prop_red))
```

```
## Error in summarize(., sd = sd(prop_red)): object 'virtual_prop_red_50' not found
```

```r
# n = 100
virtual_prop_red_100 %>% 
  summarize(sd = sd(prop_red))
```

```
## Error in summarize(., sd = sd(prop_red)): object 'virtual_prop_red_100' not found
```

```r
# As the sample size increases, our guesses at the true proportion of the bowl’s balls that are red get more precise
```


```r
# (LC7.6) In Figure 7.9, we used shovels to take 1000 samples each, computed the resulting 1000 proportions of the shovel’s balls that were red, and then visualized the distribution of these 1000 proportions in a histogram. We did this for shovels with 25, 50, and 100 slots in them. As the size of the shovels increased, the histograms got narrower. In other words, as the size of the shovels increased from 25 to 50 to 100, did the 1000 proportions

  #  A. vary less, <== 
  #  B. vary by the same amount, or
  #  C. vary more?

  # Answer: A.

# (LC7.7) What summary statistic did we use to quantify how much the 1000 proportions red varied?

  #  A. The interquartile range
  #  B. The standard deviation <== 
  #  C. The range: the largest value minus the smallest.

  # Answer: B.
```


#### 7.3.1 Terminology and notation


```r
# Virtual bowl census
bowl %>% 
  summarise(red = sum(color == "red"))
```

```
## # A tibble: 1 x 1
##     red
##   <int>
## 1   900
```


```r
# Sampling using a shovel with 50 slots

virtual_shovel <- 
  bowl %>% 
  rep_slice_sample(n = 50)
```

```
## Error in rep_slice_sample(., n = 50): could not find function "rep_slice_sample"
```

```r
virtual_shovel
```

```
## Error in eval(expr, envir, enclos): object 'virtual_shovel' not found
```


```r
virtual_shovel %>% 
  summarise(num_red = sum(color == "red")) %>% 
  mutate(prop_red = num_red / 50)
```

```
## Error in summarise(., num_red = sum(color == "red")): object 'virtual_shovel' not found
```


```r
# (LC7.8) In the case of our bowl activity, what is the population parameter? Do we know its value?

# A: The population parameter is Population_proportion = 0.375

# (LC7.9) What would performing a census in our bowl activity correspond to? Why did we not perform a census?

# A: Counting all the balls in the bowl and calculating the Pop. parameter
  # We already knew the pop.parameter, also counting a census is a lot of work. We performed random sampling to estimate the population parameter.

# (LC7.10) What purpose do point estimates serve in general? What is the name of the point estimate specific to our bowl activity? What is its mathematical notation?

# A: A point estimate serves as our 'best guess' of the real underlying population parameter. In this case the point estimate is sample_proportion. The mathematical notation is p_hat.

# (LC7.11) How did we ensure that our tactile samples using the shovel were random?

# A: We mixed the contents of the bowl and used an unbiased sampling method (the shovel)

# (LC7.12) Why is it important that sampling be done at random?

# A: So that each single ball has an equal chance of being sampled from the bowl. This keeps the sample representative so that the results from the sample can generalize to the underlying population.

# (LC7.13) What are we inferring about the bowl based on the samples using the shovel?

# A: We are trying to infer the proportion of red balls in the bowl by doing random sampling and calculating point estimates for the population proportion.

# Book answer: We are inferring that the samples are representing the total population in the ball.
```

#### 7.3.2 Statistical definitions

```r
# (LC7.14) What purpose did the sampling distributions serve?

# A: The sampling distributions show the effect of sample size on sample variance. Book: Using the sampling distributions, for a given sample size n, we can make statements about what values we can typically expect.

# (LC7.15) What does the standard error of the sample proportion p_hat quantify?

# A: It quantifies the standard deviation of the sample_proportion (point estimates) between samples. This standard deviation is known as standard error. Book: Standard errors quantify the effect of sampling variation induced on our estimates.
```

#### 7.3.3 The moral of the story


```r
# (LC7.16) The table that follows is a version of Table 7.3 matching sample sizes n to different standard errors of the sample proportion ˆp, but with the rows randomly re-ordered and the sample sizes removed. 

# Fill in the table by matching the correct sample sizes to the correct standard errors. For the following four Learning checks, let the estimate be the sample proportion ˆp: the proportion of a shovel’s balls that were red. It estimates the population proportion p: the proportion of the bowl’s balls that were red.

# A:
tribble(~Sample_size, ~Standard_error_of_p_hat,
        25, 0.094,
        50, 0.045,
        100, 0.069)
```

```
## # A tibble: 3 x 2
##   Sample_size Standard_error_of_p_hat
##         <dbl>                   <dbl>
## 1          25                   0.094
## 2          50                   0.045
## 3         100                   0.069
```


```r
# (LC7.17) What is the difference between an accurate and a precise estimate?

# A: Accurate estimate is 'on target' = close to the real value of pop.param. Precision is the consistency of a set of estimates (not for sigle estimate). Book: An accurate estimate gives an estimate that is close to, but not necessary the exact, actual value. A precise estimate gives the exact actual value.

# (LC7.18) How do we ensure that an estimate is accurate? How do we ensure that an estimate is precise?

# A: Accuracy is ensured by using unbiased sampling. Precision is ensured by taking large enough sample from the population. Book: To ensure that an estimate is accurate, we need to have a reasonable range of estimate, and make sure that the estimate is reasonably close to the actual value To ensure that an estimate is precise, we need to make sure the estimate is equivalent to the actual value.??

# (LC7.19) In a real-life situation, we would not take 1000 different samples to infer about a population, but rather only one. Then, what was the purpose of our exercises where we took 1000 different samples?

# A: The purpose was to demonstrate the the effects of sample size on sampling variation and further the effect if this variation to sampling estimates. Book: To get a narrower range of the estimates.??

# (LC7.20) Figure 7.13 with the targets shows four combinations of “accurate versus precise” estimates. Draw four corresponding sampling distributions of the sample proportion p_hat, like the one in the leftmost plot in Figure 7.12.
```

#### 7.4 Case study: Polls


```r
# Comment on the representativeness of the following sampling methodologies:

# (LC7.21) The Royal Air Force wants to study how resistant all their airplanes are to bullets. They study the bullet holes on all the airplanes on the tarmac after an air battle against the Luftwaffe (German Air Force).

# A: This has survivor bias. Only planes that _came back_ from the battle can be studied; therefore, this is not a representative sample

# (LC7.22) Imagine it is 1993, a time when almost all households had landlines. You want to know the average number of people in each household in your city. You randomly pick out 500 phone numbers from the phone book and conduct a phone survey.

# A: This is not representative. There might be biases based on: Who answered the phone? How many people live in the apartment? When are the people home (night workers...) Not all homes are listed in the book etc.  Book: This is not a good representation, because: (1) adults are more likely to pickup phone calls; (2) households with more people are more likely to have people to be available to pickup phone calls; (3) we are not certain whether all households are in the phone book.

# (LC7.23) You want to know the prevalence of illegal downloading of TV shows among students at a local college. You get the emails of 100 randomly chosen students and ask them, “How many times did you download a pirated TV show last week?”.

# A: Most definitely not representative. The investigator is assuming that people will freely admit to committing crimes. Book: This is not a good representation, because it is very likely that students will lie in this survey to stay out of trouble. So we may not get honest data. This is called volunteer bias: systematic error due to differences between those who choose to participate in studies and those who do not.

# (LC7.24) A local college administrator wants to know the average income of all graduates in the last 10 years. So they get the records of five randomly chosen graduates, contact them, and obtain their answers.

# A. This sample is way too small, only 5 graduates for 10 years. Firstly, to make it representative for all the years in the timeline, the sampling should be done for each year of the 10 years separately and with more graduates per year. Many of the 'chosen' people are likely to decline taking part in the study anyway as this is sensitive personal information. The requirement for sample size might depend on the student structure of the school (diversity in multiple ways), but not sampling each year separately (then pooling the results), might lead to low accuracy (some years over/underrepresented). Book: This is not a good representation, because the sample size is too small. The sample is representative but not precise.
```


```r
clear_libraries()
```
