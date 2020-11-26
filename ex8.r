---
title: "Sharma_Problemset03"
author: "maya"
date: "September 26, 2020"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Problem 1: Discuss the rationale behind the T-distributions. Specifically, what is the rationale for using a family of distributions rather than a single one like the z-distribution? (4 points)


T distribution which is similar to the bell-shaped distribution have higher probability for extreme values or events when even sample is too small. When the number of sample N is less than 30 then it is considered small and hence degree of freedom (determined by sample size) will be equal to one N-1. Shape of it changes with degree of freedom.
The rationale behind T -distribution can be explained simply as “T-Distribution gives a higher probability to extreme events, given a small sample size”. 
In the world of statistics t-distribution as well as z-distribution are commonly used for statistical analysis of data. Both of these needs symmetrical or reasonably normally distributed data. The outlier i.e. extremes in data will bring inaccuracy while testing.
When population standard deviation is known then it would be appropriate to use Z-test but if there is no standard deviation of population given, with the help of size, mean and standard deviation of sample t-test can be done. Hence, t-test assumes that population standard deviation is unknown, observations in data are independent, the sample is random and the null hypothesis is true.


For the following questions please use the heartValve.csv data set (found on Canvas). A description of the variables can be found below. 

Problem 2: You are evaluating the data in the heartValve.csv data set and you’ve become concerned that the data might be biased towards older subjects. However, this is just a hunch and you have no published work to back this up. You do know, however, that the population of people receiving heart valve replacement have an average age of 48.97 years old at the time of surgery. Please assess this hypothesis using a traditional approach where the p-value is calculated from a mathematically defined null distribution (e.g. not generated via bootstrapping or resampling). Be sure to test all assumptions of the test you’re using – if possible. All tests should be carried out in R. (8 Points)

Given Hypothesis:

H0: surgery age == 48.97
H1: surgery age!= 48.97
```{r}
library(tidyverse)
library(infer)
library(moderndive)

# Read data set
heart_valveData <- read.csv("heartValve.csv", header = T)
hist(heart_valveData$AGE,30)

K=which(heart_valveData$AGE!="NA") # collecting not null values
clean_hvage<-heart_valveData[K,]

# ks.test

ks.test(clean_hvage$AGE,"pnorm",mean(clean_hvage$AGE),sd(clean_hvage$AGE))

#here

proMn=48.97 # given

# shapiro.test
shapiro.test(clean_hvage$AGE)


# one sample t-test
t.test(clean_hvage$AGE, alternative = "two.sided", mu = proMn, var.equal = T)

## calculate by hand just to show you the computations 
ageMn = mean(clean_hvage$AGE)

```


Problem 3: Please repeat the above steps using a bootstrapping approach. (8 Points)

```{r}

# bootstrapping approach

number_rept=8000 # number of repetition

mean_age = data.frame(heart_valveData$AGE) # creating data frame

null_distn_one_mean <-mean_age %>%
  specify(response =heart_valveData.AGE) %>%              # working on the variables
  hypothesize(null = "point", mu = proMn) %>%             # Hypothesize the Null
  generate(reps = number_rept) %>%                              # Generating replicates
  calculate(stat = "mean")                                # Get Summary stats

## Look at the Null Dist with Xbar
null_distn_one_mean %>%
  visualize(obs_stat = ageMn, direction = "two_sided")

## get p-value
pvalue <- null_distn_one_mean %>%
  get_pvalue(obs_stat = ageMn, direction = "two_sided")
pvalue

## Double check, does the p-value make sense?
k = which(ageMn<=null_distn_one_mean$stat)
exact_p_value = (1 + length(k)) / (number_rept + 1)
exact_p_value



```

