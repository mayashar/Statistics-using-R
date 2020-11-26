---
title: "Sharma_Homework_03"
author: "maya"
date: "September 26, 2020"
output: word_document
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```
Please answer the following questions. These assume you have performed the readings and watched each of the lectures up to this point. Where necessary, please print your R output as your answer. 

1: Please find the examScores.xlsx file (Canvas > Files > homeworks > homework_03) and determine if exam1 scores are related to exam2 scores. (5 points)

```{r}
# Libraries needed 
library(ggplot2)
library(tidyverse)
#library(car)
library(readxl)
library(dplyr)
library(car)
# reading and exploring Excel file
exam_scores <- read_excel("examScores.xlsx")
#glimpse(exam_scores)
exam_scores <-data.frame(exam_scores) # creating data frame
#working on variables required
Exam1<-exam_scores$Exam1
Exam2<-exam_scores$Exam2
## make graph object
graph <- ggplot(exam_scores,aes(Exam1,Exam2))
## creating a scatter plot
graph+geom_point()
# Pearsons Correlation
cor.test(Exam1,Exam2,alternative="two.sided",method="pearson", conf.level=0.95)

```

Since it has been clear here that correlation which is not equal to 0 so relation do exist between scores of Exam1 and Exam2 
and there are positive values more than zero to 1, there is positive relation between scores of two exams with small effect.



2: Can exam1 scores be used as an accurate predictor of exam2 scores? Please provide the model that captures the relationship between these two variables. Please plot your model as well. (5 points)
```{r}
### performing simple least squares regression
lm_scores=lm(Exam2~Exam1,data=exam_scores)
print(lm_scores)
# display statistical summary of the regression
summary(lm_scores)

```

Since, p-value is less than 0.05 so there is accuracy in the predictor but
Its not good idea to take p-value as a single factor to decide accuracy. According to ASA (American statistical association), lots of other factors such as previously done analysis on similar data, how properly the analysis is done and in case of lack of evidences the reproducibility should be checked well before stepping to the concrete decision.  
```{r}

# plotting the model
## graph scatter plot with linear model (lm)
graph <- ggplot(exam_scores,aes(Exam1,Exam2))
## create a scatter plot + regression line
graph+geom_point()+geom_smooth(method="lm",se=T)
## create a scatter plot + regression line and 99% CI
graph+geom_point()+geom_smooth(method="lm", level=0.99)

## Normality check
### Qualitative , plotting histograms with bin 30
hist(exam_scores$Exam1,30) 
hist(exam_scores$Exam2,30)
# Spearman's Correlation (there is outlier present in Exam1)
cor.test(Exam1,Exam2,alternative="two.sided",method="spearman", conf.level=0.95)
# Getting rid of the warning message 
cor.test(Exam1,Exam2,alternative="two.sided",method="spearman", conf.level=0.95, exact=FALSE)
### Quantitative
shapiro.test(exam_scores$Exam1)
shapiro.test(exam_scores$Exam2)
## As outlier is present, possibly influencing the regression
hist(exam_scores$Exam1, 30)
graph <- ggplot(exam_scores,aes(Exam1,Exam2))
graph+geom_point()
### rerun model to 
lm_scores=lm(exam_scores$Exam2~exam_scores$Exam1,data=exam_scores)
influencePlot(lm_scores)
### Quantitative--> nonconstant variance test
ncvTest(lm_scores)
###filtering extreme value
k=which(exam_scores$Exam1!=min(exam_scores$Exam1))
cleanData=exam_scores[k,]
graph <- ggplot(cleanData,aes(Exam1,Exam2))
graph+geom_point()
lm_scoresClean=lm(exam_scores$Exam2~exam_scores$Exam1,data=cleanData)
influencePlot(lm_scoresClean)

```
Since, p-value is less than 0.05 so there is accuracy in the predictor but Its not good idea to take p-value as a single factor to decide accuracy. According to ASA (American statistical association), lots of other factors such as previously done analysis on similar data, how properly the analysis is done and in case of lack of evidences the reproducibility should be checked well before stepping to the concrete decision.cookD is below 1 which indicates there is no extreme scores. Overall the model is accurate.

