
#install.packages("ez")
#install.packages("kableExtra")

library(ez)
library(car)
library(ggplot2)
library(dplyr)

# Data sets
StudentData <- read.csv("dopamineTime.csv")
summary(StudentData)

# Prametric tests: Assuming data is normally distributed

# Hypothesis: 
#H0: All means are equal (there is no effect of classes time on the level of dopamine in students)
#H1: All means are not equal (there is effect of classes time on the level of dopamine in students)


## Checking assumptions qualitatively
ggplot(StudentData) +
  aes(x = TimePoint, y = Dopamine) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


# Checking assumptions quantitatively 
by(StudentData$Dopamine,StudentData$TimePoint,shapiro.test)


### Checking homogeneity of variance
leveneTest(StudentData$Dopamine~StudentData$TimePoint,StudentData, center=mean)


#The test reveals a p-value greater than 0.05, indicating that there is no significant difference between the group variance

# Running ANOVA using aov
x=aov(StudentData$Dopamine~StudentData$TimePoint,StudentData)

# summary of ANOVA results
summary(x)

library(effectsize)
## Effect Size
eta_squared(x, partial = T, ci = 0.95)

omega_squared(x, partial = T, ci = 0.95)

## Repeated measures ANOVA
ezANOVA(data=StudentData,dv=.(Dopamine), wid=.(ID), within=.(TimePoint), detailed=T)


## Equivalent Nonparametric Tests : Assuming the data is not normally distributed

# Hypothesis: 
#H0: All means are equal (there is no effect of classes time on the level of dopamine in students)
#H1: All means are not equal (there is effect of classes time on the level of dopamine in students)

## Checking assumptions qualitatively
ggplot(StudentData) +
  aes(x = TimePoint, y = Dopamine) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# Checking assumptions quantitatively using fligner test Checking homogeneity of variance

fligner.test(StudentData$Dopamine~StudentData$TimePoint,StudentData)

fligner.test(StudentData$Dopamine~StudentData$TimePoint)

# Kruskal-Wallis test 
kruskal.test(StudentData$Dopamine~StudentData$TimePoint,StudentData) 


# Frideman test

friedman.test(as.matrix(StudentData))


