
# data loading and exploring

Novel_Data=read.csv("Novel_SSRI.csv",header=TRUE)[ ,2:3]

## Compute and look at MEAN & SD for each level of the IV
ymean=apply(Novel_Data,2,mean)
ysd=apply(Novel_Data,2,sd)

# creating stacks

Novel_subject=stack(Novel_Data)
names(Novel_subject)

# Data visulaization and Graph errorbar plot 
#install.packages("Hmisc")
library(ggplot2) # loading ggplot2
graph=ggplot(Novel_subject,aes(Novel_subject$ind,Novel_subject$values))
# fun.y applies to specific point, whereas fun.data applies to all of the data
# mean_cl_normal = 95% confidence interval
graph+stat_summary(fun.y=mean, geom="bar")+stat_summary(fun.data=mean_cl_normal,geom="errorbar")
# mean_sdl = mean +/- std
graph+stat_summary(fun.y=mean, geom="bar")+stat_summary(fun.data=mean_sdl,geom="errorbar")
# mean_sdl = mean +/- se
graph+stat_summary(fun.y=mean, geom="bar")+stat_summary(fun.data=mean_se,geom="errorbar")

# Overall Graph looks good 

#Hypothesis:
# H0: μ1 = μ2   (Larger score means more depression symptoms)
# H1: μ1! = μ2  (Larger score does not mean more depression symptoms)

#Testing:
  
# Checking Normality
## Need to check each level of the IV separately

hist(Novel_Data$Placebo,20)         # Qualitative
shapiro.test(Novel_Data$Placebo)    # Quantitative

#seems normality violation

#Removal of outlier required
k=which(Novel_Data$Placebo!=max(Novel_Data$Placebo))
clean_Novel_Data=Novel_Data[k,]

hist(clean_Novel_Data$Placebo,20)         # Qualitative
shapiro.test(clean_Novel_Data$Placebo)    # Quantitative

# Now all look normal
# rechecking for second variable

hist(clean_Novel_Data$Novel.SSRI,20)
shapiro.test(clean_Novel_Data$Novel.SSRI)

# stacking again
Novel_subject=stack(clean_Novel_Data)
names(Novel_subject)

# Checking homogeneity of variance since Levene test is good for parametric 
library(car)
leveneTest(Novel_subject$values ~ Novel_subject$ind)

# One sample t-test
t.test(Novel_subject$values, alt="two.sided", mu=6.7)

# Two sample paired (dependent) t-test
t.test(Novel_subject$values ~ Novel_subject$ind, paired=T, alt="two.sided",var.equal=T)

# Two sample unpaired (independent) t-test
t.test(Novel_subject$values ~ Novel_subject$ind, paired=F, alt="two.sided",var.equal=T)

# fit linear model (compare t-value and p-value with independent t-test!)

res=lm(Novel_subject$values ~ Novel_subject$ind)
summary(res)

# Loading and exploring data file

Drug_Data=read.csv("New_Drug.csv",header=TRUE)[ ,2:3]

Drug_Data=data.frame(Drug_Data)

library(ggplot2)
ggplot(Drug_Data) +
  aes(x = Tx, y = Score) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#boxplot(Score~Tx,Drug_Data)

# visualizing distribution

hist(subset(Drug_Data, Tx== "Drug")$Score,
     main = "Drug Score",
     xlab = "Score"
)

hist(subset(Drug_Data, Tx== "Placebo")$Score,
     main = "Placebo Score",
     xlab = "Score"
)

# Hypothesis is:
#H0: Treatment method = Score (no effect of any treatment method)
#H1: Treatment method != Score (Scores are effected by methods of treatment)


#Testing:
by(Drug_Data$Score,Drug_Data$Tx,shapiro.test)

# Checking homogeneity of variance using levene test

library(car)
leveneTest(Drug_Data$Score ~ Drug_Data$Tx)

# Run ANOVA using aov
x=aov(Drug_Data$Score ~ Drug_Data$Tx)
# look at summary of ANOVA results
summary(x)

## Effect Size

library(effectsize)
eta_squared(x, partial = T, ci = 0.95)
omega_squared(x, partial = T, ci = 0.95)

# p-value is <0.05 , very low indeed *** indicates there is effect of treatment on score, even small sample size

# Assuming that outcome is NOT normally dictributed Non-Parametric test is the performed

fligner.test(Drug_Data$Score ~ Drug_Data$Tx, Drug_Data)

kruskal.test(Drug_Data$Score ~ Drug_Data$Tx, Drug_Data) 


