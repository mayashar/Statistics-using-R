
#loading libraries equired
library(ggplot2)
library(pgirmess)
library(DescTools)

# Reading file
data = read.csv("drugTx.csv" )

data #looking into data , seems there is NA

colnames(data)
# initial plotting to check distribution

boxplot(data)

# checking NA

data[!complete.cases(data),]

# removing NA

clean_data<-na.omit(data)

clean_data #checking again


# plotting again to check distribution

boxplot(clean_data)

# stacking data

stacked_data<-stack(clean_data)
stacked_data



## plotting
ggplot(stacked_data) + aes(x = ind, y = values) + geom_boxplot(fill = "orange") + theme_minimal()



## Planned comparisons (no protection for alpha inflation)
pairwise.t.test(stacked_data$values,stacked_data$ind,paired=FALSE,p.adjust.method="none")

#pairwise.t.test(stacked_data$values,stacked_data$ind,paired=TRUE,p.adjust.method="none")
## Bonferroni
pairwise.t.test(stacked_data$values,stacked_data$ind,paired=FALSE,p.adjust.method="bonferroni")
## Holm
pairwise.t.test(stacked_data$values,stacked_data$ind,paired=FALSE,p.adjust.method="holm")

# Running ANOVA using aov
x=aov(values~ind,stacked_data)

# summary of ANOVA results
summary(x)

## post-hocs 
## Simple Tukey's HSD
TukeyHSD(x) 
TukeyHSD(x, conf.level=0.99) 

## Dunnett's Test
stacked_data$ind <- as.factor(stacked_data$ind)
#DunnettTest(x = stacked_data$values, g = stacked_data$ind, control = 'None')


# Nonparametrics

## Multiple comparisons with wilcox rank sum 

pairwise.wilcox.test(clean_data$placebo,clean_data$low_Dose,paired=FALSE,p.adjust.method="none",exact=TRUE)
pairwise.wilcox.test(clean_data$placebo,clean_data$high_Dose,paired=FALSE,p.adjust.method="none",exact=TRUE)
pairwise.wilcox.test(clean_data$low_Dose,clean_data$high_Dose,paired=FALSE,p.adjust.method="none",exact=TRUE)

# Multiple comparions with kruskalmc 

kruskalmc(clean_data$placebo~clean_data$high_Dose, data=clean_data, probs = 0.01)

kruskalmc(clean_data$placebo~clean_data$high_Dose, cont="two-tailed") # restricts number of comparisons

# linear regression
model = lm(stacked_data$values~stacked_data$ind)
summary(model)

# plotting #

barplot(stacked_data$values,col=c("gray", "red","blue"))
legend("topleft", legend=c("placebo","high_Dose", "low_Dose"),fill = c("gray", "red","blue"))
                                                              



