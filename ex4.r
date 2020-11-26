




# creating data frame using data table given 

Praise= c(4,7,5,3,6,4,6)
Criticism = c(7,6,9,7,8,6,7)
None= c(7,3,7,5,7,8,5)
mydata<- data.frame(Praise,Criticism,None)                
mydata

# initial plotting to check distribution

hist(mydata$Praise,20)  
shapiro.test(mydata$Praise)

hist(mydata$Criticism,20)  
shapiro.test(mydata$Criticism)

hist(mydata$None,20)  
shapiro.test(mydata$None)

# loading library

library(car)

library(psych)

describe(mydata)

# stacking data

exdata=stack(mydata)

# box plot so check normality

boxplot(values~ind,exdata)

# Checking assumptions quantitatively 

by(exdata$values,exdata$ind,shapiro.test)

# Homogeneity of variance
leveneTest(values~ind,exdata)

# Running ANOVA using aov
x=aov(values~ind,exdata)

# summary of ANOVA results
summary(x)





