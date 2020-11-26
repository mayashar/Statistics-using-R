#install.packages("gplots")
library(gplots)
library(ggplot2)
library(foreign)
library(car)
##reading and exploring data file 
Data=read.csv("cholesterol.csv" ,header = TRUE)
summary(Data)
colnames(Data)

# The data contains a Cholesterol(dependent variable-y axis), a group  with three groups (independent variable-State), and a covariate Age

# Initial Plots too look into data

hist(Data$Cholesterol)
hist(Data$Age)


# checking normality by state with cholesterol
by(Data$Cholesterol, Data$State, shapiro.test)
boxplot(Data$Cholesterol ~ Data$State)

# California: W = 0.94602, p-value = 0.366>0.05
# Iowa:W = 0.91508, p-value = 0.2797>0.05
# Nebraska: W = 0.91796, p-value = 0.1038>0.05
# no normality violation

# checking normality by age and state
by(Data$Age, Data$State, shapiro.test)
boxplot(Data$Age~Data$State)


# California: W = 0.94486, p-value = 0.3502>0.05
# Iowa:W = W = 0.97287, p-value = 0.9139>0.05
# Nebraska:W = 0.96795, p-value = 0.7348>0.05
# no normality violation

# draw boxplot by group
boxplot <- ggplot(Data, aes(Age, Cholesterol))
boxplot + geom_boxplot() + facet_wrap(~State) + labs(x="Age", y ="Cholesterol")


## Assumptions of ANCOVA I: Are the IV(state) and CV(age) correlated?
# Qualitative assessment
plotmeans(Data$Age ~ Data$State)

# Quantitative assessment
x=aov(Age~State,data=Data)
summary(x)

# F(2, 45) = 0.82, p = 0.44>0.05 . This means that for these data the variances are very similar.

#Levene’s test
Data$State<-as.factor(Data$State)
leveneTest(Data$Age, Data$State, center = median)

# F(2, 45) = 0.53, p = 0.58>0.05 . This means that for these data the variances are very similar.

# All show that there is no statistical relationship between state and age

#Assumption of ANCOVA II: Are DV(Cholesterol) and CV(age) correlated?

# Qualitative assessment
plotmeans(Data$Cholesterol~Data$Age)

graph <- ggplot(Data,aes(Age, Cholesterol))
graph + geom_point()

#Levene’s test
agex<-as.factor(Data$Age)
leveneTest(Data$Cholesterol, agex, center = median)

# F(36, 11) = 1.4, p = 0.27>0.05 . no normality violation.

# Quantitative assessment
x=lm(Cholesterol ~ Age, data=Data)
summary(x)

# F(1, 46) = 2.48, p = 1.74e-09 ***<0.05 . looks like there is strong relationship between age and cholesterol.

# **** Assumption of ANCOVA III: Homogeneity of regression?
# qualitative assesment
graph <- ggplot(Data, aes(Age, Cholesterol, colour = State))
graph + geom_point(aes(shape = State), size = 3) + 
  geom_smooth(method = "lm", aes(fill = State), alpha = 0.1) +
  labs(x = "Age", y = "Cholesterol level")


## Clearly see the California is very different
# there is negative slope  between cholesterol and age in california state
# positive relationship between cholesterol and age can be seen here in iowa and nebraska which looks normal



# Quantitative assessment
x=aov(Cholesterol~Age*State,data=Data)
summary(x)

# F(36, 2,7) = 34.6, 3.12,40.09, p = (0.0284 *,0.2427, 0.0245 *)<0.05 
# The interaction here is significant so homogeneity of regression is violated

# Run ANCOVA (formula = (DV~CV+IV))

# 1)give up: not chosen this here as its better to check further  
# 2) Interpret interaction :california changes relationship between age and cholesterol destroying positive relationships here.

# 3) Removing "California" and repeat analyses

k=which(Data$State!="California")
cleanData=Data[k,]


## Assumptions of ANCOVA I: Are the IV(state) and CV(age) correlated?

# Qualitative assessment
plotmeans(cleanData$Age~cleanData$State)

# Quantitative assessment

leveneTest(cleanData$Age,cleanData$State, center = median)

#F(1,28) = 0.95, p = 0.33>0.05

#anova

x=aov(Age~State,data=cleanData)
summary(x)


#F(1,28) = 1.34, p = 0.25>0.05

# looks like there is no relationships between state and age.

#Assumption of ANCOVA II: Are DV(Cholesterol) and CV(age) correlated?

# Qualitative assessment
plotmeans(cleanData$Cholesterol~cleanData$Age)

graph <- ggplot(cleanData,aes(Age, Cholesterol))
graph + geom_point()

# looks like there is significant relationship between age and cholesterol

#Levene’s test
agey<-as.factor(cleanData$Age)
leveneTest(cleanData$Cholesterol, agey, center = median)

# F(25,4) = 0.0989, p =  0.9999>0.05 .There is no violation in assumption.

# Quantitative assessment
x=lm(Cholesterol ~ Age, data=cleanData)
summary(x)

# F(1,28) =  19.29 , p = 0.0001457<0.05 . looks like there is significant relationship between age and cholesterol

# **** Assumption of ANCOVA III: Homogeneity of regression?
# qualitative assesment
graph <- ggplot(cleanData, aes(Age, Cholesterol, colour = State))
graph + geom_point(aes(shape = State), size = 3) + 
  geom_smooth(method = "lm", aes(fill = State), alpha = 0.1) +
  labs(x = "Age", y = "Cholesterol level")


# positive relationship between cholesterol and age can be seen here in iowa and nebraska which looks normal


# Quantitative assessment
x=aov(Cholesterol~Age*State,data=cleanData)
summary(x)



# Post hocs
TukeyHSD(x)
# F(1,1) =  19.29 , p = (5.33e-05 ***,0.0104 *)<0.05 . 
#looks like there is significant relationship between age and cholesterol in two states here.
# but for age and state p=0.68>0.05 so no correlation there.


leveneTest(cleanData$Cholesterol, cleanData$State, center = median)


# F(1,28) =  0.38 , p = 0.53>0.05 . looks like there is significant relationship between cholesterol and state here.


# Post hocs
Data$State = as.factor(Data$State)

TukeyHSD(x, which = 'State')

## looking into three levelsi.e.x=aov(Cholesterol~Age+State,data=Data);
x=aov(Cholesterol ~ Age + State, data=Data)
summary(x)

# F(1,2) =(2.5,1.3) , p = (0.12,0.28)>0.05 .looks like there is no significant relationship between cholesterol and age in states here.

# Post hocs
cleanData$State = as.factor(cleanData$State)

TukeyHSD(x, which = 'State')

# looking into all 3 levels in two states.  Running ANCOVA (formula = (DV~CV+IV))
x=aov(Cholesterol ~ Age + State, data=cleanData)
summary(x)

# To get type III Sum of Squares use Anova from car package
x=aov(Cholesterol ~ Age * State, data=cleanData)
summary(x)
Anova(x,type="III")
# F(1,2) =(2.5,1.3) , p = 0.0311 *<0.05 and hence there strong relationship between age and cholesterol in two states here
## Can also use multcomp
library(multcomp)

postHocs=glht(x, linfct = mcp(State="Tukey"))
summary(postHocs)

# F(1,2) =(2.5,1.3) ,p=0.00922 ** and hence there strong relationship between age and cholesterol

##################################################################################################
##################################################################################################
# Hypothesis:
#H0:Mutation=SCh_Diag (NO effect of genetic mutation on Schizophrenia)
#H1:Mutation!=SCh_Diag (genetic mutation on Schizophrenia diagnosis)

## creating data frame
Mutation_No<-c(25,65)
Mutation_YES<-c(60,50)

# Chi-square test 

chisq.test(Mutation_YES,p= Mutation_No, rescale.p = TRUE)
# X-squared = 39.287, df = 1, p-value = 3.659e-10<0.05
## Checking expected values
model=chisq.test(Mutation_YES,p= Mutation_No, rescale.p = TRUE)
model$expected
# here expected values are >5 and no issue
# Test of independence:
#install.packages("vcd")
library(vcd)
## Create a table manually based on the data set from Scientific American 
Table = matrix(c(25,65,60,50), 2, 2, byrow=TRUE)
dimnames(Table) = list("Conflict"=c("NO", "YES"),"Outcome"=c("NO", "YES"))
res = chisq.test(Table, correct=FALSE)
res

# X-squared = 14.513, df = 1, p-value = 0.0001392<0.05 
res$expected 
# Expected values are >10 so no issue again
# All counts are >10 here 
mosaic(Table, shade=T, legend=TRUE)





