# 2)	Using R, plot a normal distribution as a both a PDF and CDF.

# creating example data and plotting cdf and pdf

x=seq(from=-5,to=5,by=0.5)
plot(x,pnorm(x,0,5),type="l",main="Commulative Density Function Example plot",col="red")

plot(x,dnorm(x,0,5),type="l",main="Probability Density Function Example plot",col="red")

# another example by creating example data 
set.seed(2000)
sample_score<-data.frame(gender = factor(rep(c("male","female"), each=200)), 
                   rate_of_happiness = c(rnorm(200),rnorm(200, mean=.8)))

tail(sample_score,5)

plot(sample_score,pnorm(x,0,5),type="l")

plot(sample_score,dnorm(x,0,5),type="l")

library(ggplot2)
# Histogram overlaid with kernel density curve
ggplot(sample_score, aes(x=rate_of_happiness)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="green")  # Overlay with transparent density plot

# Basic ECDF plot
ggplot(sample_score, aes(rate_of_happiness)) + stat_ecdf(geom = "line",color="blue")+
labs(title=" Cumulative Density Function",
     y = "F(rate_of_happiness)", x="rate_of_happiness")+
theme_classic()


#4)	Calculate the Z-score from a score of 80 from a distribution with a mean of 85 and standard deviation of 10. What percent of scores would fall below this score? What percent of scores would fall above this score? 
# z= scale(x,mean,sd)
z=scale(80,85,10)
z

#pnorm(x, mean = , sd = , lower.tail= )
pnorm(80, mean =85 , sd =10 , lower.tail=-0.5 )

# Since z-score is negative and pnorm is 0.6915 i.e. (50+19.15)=69.15% of scorewould fall below mean


# 5) Please go to Canvas and download the 'Programming.csv' data set. Please refer to the table below to define the variables in the data

#loading library
library(dplyr)

# Loading and Exploring file
my_data <- read.csv("programming.csv")

#  defining variables
glimpse(my_data)

# checking columns and exploring chunk of data
names(my_data)

head(my_data,5)

# 5.1) Please calculate the mean and standard deviation for the Final Score in the examinations of all the of Freshmen versus non-Freshmen.  

# selecting required columns to work on

F_Student<-my_data$F
Final_score<-my_data$Score

# creating dataframe and exploring

Std_All<-data.frame(F_Student,Final_score)

names(Std_All)

head(Std_All, 5)

# selecting scores of all and calculating mean and sd for all

fscore<-select(Std_All,Final_score)

mean(Final_score)

sd(Final_score)

# selecting subset for FRESHMEN AND NONFRESHMEN FROM "F" COLUMN
my_data_freshmen_1<-subset(my_data, F ==1)
my_data_not_freshmen_0<-subset(my_data, F ==0)

# Calculating mean and sd for Freshmen

mean(my_data_freshmen_1$Score)

sd(my_data_freshmen_1$Score)

# Calculating mean and sd for NonFreshmen

mean(my_data_not_freshmen_0$Score)

sd(my_data_not_freshmen_0$Score)

# 5.2) Please provide a qualitative assessment of the normality of the final exam scores in Freshmen and non-Freshmen groups.
freshmen<- ggplot(my_data_freshmen_1, aes(Score)) 
freshmen+ geom_density(color="darkblue", fill="lightblue")
freshmen + geom_histogram(aes(y=..density..),binwidth = 0.4) 
freshmen + geom_histogram(aes(y=..density..),binwidth = 0.4) +
                    geom_density(color="darkblue", fill="lightblue", alpha=0.25)

nonfreshmen<- ggplot(my_data_not_freshmen_0, aes(Score)) 
nonfreshmen+ geom_density(color="darkblue", fill="lightblue")
nonfreshmen + geom_histogram(aes(y=..density..),binwidth = 0.4) 
nonfreshmen + geom_histogram(aes(y=..density..),binwidth = 0.4) +
                    geom_density(color="darkblue", fill="lightblue", alpha=0.25)

# In both cases data looks not normal which means scores are not normally ditributed in both groups butfreshmen scores are far better normal than non freshmen which looks skewed towards left.

# 5.3)Please assess statistically if final exam scores are normally distributed in Freshmen and non-Freshmen groups. 

# install.packages("pastecs")

#loading library
library(pastecs)

plot(density(my_data_freshmen_1$Score),main="Estimated pdf freshmen score")

stat.desc(my_data_freshmen_1$Score,basic=T,norm=TRUE) 

shapiro.test(my_data_freshmen_1$Score)

#ks.test(my_data_freshmen_1$Score,"pnorm",mean(my_data_freshmen_1$Score),sd(my_data_freshmen_1$Score))



plot(density(my_data_not_freshmen_0$Score),main="Estimated pdf nonfreshmen score")

stat.desc(my_data_not_freshmen_0$Score,basic=T,norm=TRUE) 

shapiro.test(my_data_not_freshmen_0$Score)

#ks.test(my_data_not_freshmen_0$Score,"pnorm",mean(my_data_not_freshmen_0$Score),sd(my_data_not_freshmen_0$Score))

# here p-value is is less than 0.05 so null hypothesis that data are normally distributed is rejected.

