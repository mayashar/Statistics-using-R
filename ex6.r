# loading and reading my researchdata

data.gaain<-read.csv("researchdata/gaain/gaain.csv", header=T,sep = ',')

data.mimic<-read.csv("researchdata/mimic/mimic.csv", header=T,sep = ',')

data.inpc<-read.csv("researchdata/inpc/inpc.csv", header=T,sep = ',')

#2.	Import ps1.csv (Canvas ??? Files??? dataSets) into R and answer the following questions. (10 points)

# *** All data are kept under respective folders into the working directory in R studio ***

# Reading ps1.csv file

data.ps1<-read.csv("datastore/ps1.csv", header=T,sep = ',')

# installing package dplyr

# install.packages("dplyr")

library(dplyr)

#a.	Please use the glimpse() function to view the data. How is this function useful?
# Using glimpse function
glimpse(data.ps1)


#b.	Please show the statistics for only females. To do this, you need to determine which rows describe data from females (use a logical).
#Pull out these rows and put them in a new variable.
#This can/should be be done with 2-3 lines of code, by using logical indexing. 

# Working on gender variable
ps1.gender<-data.ps1$gender

# Working on ethnicity variable
ps1.ethnicity<-data.ps1$ethnicity


# Statistics for only females

ps1.female.stat<-summary(data.ps1[which(ps1.gender=='female'),])
ps1.female.stat

#c.	Please show the statistic for all Hispanic Females. Hint: use the & command

# Statistic for all Hispanic Females

ps1.hisp.female.stat<-summary(data.ps1[which(ps1.gender=='female' & ps1.ethnicity=='Hispanic'),])
ps1.hisp.female.stat
