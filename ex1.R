
# libraries needed
library(ggplot2)
library(readxl)
library(tidyverse)

#1) Please make a scatterplot that compares any of the two variables in the drinks.csv data sets.
dataFileName='./datastore/drinks.csv'
drinks = read.csv(dataFileName,header=T)

graph <- ggplot(drinks,aes(x=spirit_servings, y=wine_servings)) 
graph + geom_point() +
  geom_smooth(method = "lm", se = FALSE)
## `geom_smooth()` using formula 'y ~ x'

#2) Please open the Novel_SSRI.xls data set and make a plot where the bars represent the group means and the errorbars are the standard error of the mean of each group.
Novel_SSRI <- read_excel("./datastore/Novel_SSRI.xls")
## get into Tidy format
Novel_SSRI_tidy <- gather(data=Novel_SSRI, key = Tx, value = Score, -subjectNumber)
# For Tidy format could also use pipe operator %>&=%
Novel_SSRI_tidy <- Novel_SSRI %>%
  gather(key = Tx, value = Score, -subjectNumber)

## Make Data Frame with a Mn and SEM for each group
# create variable mnData which has the means
mnData=c(mean(Novel_SSRI_tidy[Novel_SSRI_tidy$Tx=='Placebo',]$Score), +
           mean(Novel_SSRI_tidy[Novel_SSRI_tidy$Tx=='Drug',]$Score))

# create variable sdData which has the St. Dev. 
sdData=c(sd(Novel_SSRI_tidy[Novel_SSRI_tidy$Tx=='Placebo',]$Score), +
           sd(Novel_SSRI_tidy[Novel_SSRI_tidy$Tx=='Drug',]$Score))

# create variable semData which has the SEM 
semData=sdData/sqrt(13) # <-- 13 = Bad habit!!!

# Create a label for each row
label=c("Placebo","Tx")
#Create a data frame with all of the needed info
ssriData=data.frame(label,mnData,sdData,semData)

## Plot the data
graph <- ggplot(ssriData,aes(label,mnData))
graph + geom_bar(stat="identity") + # stat="identity" tells R to use the values provided instead of counts (default)
  geom_errorbar(aes(ymin = mnData-semData, ymax = mnData+semData)) 
## Notice the huge error bars in the Placebo group? 
# plot each data point
graph <- ggplot(Novel_SSRI_tidy,aes(Tx,Score))
graph + geom_point(stat="identity")
## Indicates a problem. Remove the data and replot
Novel_SSRI_tidy_clean=Novel_SSRI_tidy[Novel_SSRI_tidy$subjectNumber!=11,]
## repeat steps above to compute mean and sd

#3) Please open the DeprScore.csv data set and make a plot where the bars represent the group means and the errorbars are the standard error of the mean of each group. Please plot the bars that refer to treatment in different colors.
## make the data frame
#means
dataFileName='./datastore/DeprScore.csv'
DeprScore = read.csv(dataFileName,header=T)
mnData=c(mean(DeprScore[DeprScore$Exercise=='None' &  DeprScore$Treatment=='Placebo',]$Score), +
           mean(DeprScore[DeprScore$Exercise=='Light' & DeprScore$Treatment=='Placebo',]$Score), +
           mean(DeprScore[DeprScore$Exercise=='Heavy' & DeprScore$Treatment=='Placebo',]$Score), +
           mean(DeprScore[DeprScore$Exercise=='None' & DeprScore$Treatment=='Tx',]$Score), +
           mean(DeprScore[DeprScore$Exercise=='Light' & DeprScore$Treatment=='Tx',]$Score), +
           mean(DeprScore[DeprScore$Exercise=='Heavy' & DeprScore$Treatment=='Tx',]$Score))

#sd
sdData=c(sd(DeprScore[DeprScore$Exercise=='None' &  DeprScore$Treatment=='Placebo',]$Score), +
           sd(DeprScore[DeprScore$Exercise=='Light' & DeprScore$Treatment=='Placebo',]$Score), +
           sd(DeprScore[DeprScore$Exercise=='Heavy' & DeprScore$Treatment=='Placebo',]$Score), +
           sd(DeprScore[DeprScore$Exercise=='None' & DeprScore$Treatment=='Tx',]$Score), +
           sd(DeprScore[DeprScore$Exercise=='Light' & DeprScore$Treatment=='Tx',]$Score), +
           sd(DeprScore[DeprScore$Exercise=='Heavy' & DeprScore$Treatment=='Tx',]$Score))
#sem
semData=sdData/sqrt(5) #<-- bad habit!

# Create a label for each row
Exercise=c("None","Heavy","Light","None","Heavy","Light")
Treatment=c("Placebo","Placebo","Placebo","Tx","Tx","Tx")

#Create a data frame with all of the needed info
dScore=data.frame(Exercise,Treatment,mnData,sdData,semData)

#Plot the data
ggplot(data = dScore, aes(x = Exercise, y = mnData, fill = Treatment)) +
  geom_bar(stat='identity',position = "dodge") +
  geom_errorbar(aes(ymin = mnData-semData, ymax = mnData+semData), position = "dodge") 

