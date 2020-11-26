# loading library
library(ggplot2)
library(dplyr)
library(car)
#Reading fugazi.dat
mydata<-read.delim("fugazi.dat", header = TRUE)

# Exploring data
colnames(mydata)
#View(mydata)

graph <- ggplot(mydata, aes(music, liking, group = age, colour = age))
graph + geom_line() + geom_point(size = 3)

# Plotting age vs liking
graph <- ggplot(mydata, aes(age, liking, group = music, colour = music))
graph + geom_line() + geom_point(size = 3)

# since by question liking scale range is from -100 to 100 and plots to show extremes , data cleaning is required 
clean_data <-mydata%>% 
  filter(liking >=-100 & liking <=100 )

# checking
#View(clean_data)

# assigning factor music 
clean_data$music<-factor(clean_data$music, levels = c(1:3), labels = c("Fugazi", "Abba", "Barf Grooks"))

# assigning factor age
clean_data$age<-factor(clean_data$age, levels = c(1:2), labels = c("Age >40", "Age =<40 "))

# summary 
summary(clean_data)

#install.packages("hrbrthemes")
library(hrbrthemes)
# box plots liking vs age
ggplot(clean_data, aes(x=music, y=liking, fill=age, alpha=age)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("red", "blue")) +
  scale_alpha_manual(values=c(0.1,0.1)) +
  theme_ipsum() +
  theme(legend.position = "right") +
  xlab("music")

# box plots liking vs music
ggplot(clean_data, aes(x=age, y=liking, fill=music, alpha=music)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("green", "red","yellow")) +
  scale_alpha_manual(values=c(0.1,0.1,0.1)) +
  theme_ipsum() +
  theme(legend.position = "right") +
  xlab("age")
# Plotting music vs liking
graph <- ggplot(clean_data, aes(music, liking, group = age, colour = age))
graph + geom_line() + geom_point(size = 3)

# Plotting age vs liking
graph <- ggplot(clean_data, aes(age, liking, group = music, colour = music))
graph + geom_line() + geom_point(size = 3)

#Descriptive statistics by group
by(clean_data$liking, list(clean_data$age, clean_data$music), summary)

#Levene's Test:
leveneTest(clean_data$liking, interaction(clean_data$age, clean_data$music), center = median)

# Factorial ANOVA 
res <-aov(liking ~ music*age, data = clean_data)
summary(res)

# Anova
Anova(res)

TukeyHSD(res)

#Post hocs 
pairwise.t.test(clean_data$liking, clean_data$music, p.adjust.method = "bonferroni")
pairwise.t.test(clean_data$liking, clean_data$age, p.adjust.method = "bonferroni")

# linear regression
model = lm(liking ~ music*age, data = clean_data)
summary(model)

# plotting model
plot(model)
par(mfrow=c(2,2))

#Error bar graph music vs liking
graph <- ggplot(clean_data, aes(music, liking, fill = age))
graph + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Music", y = "Liking mean", fill = "Group") 

#Error bar graph Age vs liking
graph <- ggplot(clean_data, aes(age, liking, fill = music))
graph + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Age", y = "Liking mean", fill = "Group") 


# Line plot music vs liking
ggplot(clean_data, aes(y = liking, x = music, colour = age, group = age)) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "pointrange") +
  stat_summary(fun = mean,
               geom = "line") +
  facet_wrap( ~age)

# Line plot Age vs liking
ggplot(clean_data, aes(y = liking, x = age, colour = music, group = music)) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "pointrange") +
  stat_summary(fun = mean,
               geom = "line") +
  facet_wrap( ~music)














