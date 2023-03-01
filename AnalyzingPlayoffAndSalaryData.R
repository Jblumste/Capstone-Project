library(ggplot2)

stats <- read.csv('NBASalaryAndPlayoffs.csv')

#Plots for teams that made playoffs vs teams that didn't make playoffs
ggplot(data = stats, aes(x = Playoff)) +
  geom_bar() + 
  labs(title = 'Bar chart for NBA teams making playoffs')

ggplot(data = stats, aes(x = Quarter)) +
  geom_bar() + 
  labs(title = 'Bar chart for NBA teams making Quarterfinals')

ggplot(data = stats, aes(x = Semi)) +
  geom_bar() + 
  labs(title = 'Bar chart for NBA teams making Semifinals')
#Histogram for eFG
ggplot(data = stats, aes(x = eFG.)) +
  geom_histogram(color = 'black', fill = 'white', bins = 10) 

median(stats$eFG.)
mean(stats$eFG.)
sd(stats$eFG.)

#Histogram for Salaries
ggplot(data = stats, aes(x = Salary)) + 
  geom_histogram(color = 'black', fill = 'white', bins = 10)

median(stats$Salary)
mean(stats$Salary)
sd(stats$Salary)

summary(stats)
#Histogram for Age
ggplot(data = stats, aes(x = Age)) + 
  geom_histogram(color = 'black', fill = 'white', bins = 10)

sd(stats$Age)


# Finding Correlations in Data ------------------------------------------------------
library(corrplot)

statsNoNames <- stats[,c(-1,-32,-33)]

corr <- cor(statsNoNames)

corr

corrplot(corr, method = 'circle')

#High Correlation between
#A lot of values
#When you see a square of highly correlated values, it usually means that the statistics are related by name
#e.g. FG and FGA being Field Goals and Field Goal attempts


#Removing columns that have very high correlation
#Going to remove FG and FGA because FG% sums up both of them pretty well
#Same with 3P and 3PA along with 2P and 2PA
#Getting rid of FT, FTA. ORB, DRB, and GS

statsReduced <- statsNoNames[,c(-3,-5,-6,-8,-9,-10,-11,-15,-16,-18,-19)]

corrReduced <- cor(statsReduced)

corrReduced

corrplot(corrReduced, method = 'circle')

#Pts/G seems to be highly correlated with a lot of these values, so I am going to remove that too

statsFinal <- statsReduced[,-15]

corrFinal <- cor(statsFinal)

corrplot(corrFinal, method = 'circle')


# Predicting Playoffs based off Players -----------------------------------

library(RWeka)
library(C50)

playersOnly <- statsFinal
#playersOnly$Names <- stats$Name

playersOnly <- playersOnly[,c(-18,-17)]


oneRule <- OneR(as.factor(Playoff) ~ ., data = playersOnly)

oneRule

stats2022 <- read.csv('2022Stats.csv')

newStats <- stats2022[stats2022$MP > 12,]

572/812 * 605
572/812
