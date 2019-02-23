# Day3 Home-2

library('dplyr')
library('ggplot2')

###### Prepare data ######
setwd("d:/DataScienceCourse/data-science-univer/datasets/day2")
df <- read.csv("DataDay2.csv", sep = ";", dec = ",")
colnames(df)[4] <- "Population" # correct column name
# Correct negative GDP per cap values
df$GDP.per.capita[!is.na(df$GDP.per.capita) & (df$GDP.per.capita < 0)] <-       
  (-1) * df$GDP.per.capita[!is.na(df$GDP.per.capita) & (df$GDP.per.capita < 0)]
# Check other columns
df$CO2.emission[df$CO2.emission < 0] # Check if CO2.emission column contains negative values
df$Population[df$Population < 0]     # Check if population column contains negative values
df$Area[df$Area < 0]
df$Area[df$Area < 0] <- (-1) * df$Area[df$Area < 0] # Correct negative values in "Area" column
###########################

df$GDP <- df$GDP.per.capita * df$Population

## Task 2
#*Description*: plot grapphics of dependencies between variables

#1
ggplot(aes(x = GDP.per.capita, y = Population), data = df) +
  geom_point()

#2
ggplot(aes(x = GDP.per.capita, y = CO2.emission), data = df) +
  geom_point()

#3
ggplot(aes(x = GDP.per.capita, y = Area), data = df) +
  geom_point()

#4
ggplot(aes(x = Population, y = CO2.emission), data = df) +
  geom_point()

#5
ggplot(aes(x = Population, y = Area), data = df) +
  geom_point()

#6
ggplot(aes(x = CO2.emission, y = Area), data = df) +
  geom_point()



ggplot(aes(x = GDP, y = CO2.emission), data = df) +
  geom_point()
