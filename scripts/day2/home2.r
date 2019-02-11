# Home task day 2
setwd("d:/DataScienceCourse/data-science-univer/datasets/day2")
df <- read.csv("DataDay2.csv", sep = ";", dec = ",")


###### Correct data ######
colnames(df)[4] <- "Population" # correct column name

# Correct negative GDP per cap values
df$GDP.per.capita[!is.na(df$GDP.per.capita) & (df$GDP.per.capita < 0)] <-       
  (-1) * df$GDP.per.capita[!is.na(df$GDP.per.capita) & (df$GDP.per.capita < 0)]

# Check other columns
df$CO2.emission[df$CO2.emission < 0] # Check if CO2.emission column contains negative values
df$Population[df$Population < 0]     # Check if population column contains negative values
df$Area[df$Area < 0]
df$Area[df$Area < 0] <- (-1) * df$Area[df$Area < 0] # Correct negative values in "Area" column

###### Add "Population Density" column  ######
df$Country.Name[is.na(df$Population)]






df$GDP.per.capita[df$GDP.per.capita < 0]
mean(df$GDP.per.capita[!is.na(df$GDP.per.capita)])

