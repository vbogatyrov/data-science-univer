# Home task day 2

library("dplyr")
library("tidyr")


### Load data set ###
setwd("d:/DataScienceCourse/data-science-univer/datasets/day2")
df <- read.csv("DataDay2.csv", sep = ";", dec = ",")
######


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


## replace NA in "Polpulation" column with mean value
df$Population[is.na(df$Population)] <- mean(df$Population[!is.na(df$Population)])
df$PeopleDensity <- df$Population / df$Area
##############################################


## Replace NA's with mean values
df$GDP.per.capita[is.na(df$GDP.per.capita)] <- mean(df$GDP.per.capita[!is.na(df$GDP.per.capita)])
df$CO2.emission[is.na(df$CO2.emission)] <- mean(df$CO2.emission[!is.na(df$CO2.emission)])
#############################################


### 7 Country with the bigggest GDP per capita
df %>% select(Country.Name, GDP.per.capita) %>% arrange(desc(GDP.per.capita)) %>% head # Max. GFP per capita
df %>% select(Country.Name, Area) %>% arrange(Area) %>% head # Country with min. area

### 8 Region eith the largest mean country area
df %>% group_by(Region) %>% summarise(avg_area = mean(Area)) %>% arrange(desc(avg_area))

### Task 9
# ?????
# In the world
boxplot(df$Population)
hist(df$Population)
# the most is common population number does not exceed 2.0e+08
## Europe
df %>% select(Country.Name, Population) %>% dplyr::filter(as.character(df$Region) =="Europe & Central Asia")
df %>% select(Country.Name, Population) %>% dplyr::filter(df$Region =="Europe & Central Asia")
##################

### Task 10 Mean and median of GDP per region
df %>% group_by(Region) %>% summarise(median_gdp = median(GDP.per.capita), avg_gdp = mean(GDP.per.capita)) # %>% filter(median_gdp == avg_gdp)
##################


### Task 11

df %>% mutate(GDP = GDP.per.capita * Population) %>%  select(Country.Name, GDP, CO2.emission) %>% arrange(desc(GDP)) %>% head
df %>% mutate(GDP = GDP.per.capita * Population) %>%  select(Country.Name, GDP, CO2.emission) %>% arrange(desc(CO2.emission)) %>% head

df %>% mutate(GDP = GDP.per.capita * Population) %>%  select(Country.Name, GDP, CO2.emission) %>% arrange(GDP) %>% head
df %>% mutate(GDP = GDP.per.capita * Population) %>%  select(Country.Name, GDP, CO2.emission) %>% arrange(CO2.emission) %>% head



