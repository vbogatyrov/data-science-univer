# Home 6 R

library('dplyr')
library('tidyr')
library('ggplot2')
library('VIM')
library('mice')

setwd("d:/DataScienceCourse/data-science-univer/datasets/day4")
titanic <- read.csv("Day6-titanic.csv", sep = ",", dec = ".")

### Avg. age of survived passanger
titanic_surv_age_no_na <- titanic %>% filter(Survived == 1 & !is.na(Age))
mean(titanic_surv_age_no_na$Age) # 28.34

### Dead age distribution -- normal, p-value 7.816e-08
titanic_death_age_no_na <- titanic %>% filter(Survived == 0 & !is.na(Age))
shapiro.test(titanic_death_age_no_na$Age)

### Embarkation port of the most survivors -- Southampton
titanic %>% filter(Survived == 1) %>% select(Embarked) %>% group_by(Embarked) %>% summarise(passgrs = n()) %>% arrange(desc(passgrs))

### Piechart of passangers of certain cabin class
cabin_class_passgrs <-titanic %>% select(Pclass) %>% group_by(Pclass) %>% summarise(passgrs = n())
pie(cabin_class_passgrs$passgrs, main = "Passanger cabin classes", labels=cabin_class_passgrs$passgrs, col= c(2,3,4))
legend("bottomright", legend=c("1st class", "2nd class", "3rd class"), cex=0.8, fill= c(2,3,4))


### ???????????????????????????????????????????????????????????
### Hypotesis that older passangers buy more expensive tickets
plot(titanic$Age, titanic$Fare) # shows no correlation
titanic_age_no_na <- titanic %>% filter(!is.na(Age))
tmp <- titanic_age_no_na %>% select(Age, Fare)

titanic$age_group <- cut(titanic$Age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90))
age_fare <- titanic %>% select(Fare, age_group) %>% group_by(age_group) %>% summarise(mean_fare = mean(Fare))
plot(age_fare)

### Fill in NA's
md.pattern(titanic)
tmp <- titanic %>% select(Survived, Pclass, Sex, SibSp, Parch, Age)
imp <- mice(tmp, seed=1234)
res <- complete(imp, action=2) 
titanic$Age <- res$Age

## Clusters
str(titanic)
tmp <- titanic %>% select(PassengerId, Survived, Age, Fare)
clust <- kmeans(tmp[, 2:4], 3)
titanic$clust <- clust$cluster

ggplot(data=titanic,aes(x=Age, y=Fare))+
  geom_point(col=titanic$clust)


cor(titanic$Age,titanic$Fare,use="pairwise")

####################################################################################################################
####################################################################################################################
########## Slide 6 ###########################

########################################
### 1 Load data from DataDay2.csv ######

day2 <- read.csv("DataDay2.csv", sep = ";", dec = ",")
df <- day2
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
##########################################

#########################################
####### 2 Check vybrosy         #########
boxplot(df$CO2.emission)
library("outliers")
grubbs.test(df$CO2.emission, type = 10) # G = 12.14900, U = 0.27291, p-value < 2.2e-16
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 10291920)
#Country.Name CO2.emission
#1        China     10291927
which(df$Country.Name == "China") # 42
df <- df[-c(42),] # Remove China from dataframe
## Do it again
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 5254278) # United States
which(df$Country.Name == "United States")
df <- df[-c(206),] # Remove the US from the dataframe
## and again
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 2238376)
which(df$Country.Name == "India")
df <- df[-c(88),]
####
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 1705340)
which(df$Country.Name == "Russian Federation")
df <- df[-c(159),]
###
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 1214040)
which(df$Country.Name == "Japan")
df <- df[-c(96),]
### 
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 719882)
which(df$Country.Name == "Germany")
df <- df[-c(72),]
###
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 649480)
which(df$Country.Name == "Iran, Islamic Rep.")
df <- df[-c(88),]
###
df %>% select(Country.Name, CO2.emission) %>% filter(CO2.emission > 587156)
which(df$Country.Name == "Korea, Rep.")
df <- df[-c(99),]
##############################################################################
quantile(df$CO2.emission)
X25 <- zz[2]
X75 <- zz[4]
outlier_value <- X75 + 1.5*(X75 - X25) 
outlier_min <- X25 - 1.5*(X75 - X25)
df <- df %>% filter(CO2.emission > outlier_min)

##############################################################################################
##### REMOVE NA's
md.pattern(df)
tmp <- df %>% select(Population, CO2.emission, GDP.per.capita)
imp <- mice(tmp, seed=1234)
res <- complete(imp, action=2) 
df$Population <- res$Population
df$CO2.emission <- res$CO2.emission
df$GDP.per.capita <- res$GDP.per.capita

##################################################################
######## 3 Linear regression CO2 per 1000 person #################
df$CO2_per_1000 <- (df$CO2.emission / df$Population) * 1000
plot(df$CO2_per_1000, df$GDP.per.capita)
cor(df$CO2_per_1000_logistic, df$GDP.per.capita_logistic)

#####
logistic <- function(d) {
  #return(1 - 1/(1+exp(-d)))
  return (1/(1 + exp( (sd(d) - d)/(sd(d)*sd(d)) )))
}
df$CO2_per_1000_logistic <- logistic(df$CO2_per_1000)
df$GDP.per.capita_logistic <- logistic(df$GDP.per.capita)
#####
library("plotly")
#####

tmp <- df %>% filter(Country.Name == "Uganda" | Country.Name == "Zambia") %>% select(CO2.emission, GDP.per.capita)
plot_ly(data = tmp, type = 'scatterpolar') %>% 
  add_trace(r = ~df[1,CO2.emission], theta = ~45, Name="CO2", line = list(color = "blue")) %>%
  add_trace(r = ~df[2,GDP.per.capita], theta = ~90, Name="GDP")

chart_link <- api_create(p)
