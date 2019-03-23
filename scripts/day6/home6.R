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
hist(age_fare)

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
