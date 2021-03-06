# Home 4

library('dplyr')
library('ggplot2')
setwd("d:/DataScienceCourse/data-science-univer/datasets/day4")

df <- read.csv("Day4.csv", sep = ";", dec = ",")

cor(df[, 5:7])
colnames(df)

#------------
ggplot(aes(x = Cql, y = Ie), data = df) +
  geom_point()   

ggplot(aes(x = Cql, y = Iec), data = df) +
  geom_point() 

ggplot(aes(x = Cql, y = Is), data = df) +
  geom_point() 
#----------
ggplot(aes(x = Ie, y = Iec), data = df) +
  geom_point()   

ggplot(aes(x = Ie, y = Is), data = df) +
  geom_point() 
#------------
ggplot(aes(x = Iec, y = Is), data = df) +
  geom_point() 

lm1 <- lm(Cql ~ Ie + Iec + Is, data = df)
# lm1$coefficients
lm2 <- lm(Cql ~ Ie + Iec, data = df)
lm3 <- lm(Cql ~ Ie + Is, data = df)
lm4 <- lm(Cql ~ Iec + Is, data = df)


nls1<-nls(formula = Cql ~ a*Ie^d + b*Iec^e + c*Is^f, data=df,start=list(a=1, b=1, c=1, d=1, e=1, f=1))
cc <- coef(nls1)



