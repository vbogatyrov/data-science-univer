# Class4
library('dplyr')
library('ggplot2')
# Task 1

df1 <- read.csv("Day4-1.csv", sep = ";", dec = ",") # linear 
df2 <- read.csv("Day4-2.csv", sep = ";", dec = ",") # parabola

ggplot(aes(x = Par.1, y = Par.2), data = df1) +
  geom_point()



ggplot(aes(x = Par.1, y = Par.2), data = df2) +
  geom_point() +
  

  


reslm1<-lm(formula = df1$Par.2 ~ df1$Par.1)
summary (reslm1)

b <- reslm1$coefficients[1]
k <- reslm1$coefficients[2]

xmin <- min(df1$Par.1)
xmax <- max(df1$Par.2)
x <- seq(from = xmin, to = xmax, length.out =100)
y <- k*x + b
str(x)

#-----------------------------------------------------


#reslm2<-nls(formula = Par.2 ~ a*Par.1^2+b*Par.1+c, data=df2,start=list(a=1,b=1, c=500))
reslm2<-nls(formula = Par.2 ~ a*Par.1^2+c, data=df2,start=list(a=1, c=1))
summary (reslm2)
#--------------------------------

## Task 2

###### Prepare data ######
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
## 1
shapiro.test(df$GDP.per.capita)
shapiro.test(df$Population)
shapiro.test(df$CO2.emission)
tmp <- shapiro.test(df$Area)
## No normal params.

## 2
gdppc <- df$GDP.per.capita[!is.na(df$GDP.per.capita)]
median(gdppc)
wilcox.test(gdppc, mu=median(gdppc), conf.int=T) ## median GDP is not significant
mean(gdppc)
wilcox.test(gdppc, mu=mean(gdppc), conf.int=T) ## mean GDP is not significant

popul <- df$Population[!is.na(df$Population)]
median(popul)
wilcox.test(popul, mu=median(popul), conf.int=T) ## median GDP is not significant
mean(popul)
wilcox.test(popul, mu=mean(popul), conf.int=T) ## mean GDP is not significant

co2 <- df$CO2.emission[!is.na(df$CO2.emission)]
median(co2)
wilcox.test(co2, mu=median(co2), conf.int=T) ## median GDP is not significant
mean(co2)
wilcox.test(co2, mu=mean(co2), conf.int=T) ## mean GDP is not significant

area <- df$Area[!is.na(df$Area)]
median(area)
wilcox.test(area, mu=median(area), conf.int=T) ## median GDP is not significant
mean(area)
wilcox.test(area, mu=mean(area), conf.int=T) ## mean GDP is not significant


df$Population <- abs(df$Population)
#-----------------------------------
df_co2 <- df %>% filter( !is.na(df$CO2.emission))
df_co2 %>% group_by(Region) %>% summarise(prox = shapiro.test(CO2.emission)$p.value) %>% arrange(desc(prox))
#-----------------------------------
popul_region <- df %>% group_by(Region) %>% summarise(tot_pop = sum(as.numeric(Population)))
str(popul_region)

aa <- pull(popul_region, "tot_pop")
aa <- aa[1:6]

bb <- pull(popul_region, "Region")
bb <- bb[1:6]
#pie(popul_region[2:6,2], labels = popul_region[2:6,1])
pie(aa, labels = bb)
str(aa)
