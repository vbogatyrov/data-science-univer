# Home 5

library('dplyr')
library('ggplot2')
library('fpc')


setwd("d:/DataScienceCourse/data-science-univer/datasets/day4")
dfDay4 <- read.csv("Day4.csv", sep = ";", dec = ",")

###### Task 1 ###### 

kres=kmeans(dfDay4[,5:7], 4, nstart=20)
dfDay4$cluster <- kres$cluster
# K-means clustering with 4 clusters of sizes 28, 40, 34, 30
# (between_SS / total_SS =  80.9 %)
###### Task 2 #####
# Draw culsters with central points
cluster4Draw <- dfDay4[, 5:8]

# Centers of clusters will be in grey colour(16)
cluster4Draw[133,] <- c(kres$centers[1,], 16)
cluster4Draw[134,] <- c(kres$centers[2,], 16)
cluster4Draw[135,] <- c(kres$centers[3,], 16)
cluster4Draw[136,] <- c(kres$centers[4,], 16)


ggplot(data=cluster4Draw,aes(x=Ie, y=Iec))+
  geom_point(col=cluster4Draw$cluster)

###### Task 3 ######

kresCql = kmeans(dfDay4[,4], 4, nstart=20)
dfDay4$clusterCql <- kresCql$cluster
str(dfDay4)
# K-means clustering with 4 clusters of sizes 29, 35, 40, 28
# (between_SS / total_SS =  93.7 %)

zz <- apply(kres$centers, 1, function(x) { return(0.6579*x[1] + 0.8417*x[2] + 1.3042*x[3] - 0.5421)})
#(Intercept)           Ie          Iec           Is  
#-0.5421       0.6579       0.8417       1.3042 

## How many obs. got into different clusters
diffClust <- function(x,y) {
  len <- length(x)
  dif_num <- 0
  for (i in 1:len) {
    if ((x[i] == 1 && y[i] == 4) ||(x[i] == 4 && y[i] == 1)) {
      next()
    }
    if (x[i] != y[i]) {
      dif_num <- dif_num + 1
    }
  }
  return(dif_num)
}
################################################################

###### Task 4 ######

df <- read.csv("DataDay2.csv", sep = ";", dec = ",")
dfDay2 <- df

dfDay2$GDP.per.capita[is.na(dfDay2$GDP.per.capita)] <- mean(dfDay2$GDP.per.capita) # remove NA's
#kresDay2 = pamk(dfDay2[,3])
kresDay2 = kmeans(dfDay2[,3], 5, nstart=20)
kresDay2
str(dfDay2)
#dfDay2$cluster <- kresDay2$pamobject$clustering
dfDay2$cluster <- kresDay2$cluster

#cc = matrix(nrow = 5)
#for (i in 1:5) {
#  cc[i] <- dfDay2 %>% filter(cluster == 1) %>% group_by(Region) %>% summarise(rc = n())  
#}


dfDay2 %>% filter(cluster == 1) %>% group_by(Region) %>% summarise(rc = n(), mgdp = mean(GDP.per.capita))  
dfDay2 %>% filter(cluster == 2) %>% group_by(Region) %>% summarise(rc = n(), mgdp = mean(GDP.per.capita))  
dfDay2 %>% filter(cluster == 3) %>% group_by(Region) %>% summarise(rc = n(), mgdp = mean(GDP.per.capita))  
dfDay2 %>% filter(cluster == 4) %>% group_by(Region) %>% summarise(rc = n(), mgdp = mean(GDP.per.capita))  
dfDay2 %>% filter(cluster == 5) %>% group_by(Region) %>% summarise(rc = n(), mgdp = mean(GDP.per.capita))  

###### Task 5 ######
for (i in 3:6){
  print(  qplot(x=dfDay2[,i]) + 
            geom_histogram()) 
}

for (i in 3:5){
  hist(dfDay2[,i])
}

###### Task 6 ######

isLinear <- function(x, y) {
  cc <- cor(x,y)
  if (abs(cc) > 0.8) {
    return(T)
  } else {
    return(F)
  }
}
