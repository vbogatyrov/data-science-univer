# This is day2 class script

###### 1 Create rand matrix ######
m1 = rnorm(2500)
dim(m1) <- c(50,50)
##################################

###### 2 Visualize ######

meansOfColumns = colMeans(m1)
min(meansOfColumns)

mmm = as.matrix(meansOfColumns)
min(mmm)

# column #37 -- minimal mean


# install.packages('matrixStats')
# library(matrixStats)
 colSds(m1)
 max(colSds(m1)) # function "colSds" calculating standard deviation per column is from 'matrixStats' package
# row #20 is the one with the max standard deviation (sd)
plot(x = m1[,37], y = m1[,20], type = "l", col = "red", 
    xlab = "Vals. of col with min mean", ylab = "Vals. of col with mx ds")
###############################################################################

##### Task 3 #######

x <- 4
y <- 3

((x + y) %% 2) == 0
###################

#### Task 4 #######
x1 <- 4
y1 <- 5
x2 <- 2
y2 <- 7
(x1 == x2) || (y1 == y2) || (abs(x1 -x2) == abs(y1 - y2))
###################

################################################################################################
################################################################################################
################################################################################################

############## Factors ##############

gender <- c(0,1,0,0,0,0,0,1,1,1,1,0,1,0,1,0)
gender <- factor(gender, levels = )


############# Functions #############

summa <- function(a, b) {
  a - b
}

aaa <- print(1,2)

for (i in m1[,2]){
  print(i)
}


################################################################################################
################################################################################################
################################################################################################


#################### TASK 2 ####################

m2 <- rnorm(100)
m2 <- as.array(m2)


task2 <- function (matr) {
  m_breaker = mean(matr) - 2 * sd(m2)
  result <- cut(matr, c(min(matr),
                          mean(matr) - 2 * sd(m2), 
                          mean(matr) + 2 * sd(m2),
                          max(matr)),
                labels = c("First", "Second", "Third"))
  return(result)
}

#################
### Task 2.2 ####

task2_2 <- function(a,b) {
  res <- a:b
  print(res)
  print(length(res))
}
#################
### Task 2.3 ####

task2_3 <- function(N) {
  razryad <- 10
  while (N >= 1) {
    print(N %% razryad)
    N <- N - (N %% razryad)
    N <- N / 10
  }
}

nn <- 123
task2_3(nn)

#################
### Task 2.4 ####

m3 <- rnorm(20)

task2_4 <- function (matr) {
  m_mean <- mean(matr)
  i <- 1
  while(i <= length(matr)) {
    if (matr[i] > m_mean) {
      matr[i] <- matr[i] - 18
    }
    i <- i + 1
  }
  return(matr)
}

task2_4(m3)
###############################################################################
# TODO TASK 2.5-6

###### Taks 2.5 ######

m25 <- rnorm(15)
dim(m25) <- c(5,3)

task2_5 <- function(matr) {
  col_num <- dim(matr)[2]
  i <- 1
  for (i in 1:col_num) {
    hist(matr[,i])
  }
}
######################
###### Taks 2.6 ######





###############################################################################
#########  
setwd("d:/DataScienceCourse/data-science-univer/datasets/day1")
df <- read.csv("DataDay1.csv", sep = ";", dec = ",")


df %>% 
  mutate(GDP = GDP.per.capita / Population) %>%
  head

df %>% summarise(avg_gdp = mean(GDP.per.capita))

df %>% group_by(Conflicts.intencity) %>%
  summarise(avg_gdp = mean(GDP.per.capita),kol=n())





head(df)
newdf<-df %>%
  gather(Variables,Value, Conflicts.intencity:Population)

View(newdf)