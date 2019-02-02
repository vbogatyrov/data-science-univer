
# This is comment

1 + 5
1 + "a"

par <- 8
iris

logic_var <- (3 == 2)
logic_var <- T

logic_var <- 3>2 & 54<2
logic_var

par %% 3 # ostatok ot deleniya
par %/% 3 # celochislennoe delenie

#####################   Tasks 1   ##############################

#1 
a = 1
b = 2
S <- a * b
P <- 2*(a + b)

#2
avg_geom <- sqrt(a*b)

#3 
x1 <- 1
y1 <- 1
x2 <- 5
y2 <- 7
P3 = 2 * (abs(x2 -x1) + abs(y2 - y1))
S3 = abs(x2- x1) * abs(y2 - y1)

#4 
A <- 21
cond4 <- (A %% 2) != 0

#5
A <- 5
B <- 7
C <- 8
cond5 <- (A < B) & (B < C)

#6
cond6 <- (A > 0) | (B > 0) | (C > 0)

####################### End Tasks1  ###############################

arr_1 <- c(1,2,3,4,5,6,7,8,9,10)
arr_2 <- 1:10

arr_1[-c(3,5:8)] # Take all elements from arr_1 except 3,5,6,7,8

# Matrixes

matr1 <- matrix(seq(1,16), nrow = 4, ncol = 4)

# Lists

lst$Logic
lst$Text
lst$First[-3]
lst[[2]][1]

tbl1 <- read.csv("1.csv", sep=";")

head(tbl1$X)
tbl1[2:8, 2:3]

tbl1[tbl1$Par.1>30,]
tbl1[tbl1$Par.1>30, c(1,3)]
tbl1[tbl1$Par.1>30, -2]

tbl1$Sum <- tbl1$Par.1 + tbl1$Par.2 # Add new column to data frame
str(tbl1)
class(tbl1)
tbl1$Sum <- NULL # remove column from data frame

###### Statistical functions ########
mean(1:10)
mean(tbl1$Par.1)
quantile(tbl1$Par.1)
summary(tbl1)

##### Statistical functions tasks #####

#1 OK
#2 
norm_arr1 <- rnorm(100, 50)
norm_arr2 <- rnorm(100, 20)

mean(norm_arr1)
mean(norm_arr2)

sd(norm_arr1) # standard deviation
sd(norm_arr2) # standard deviation

#3
plot(norm_arr1, norm_arr2)

#4
hist(norm_arr1)
hist(norm_arr2)

#5
norm_arr1 <- norm_arr1 * 3

#6
mean(norm_arr2)
norm_arr2_m <- norm_arr2
norm_arr2_m[norm_arr2_m > mean(norm_arr2)] <- norm_arr2_m[norm_arr2_m > mean(norm_arr2)] * 18
hist(norm_arr2_m) # norm_arr2_m contains required result

