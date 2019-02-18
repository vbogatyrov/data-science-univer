# Day 3 class work
library("dplyr")
library("tidyr")

setwd("d:/DataScienceCourse/data-science-univer/datasets/day3")

###### R Markdown ######
install.packages("rmarkdown", dependencies = TRUE)
library("rmarkdown")
########################


######### Task 1 #########
## Join two data frames

df <- read.csv("DataDay3a.csv", dec = ".", sep = ";")
df2 <- read.csv("DataDay3b.csv", dec = ".", sep = ";")

# Remove empty columns from df2
df2$X <- NULL
df2$X.1 <- NULL
df2$X.2 <- NULL
df2$X.3 <- NULL
df2$X.4 <- NULL
df2$X.5 <- NULL
df2$X.6 <- NULL

# Join data frames and remove df2 which is not needed anymore
df <- left_join(df, df2, by = "id")
rm(df2)
###########################


######### Task 3 #########
## Add "Profit" column
df$Profit <- df$Total.Volume * df$AveragePrice
##########################


######### Task 4 #########
## Find total profit by organic and non-organic avocado
df %>% group_by(type) %>% summarise(total_profit_by_type = sum(Profit))
##########################


######### Task 5 #########
## Which year is the most successfull
df %>% group_by(year) %>% summarise(annual_profit = sum(Profit)) %>% arrange(desc(annual_profit))
##########################


######### Task 6 #########
## Plot 3 graphics of dependencies of avg. price from number of packs of different size
plot(df$Small.Bags, df$AveragePrice)
plot(df$Large.Bags, df$AveragePrice)
plot(df$XLarge.Bags, df$AveragePrice)
## If the number of packs is small, the avg. price varies greatly.
## When the number of packs is large enough, avg. price is getting closer to some specific value.
##########################


######### Task 7 #########
## ???
boxplot(df$Total.Volume)
##########################


######### Task 8 #########
## Pie plot of different types of avocado sold in 2017
pp <- df %>% filter(year == 2017) %>% summarise(t4046 = sum(X4046), t4225 = sum(X4225), t4770 = sum(X4770))
pp <- as.numeric(pp[1,]) # Convert pp from data.frame to vector, because pie() takes vectors as input
pie(pp, labels = c("X4046", "X4225", "X4770"))
##########################


######### Task 9 #########
## In which region mean avg. price was the highest, and in which region - the lowest
df %>%  group_by(region) %>% summarise(mean_price = mean(AveragePrice)) %>% arrange(mean_price) %>% head(n = 3)
df %>%  group_by(region) %>% summarise(mean_price = mean(AveragePrice)) %>% arrange(desc(mean_price)) %>% head(n = 3)
## The lowest mean avg. price was in Houston
## The highest mean avg. price was in HartfordSpringfield
##########################


######### Task 10 #########
## Which regions are similar in terms of avocado sales
#df[,c(3:12,14:15)]
t10 <- df[,c(3:12,14:15)]

t11 <- t10 %>% group_by(region, type) %>% summarise(avg_price = mean(AveragePrice), tot_vol = sum(Total.Volume),
                                                    tot_4046 = sum(X4046), tot_4225 = sum(X4225),
                                                    tot_4770 = sum(X4770), tot_bags = sum(Total.Bags),
                                                    tot_small.bags = sum(Small.Bags), tot_large.bags = sum(Large.Bags),
                                                    tot_xlarge.bags = sum(XLarge.Bags), tot_profit = sum(Profit))


# Spread multiple columns for conventional/organic types
library(data.table)
t12 <- dcast(setDT(t11), region ~ type, value.var = colnames(t11)[3:12])

########### USED STANDARD DIST INSTEAD ##################
# Install and load "distances" library to calculate 'distance' between regions
#install.packages("distances", dep = TRUE)
#library(distances)
#my_dm <- distances(t12, id_variable = "region", dist_variables = colnames(t12)[2:21])
#########################################################

region_dist <- dist(t12[,2:21])
hist(region_dist, breaks = 100)
plot(density(region_dist))
region_dist_cleaned <- region_dist[region_dist < 3e+08] # 2e+08
plot(density(region_dist_cleaned))

min_distances <- region_dist_cleaned[region_dist_cleaned < (median(region_dist_cleaned) - 1.09*sd(region_dist_cleaned))]

region_dist_matr <- as.matrix(region_dist) # Create full matrix from dist. object

## Finds indexes of elements with the given  "vals" values in "matr" matrix
indexOf <- function(matr, vals) {
    res <- c()
    row_num <- dim(matr)[1]
    col_num <- dim(matr)[2]
    for (val in vals) {
        for (i in 1:row_num) {
            for (j in 1:col_num) {
                if (abs(val - matr[i,j]) < 1) {
                  res <- c(res, c(val, i,j))
                  break
                }
            }
        }
    }
  return(res)
}

indexOf(region_dist_matr, min_distances)

# (4,48)  -- Boise, Spokane
# (24,35) -- Louisville, Pittsburgh
# (24,48) -- Louisville, Spokane
# (27,49) -- Nashville, StLouis
# (48,50) -- Spokane, Syracuse

ttt <- t12 %>% filter(region %in% c("Boise", "Spokane", "Louisville", "Pittsburgh", "Syracuse"))
ttt <- t12 %>% filter(region %in% c( "Spokane", "Boise"))
ttt <- t12 %>% filter(region %in% c( "Nashville", "StLouis"))
