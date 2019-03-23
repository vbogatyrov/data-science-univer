# Home 7

library('dplyr')
library('tidyr')

setwd("d:/DataScienceCourse/data-science-univer/datasets/NN-life-expectancy/data/")
######
raw_literacy <- read.csv("adult_literacy.csv", sep = ",", dec = ".")
literacy <- raw_literacy
colnames(literacy)[1] <- "Country.Name"
literacy$Country.Name <- NULL
literacy$Indicator.Name <- NULL
literacy$Indicator.Code <- NULL
literacy$X <- NULL # incorrect column
######

