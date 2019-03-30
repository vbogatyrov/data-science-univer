# Home 7

library('dplyr')
library('tidyr')
library('mice')
library('neuralnet')

setwd("d:/DataScienceCourse/data-science-univer/datasets/NN-life-expectancy/data/")

###################################################################################
######   LOAD DATA
###################################################################################
load_data <- function(fileName) {
   ld <- read.csv(fileName, sep = ",", dec = ".")
   colnames(ld)[1] <- "Country.Name"
   ld$Country.Name <- NULL
   ld$Indicator.Name <- NULL
   ld$Indicator.Code <- NULL
   ld$X <- NULL # incorrect column  
   return(ld)
}

literacy <- load_data("adult_literacy.csv")
energy   <- load_data("energy_use.csv")
imports  <- load_data("imports.csv")
life     <- load_data("life_expectancy.csv")
reserves <- load_data("total_reserves.csv")

imports_raw <- read.csv("imports.csv", sep = ",", dec = ".")
colnames(imports_raw)[1] <- "Country.Name"

###################################################################################
######   REMOVE YEARS WITH TOO MANY NA's
###################################################################################
cc <- colnames(literacy)[2:60]
#literacy[, cc[20]]
#######
na_s_per_column <- function(df) {
   na_vector <- vector(mode = "integer", length = 59)
   for (i in 1:59) {
      na_count <- 0
      for (v in df[, cc[i]]) {
         if (is.na(v)) {
            na_count <- na_count + 1
         }
      }
      na_vector[i] <- na_count
   }
   return(na_vector)
}
#######
na_literacy <- na_s_per_column(literacy) / 264
na_energy <- na_s_per_column(energy) / 264
na_imports <- na_s_per_column(imports) / 264
na_life <- na_s_per_column(life) / 264
na_reserves <- na_s_per_column(reserves) / 264

year_nas <- data_frame("col" = cc, "literacy" = na_literacy, 
                       "energy" = na_energy, "imports" = na_imports, 
                       "life" = na_life, "reservers" = na_reserves)
########################################################
### Basing on year_nas, remove the following years:
###              1960-1969, 2016-2018
########################################################
energy   <- energy[-c(2:11, 58:60)]
imports  <- imports[-c(2:11, 58:60)]
life     <- life[-c(2:11, 58:60)]
reserves <- reserves[-c(2:11, 58:60)]

###################################################################################
######   REMOVE COUNTRIES WITH TOO MANY NA's
###################################################################################

countries <- energy$Country.Code
na_s_per_row <- function(df) {
   na_vector <- vector(mode = "integer", length = 264)
   for (i in 1:264) {
      na_count <- 0
      for (j in 2:47) {
         if (is.na(df[i,j])) {
            na_count <- na_count + 1
         }
      }
      na_vector[i] <- na_count
   }
   return(na_vector)
}

na_contry_energy <- na_s_per_row(energy) / 46
na_contry_imports <- na_s_per_row(imports) / 46
na_contry_life <- na_s_per_row(life) / 46
na_contry_reserves <- na_s_per_row(reserves) / 46

country_nas <- data_frame("country" = countries,
                       "energy" = na_contry_energy, "imports" = na_contry_imports, 
                       "life" = na_contry_life, "reserves" = na_contry_reserves)

## Add country name column
country_nas$CountryName <- imports_raw$Country.Name
country_nas <- country_nas[, c(1,6, 2:5)]

# Calc. total NA
country_nas$TotalNA <- country_nas$energy + 
   country_nas$imports + country_nas$life + country_nas$reserves


country_nas <- country_nas %>% filter(TotalNA < 2)
country_nas <- country_nas %>% filter(energy < 1 & imports < 1 & life < 1 & reserves < 1)
#country_nas <- country_nas %>% filter(energy + imports + life + reserves <  2 )
###### Remove countries with too many NA's ######
energy <-energy %>% filter(Country.Code %in% country_nas$country)
imports <- imports %>% filter(Country.Code %in% country_nas$country)
life <- life %>% filter(Country.Code %in% country_nas$country)
reserves <- reserves %>% filter(Country.Code %in% country_nas$country)
######################################################
#energy_tmp <- energy
#na.approx(energy_tmp,na.rm=F)
#na.spline(sleep,na.rm=F)
#na.spline(energy,na.rm=T)
#na.approx(energy, na.rm = T)
######################################################
#tmp <- literacy
#imp <- mice(tmp, seed = 1234)
#res <- complete(imp, action=2) 

###### Convert data to long format ######
#long_filled <- function(df) {
#   df_long <- df %>% gather(year, value, X1970:X2015)
#   tmp <- df_long
#   imp <- mice(tmp, seed = 1234, m = 1)
#   res <- complete(imp, action = 1)
#   df_long <- res
#   return(df_long)
#}
#energy_long <- long_filled(energy)
#imports_long <- long_filled(imports)
#life_long <- long_filled(life)
#reserves_long <- long_filled(reserves)

long_unfilled <- function(df) {
   df_long <- df %>% gather(year, value, X1970:X2015)
   return(df_long)
}

energy_long <- long_unfilled(energy)
imports_long <- long_unfilled(imports)
life_long <- long_unfilled(life)
reserves_long <- long_unfilled(reserves)

all_long <- energy_long
colnames(all_long)[3] <- "energy"
all_long$imports <- imports_long$value
all_long$life <- life_long$value
all_long$reserves <- reserves_long$value
all_long$literacy <- literacy_long$value
################################################
years <- unique(all_long$year)
tmp_tables <- array(dim = c(46))
#tmp_table <- all_long %>% filter(year == "X1980")
#tmp <- tmp_table
#imp <- mice(tmp, seed = 4311, m = 1, method = "rf")
#res <- complete(imp, action = 1)
#########################
joined_table <- NULL
for(yr in years)  {
  tmp_table <- all_long %>% filter(year == yr)
  ### fill NA's with mice
  tmp <- tmp_table
  imp <- mice(tmp, seed = 4311, m = 1, method = "rf")
  res <- complete(imp, action = 1)
  tmp_table <- res
  ## merge table
  if(is.null(joined_table)) {
    joined_table <- tmp_table
  } else {
    joined_table <- rbind(joined_table, tmp_table)
  }
}

all_long <- joined_table

########################################
######  Normalize all_long table  ######
########################################


country_nas <- country_nas[, c(1,6, 2:5)]
all_long <- all_long[, c(1:4, 6,5)]


all_norm <- all_long
all_norm$Country.Code <- NULL
all_norm$year <- NULL

max = apply(all_norm , 2 , max)
min = apply(all_norm, 2 , min)
scaled = as.data.frame(scale(all_norm, center = min, scale = max - min))
all_norm <- scaled



# Generate random set of indexes for training data
index = sample( seq_len(nrow(all_norm)), size = 4700 )
trainNN = all_norm[index , ]
testNN = all_norm[-index , ]

#
f<-as.formula("life ~ energy + imports + reserves")
NN = neuralnet(f, trainNN, hidden = c(2,3) )

predict_testNN = compute(NN, testNN[,c(1:3)])
predict_testNN = (predict_testNN$net.result * (max(all_long$life) - min(all_long$life))) + min(all_long$life)

testNN$life_predicted <- predict_testNN$net.result

## TODO Scale testNN back to real values

