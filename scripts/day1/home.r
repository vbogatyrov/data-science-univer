###### Task 1.7 ######

A <- sample(-10:10,1)
B <- sample(-10:10,1)
C <- sample(-10:10,1)

answer1_7 <- ((A > 0) & (B <= 0) & (C <= 0)) | ((A <= 0) & (B > 0) & (C <= 0)) | ((A <= 0) & (B <= 0) & (C > 0))



###### Task 2 ######

#7
setwd("d:/DataScienceCourse/data-science-univer/datasets/day1")
dataDay1 <- read.csv("DataDay1.csv", sep = ";", dec = ",")


head(dataDay1)
tail(dataDay1)

#8 
dataDay1$ISO <- NULL # Remove ISO column
dataDay1$AnnualGdp <- dataDay1$GDP.per.capita * dataDay1$Population # Add 'annual GDP' column

########## Replace Na's with zeroes ###########
length(dataDay1$Country[is.na(dataDay1$Hospital.beds)]) > 0 # Check that column 'Hospital.beds' contains NA's
dataDay1$Hospital.beds[is.na(dataDay1$Hospital.beds)] <- 0  # Replace NA's in 'Hospital.beds' column with zeroes
length(dataDay1$Country[is.na(dataDay1$Hospital.beds)]) > 0 # Check that no NA's left in 'Hospital.beds' after the replacement

length(dataDay1$Country[is.na(dataDay1$High.technology.exports)]) > 0 # Same for 'High tech. export column'
dataDay1$High.technology.exports[is.na(dataDay1$High.technology.exports)] <- 0
length(dataDay1$Country[is.na(dataDay1$High.technology.exports)]) > 0
###############################################

#9 
# All summaries
summary(dataDay1$Conflicts.intencity)
summary(dataDay1$Hospital.beds)
summary(dataDay1$High.technology.exports)
summary(dataDay1$GDP.per.capita)
summary(dataDay1$Population)
summary(dataDay1$AnnualGdp)

boxplot(dataDay1$GDP.per.capita)

#10
plot(dataDay1$AnnualGdp, dataDay1$High.technology.exports)
