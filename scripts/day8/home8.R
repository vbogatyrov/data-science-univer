# Home 8

library("dplyr")
library(jpeg)

setwd("d:/DataScienceCourse/data-science-univer/datasets/maps/")

image <- readJPEG("Ukraine.jpg")
Xras = 831; Yras = 553
Reg <- c("Lugansk","Kiev","Lviv","Odessa", "Harkov")
par(mar = c(0, 0, 0, 0))
?par
plot(1,xlim = c(0, Xras), ylim = c(0, Yras), xlab = "", ylab = "")
lim <- par()
lim$usr
rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
######################################################################

xy <- locator(5)
xy

Population <- c(423,2884, 721, 993, 1419)
mycex = (Population - min(Population))/max(Population) + 2
colpts = rgb(0.2, 0.5, 0.4, alpha = 0.6)
points(xy$x, xy$y,cex = mycex, col = 1, pch = 21, bg = colpts)
####################################################################
## Distances
df <- data.frame(xy)
max(dist(df))
###################################################################
###################################################################
###################################################################
library(sp)
library(maptools)
library(ggplot2)
library(rgeos)

gadm<-readRDS("gadm36_UKR_0_sp.rds") #source https://gadm.org/download_country.html
ukraine <- fortify(gadm)

m <- ggplot() + geom_map(data = ukraine, 
                         aes(map_id = id), 
                         map = ukraine,
                         fill = "white", color = "black") +
  expand_limits(x = ukraine$long, y = ukraine$lat) +
  coord_map("mercator") + 
  xlab("Lon") + ylab("Lat") + theme_bw()
######

mp <- m + geom_point(data = data.frame(Lat = c(50.3, 49.5, 46.3, 50.0),
                                       Lon = c(30.3, 24.0, 30.4, 36.1)),
                     aes(Lon, Lat), color = I("red"), size = 3)
mp
####
zz <- locator(2)
mpp <- mp + geom_line(aes(x= Year, y= bp_median, color=bp_median), show.legend = FALSE) +

