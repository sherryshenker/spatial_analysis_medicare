# base script for loading packages, helper functions, and data for all analysis

library(maps)
library(maptools)  #for shapefiles
library(scales)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(sp)
library(viridis)
library(spdep)

format_plot <- function(p){
  final <- (p + theme_bw() + theme(strip.text.x = element_text(size = 14),
                                   text = element_text(size=16),
                                   legend.text=element_text(size=16),
                                   panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   axis.text.y = element_blank(),
                                   axis.ticks.y = element_blank(),
                                   axis.title.x = element_blank(),
                                   axis.title.y = element_blank()) 
            + coord_fixed())
  return(final)
}

setwd("~/Desktop/geospatial data/final_project/")

medicare <- read.csv("medicare.csv")

# make colnames equal to first row
colnames(medicare) <- as.matrix(medicare[1,])

#delete first row 
medicare <- medicare[-1, ]

#spatial county data
setwd("~/Desktop/geospatial data/final_project/cb_2016_us_county_500k")

countys <- rgdal::readOGR(dsn = ".",layer="cb_2016_us_county_500k")

countys_f <- fortify(countys,region="GEOID")

#extract state and county codes separately

countys.df <- merge(countys_f,countys@data,by.x="id",by.y="GEOID")
countys.df$code_combo <- paste0(countys.df$STATEFP,countys.df$COUNTYFP)
countys.df$code_combo <- as.numeric(countys.df$code_combo)

county_medicare <- merge(countys.df,medicare,by.x="code_combo",by.y="State and County FIPS Code",all.x=TRUE)

# filter for only midwestern states 
midwest <- c("17","18","19","20","26","27","29","31","38","39","46","55")

medicare_spatial <- county_medicare[which(county_medicare$STATEFP %in% midwest),]

no_spaces <- make.names(names(medicare_spatial), unique=TRUE)
names(medicare_spatial) <- no_spaces

#convert from factors to numeric 
midwest_spatial <- sapply(medicare_spatial, function(x) as.numeric(as.character(gsub("%","",x))))
midwest_spatial <- as.data.frame(midwest_spatial)
midwest_spatial1 <- subset(midwest_spatial, select = -c(State,County))

#data only without the geographic component 
midwest_data <- medicare[which(medicare$`State and County FIPS Code` %in% midwest_spatial$code_combo),]
midwest_data <- sapply(midwest_data, function(x) as.numeric(as.character(gsub("%","",x))))
midwest_data <- as.data.frame(midwest_data)
midwest_data <- subset(midwest_data,select=-c(State,County))
