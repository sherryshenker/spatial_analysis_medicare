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

setwd("~/Desktop/geospatial data/final_project/cb_2016_us_county_500k")

countys <- rgdal::readOGR(dsn = ".",layer="cb_2016_us_county_500k")

countys_f <- fortify(countys,region="GEOID")

medicare$state_fips <- substr(as.character(medicare$`State and County FIPS Code`),
                              start=1,stop=2)

medicare$county_fips <- substr(as.character(medicare$`State and County FIPS Code`),
                               start=3,stop=4)


countys_sub <- as.data.frame(countys)
countys_sub$row_id <- rownames(countys_sub)
countys_sub$code_combo <- paste0(countys_sub$STATEFP,countys_sub$COUNTYFP)

county_medicare <- merge(countys_sub,medicare,by.x="code_combo",by.y="State and County FIPS Code",all.x=TRUE)

countys_df <- merge(fortify(countys), county_medicare,
                    by.x=c("id"), by.y=c("row_id"))

continent <- countys_df[which(countys_df$STATEFP!="02"&countys_df$STATEFP!="15"),]

#north_east <- c("09","25","23","50","44","42","36","33","34")


continent$`Actual Per Capita Costs` <- as.numeric(continent$`Actual Per Capita Costs`)
continent$`Standardized Per Capita Costs` <- as.numeric(continent$`Standardized Per Capita Costs`)

#north_east_df <- continent[which(continent$STATEFP %in% north_east),]
midwest <- c("17","18","19","20","26","27","29","31","38","39","46","55")

midwest_df <- continent[which(continent$STATEFP %in% midwest),]
#midwest_df$`Standardized Per Capita Costs` <- as.numeric(midwest_df$`Standardized Per Capita Costs`)

no_spaces <- make.names(names(midwest_df), unique=TRUE)
names(midwest_df) <- no_spaces
midwest_df <- sapply(midwest_df, as.numeric )
midwest_df <- as.data.frame(midwest_df)
