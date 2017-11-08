# Spatial Autocorrelation in Medicare Costs

setwd("~/Desktop/Dropbox/UChicago/4.1/GeoSpatial/Final_Project")

source("load_medicare_data.R")

library(dismo)
library(ncf)

contained_codes <-midwest_df$code_combo[which(!is.na(midwest_df$Standardized.Per.Capita.Costs))]
countys@data$code_combo <-  paste0(countys@data$STATEFP,countys@data$COUNTYFP)

midwest_spatial <- countys[which(countys$code_combo %in% contained_codes),]


points <- coordinates(midwest_spatial)
points_df <- as.data.frame(points)

costs <- (midwest_df[which(!is.na(midwest_df$Standardized.Per.Capita.Costs)),] %>% group_by(id) %>% summarise(cost = min(Standardized.Per.Capita.Costs),
                                                   pop = min(Beneficiaries.with.Part.A.and.Part.B)) %>%
                arrange(id))

c2 <- correlog(points_df$V1, points_df$V2, costs$cost, w = NULL, 1, resamp = 100, 
               latlon =TRUE, na.rm = TRUE, quiet = FALSE)

df2 <- as.data.frame(cbind(c2$mean.of.class,c2$correlation))
names(df2) <- c("distance","correlation")

correlogram2 <- (ggplot(df2,aes(x=distance,y=correlation)) + geom_point(aes(x=distance,y=correlation),size=1,shape=21,color="black",fill="lightblue")
                 + theme_bw()
                 + scale_y_continuous(limits=c(-2,2))
                 + geom_smooth(span=0.3,color="firebrick",fill="indianred1")
                 + labs(x="Distance between Centroids",y="Spatial AutoCorrelation")
                 + ggtitle("Spatial Correlation in Medicare Costs"))
setwd("~/spatial_analysis_medicare/images")
ggsave("correlogram.png",correlogram2)
