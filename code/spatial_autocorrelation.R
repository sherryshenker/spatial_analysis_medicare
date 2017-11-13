# Spatial Autocorrelation in Medicare Costs

setwd("~/spatial_analysis_medicare/code")

source("load_medicare_data.R")

library(dismo)
library(ncf)

contained_codes <-midwest_df$code_combo[which(!is.na(midwest_df$Standardized.Per.Capita.Costs))]
countys@data$code_combo <-  paste0(countys@data$STATEFP,countys@data$COUNTYFP)

midwest_spatial <- countys[which(countys$code_combo %in% contained_codes),]

points <- coordinates(midwest_spatial)
points_df <- as.data.frame(points)

costs <- (midwest_df[which(!is.na(midwest_df$Standardized.Per.Capita.Costs)),] %>% group_by(id) %>% summarise(cost = min(Standardized.Per.Capita.Costs),
                                                   pop = min(Beneficiaries.with.Part.A.and.Part.B),
                                                   rate = min(Hospital.Readmission.Rate)) %>%
                arrange(id))

# ----- Correlogram for Global Spatial Autocorrelatoin -----------------

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


# ----- Local Spatial Autocorrelation -----------
nb2 <- poly2nb(midwest_spatial, queen=FALSE)
nbls2 <- nb2listw(nb2,zero.policy=TRUE )

q1 <- poly2nb(midwest_spatial, queen=TRUE)
q2 <- nb2listw(q1,zero.policy=TRUE )

#' Function for mapping local spatial clusters
#' Using Moran's I statustic
#' Requires: 
#' - weights: a neighborweight list object
#' - var: variable of interest as a string
#' - alpha: significance cut-off, as a float < 1
plot_local_moran <- function(weights,var,alpha){
  lmoran <- localmoran(costs[[var]], weights, zero.policy=NULL, na.action=na.fail,
             alternative = "greater", p.adjust.method="none", mlvar=TRUE,
             spChk=NULL, sokal98=FALSE)
  
  midwest_spatial@data$cost <- costs[[var]]
  midwest_spatial$scost <- scale(midwest_spatial$cost)
  # create a lagged variable
  midwest_spatial$lag_scost <- lag.listw(weights, midwest_spatial$scost)
  
  #define significance intervals
  midwest_spatial$quad_sig <- NA
  
  # high-high quadrant
  midwest_spatial[(midwest_spatial$scost >= 0 & 
                  midwest_spatial$lag_scost >= 0) & 
                 (lmoran[, 5] <= alpha), "quad_sig"] <- "high-high"
  # low-low quadrant
  midwest_spatial[(midwest_spatial$scost <= 0 & 
                  midwest_spatial$lag_scost <= 0) & 
                 (lmoran[, 5] <= alpha), "quad_sig"] <- "low-low"
  # high-low quadrant
  midwest_spatial[(midwest_spatial$scost >= 0 & 
                  midwest_spatial$lag_scost <= 0) & 
                 (lmoran[, 5] <= alpha), "quad_sig"] <- "high-low"
  # low-high quadrant
  midwest_spatial@data[(midwest_spatial$scost <= 0 
                     & midwest_spatial$lag_scost >= 0) & 
                      (lmoran[, 5] <= alpha), "quad_sig"] <- "low-high"
  # non-significant observations
  midwest_spatial@data[(lmoran[, 5] > alpha), "quad_sig"] <- "not signif."  
  
  midwest_spatial$quad_sig <- as.factor(midwest_spatial$quad_sig)
  midwest_spatial@data$id <- rownames(midwest_spatial@data)
  
  # create plot 
  
  plot_data <- fortify(midwest_spatial, region="id")
  pdata <- merge(plot_data,midwest_spatial@data,by="id")
  
  map_plot <- (ggplot(pdata)
                + geom_polygon(aes(x=long,y=lat,group=group,fill=quad_sig))
                + ggtitle("Per Capita Cost - Local Moran's I")
                + scale_fill_manual(name="Cluster Type",values=c("red","blue","grey")))
  plot <- format_plot(map_plot) 
  
  return(list("plot"=plot,"moran"=lmoran,"data"=pdata)) }

IDs <- row.names(as(midwest_spatial, "data.frame"))

knn2 <- nb2listw(knn2nb(knearneigh(points, k = 7),row.names=IDs))
kn7_rate <- plot_local_moran(knn2)
ggsave("images/rook_local_moran.png",rook_plot )

lmoran_rook <- localmoran(costs[["cost"]], nbls2, zero.policy=NULL, na.action=na.fail,
                          alternative = "greater", p.adjust.method="none", mlvar=TRUE,
                          spChk=NULL, sokal98=FALSE)
lmoran_rook_rate <- localmoran(costs[["rate"]], nbls2, zero.policy=NULL, na.action=na.fail,
                               alternative = "greater", p.adjust.method="none", mlvar=TRUE,
                               spChk=NULL, sokal98=FALSE)

cost_queen_p10 <- plot_local_moran(q2,"cost",0.1)

ggsave("../images/cost_queen_localmoran_p10.png",cost_queen_p10)

cost_queen_p001 <- plot_local_moran(q2,"cost",0.01)

ggsave("../images/cost_queen_localmoran_p01.png",cost_queen_p001)

p <- coordinates(midwest_spatial)
p1 <- as.data.frame(p)
fit1 <- lisa.nc(x=p1$V1, y=p1$V2, z=costs[,c("cost","rate")], neigh=3)
fit1$id <- seq(from=0,to=1053,by=1)
length(fit1$p)

lisa <- cbind(fit1$id,fit1$p,fit1$correlation)
lisa <- as.data.frame(lisa)
names(lisa) <- c("id","p","corr")

midwest_spatial@data$bivar_p <- lisa$p
midwest_spatial@data$corr <- lisa$corr

#midwest_df2 <- left_join(midwest_df,lisa,by="id")
midwest_spatial@data$sig[midwest_spatial@data$bivar_p<0.05] <- 1
midwest_spatial@data$sig[midwest_spatial@data$bivar_p>=0.05] <- 0

midwest_spatial@data$sig[(midwest_spatial@data$bivar_p<0.05) 
                         & (midwest_spatial@data$corr > 0)] <- "high-high"

midwest_spatial@data$sig[(midwest_spatial@data$bivar_p<0.05) 
                         & (midwest_spatial@data$corr > 0)] <- "high-high"



midwest_spatial@data$id <- seq(from=0,to=1053,by=1)
pdata <- fortify(midwest_spatial,region="id")
pdata2 <- merge(pdata,midwest_spatial@data,by="id")
lisa_plot <- (ggplot(pdata2) 
              + geom_polygon(aes(x=long,y=lat,group=group,
                                 fill=as.factor(pdata2$sig)))
              + title("Bivariate Local Spatial Autocorrelation"))
