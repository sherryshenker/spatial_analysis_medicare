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

plot_local_moran <- function(weights,var,alpha){
  lmoran_rook <- localmoran(costs[[var]], weights, zero.policy=NULL, na.action=na.fail,
             alternative = "greater", p.adjust.method="none", mlvar=TRUE,
             spChk=NULL, sokal98=FALSE)
  
  midwest_spatial@data$cost <- costs[[var]]
  midwest_spatial$scost <- scale(midwest_spatial$cost)
  # create a lagged variable
  midwest_spatial$lag_scost <- lag.listw(weights, midwest_spatial$scost)

  midwest_spatial$quad_sig <- NA
  
  # high-high quadrant
  midwest_spatial[(midwest_spatial$scost >= 0 & 
                  midwest_spatial$lag_scost >= 0) & 
                 (lmoran_rook[, 5] <= alpha), "quad_sig"] <- "high-high"
  # low-low quadrant
  midwest_spatial[(midwest_spatial$scost <= 0 & 
                  midwest_spatial$lag_scost <= 0) & 
                 (lmoran_rook[, 5] <= alpha), "quad_sig"] <- "low-low"
  # high-low quadrant
  midwest_spatial[(midwest_spatial$scost >= 0 & 
                  midwest_spatial$lag_scost <= 0) & 
                 (lmoran_rook[, 5] <= alpha), "quad_sig"] <- "high-low"
  # low-high quadrant
  midwest_spatial@data[(midwest_spatial$scost <= 0 
                     & midwest_spatial$lag_scost >= 0) & 
                      (lmoran_rook[, 5] <= alpha), "quad_sig"] <- "low-high"
  # non-significant observations
  midwest_spatial@data[(lmoran_rook[, 5] > alpha), "quad_sig"] <- "not signif."  
  
  midwest_spatial$quad_sig <- as.factor(midwest_spatial$quad_sig)
  midwest_spatial@data$id <- rownames(midwest_spatial@data)
  
  
  plot_data <- fortify(midwest_spatial, region="id")
  pdata <- merge(plot_data,midwest_spatial@data,by="id")
  
  rook_plot <- (ggplot(pdata)
                + geom_polygon(aes(x=long,y=lat,group=group,fill=quad_sig))
                + ggtitle("Per Capita Cost - Local Moran's I")
                + scale_fill_manual(name="Cluster Type",values=c("red","blue","grey")))
  plot <- format_plot(rook_plot) 
return(plot)}

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
