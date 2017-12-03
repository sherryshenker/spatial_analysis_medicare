# Spatial Autocorrelation in Medicare Costs

setwd("~/spatial_analysis_medicare/code")

source("load_medicare_data.R")

contained_codes <-midwest_data$`State and County FIPS Code`
countys@data$code_combo <-  paste0(countys@data$STATEFP,countys@data$COUNTYFP)

midwest_spatial <- countys[which(countys$code_combo %in% contained_codes),]

midwest_data$code_combo <- midwest_data$`State and County FIPS Code`
midwest_spatial@data <- left_join(midwest_spatial@data,midwest_data,
                                  by="code_combo")


merged <- merge(fortify(midwest_spatial,region="code_combo"),midwest_data,
                by.x="id",by.y="State and County FIPS Code")

points <- coordinates(midwest_spatial)
points_df <- as.data.frame(points)

costs <- (midwest_df[which(!is.na(midwest_df$Standardized.Per.Capita.Costs)),] %>% group_by(id) %>% summarise(cost = min(Standardized.Per.Capita.Costs),
                                                   pop = min(Beneficiaries.with.Part.A.and.Part.B),
                                                   rate = min(Hospital.Readmission.Rate)) %>%
                arrange(id))

# ----- Correlogram for Global Spatial Autocorrelatoin -----------------

c2 <- correlog(points_df$V1, points_df$V2, midwest_spatial@data$`Actual Per Capita Costs`, w = NULL, 1, resamp = 100, 
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
county_med_spatial <- countys[which(countys@data$code_combo %in% midwest_data$`State and County FIPS Code`),]

nb2 <- poly2nb(county_med_spatial, queen=FALSE)
nbls2 <- nb2listw(nb2,zero.policy=TRUE )

q1 <- poly2nb(county_med_spatial, queen=TRUE)
q2 <- nb2listw(q1,zero.policy=TRUE )

#' Function for mapping local spatial clusters
#' Using Moran's I statustic
#' Requires: 
#' - weights: a neighborweight list object
#' - var: variable of interest as a string
#' - alpha: significance cut-off, as a float < 1
#' - title: string
#' Optional:
#' - correct: a boolean, if TRUE, apply bonferroni correction
plot_local_moran <- function(weights,var,alpha,title,correct=FALSE){
  lmoran <- localmoran(midwest_data[[var]], weights, zero.policy=NULL, na.action=na.fail,
             alternative = "two.sided", p.adjust.method="none", mlvar=TRUE,
             spChk=NULL, sokal98=FALSE)

  county_med_spatial@data$var <- midwest_data[[var]]
  county_med_spatial@data$scost <- scale(county_med_spatial@data$var)
  # create a lagged variable
  county_med_spatial@data$lag_scost <- lag.listw(weights,county_med_spatial@data$scost)
  
  #define significance intervals
  county_med_spatial$quad_sig <- NA
 
  # bonferroni correction
  if (correct){
    lmoran[, 5] <- p.adjust(lmoran[, 5],"bonferroni")
  }

  
  county_med_spatial@data$sig <- lmoran[,5]
  
  county_med_spatial[(county_med_spatial$scost >= 0 & 
                  county_med_spatial$lag_scost >= 0) & 
                 (lmoran[, 5] <= alpha), "quad_sig"] <- "high-high"
  # low-low quadrant
  county_med_spatial[(county_med_spatial$scost <= 0 & 
                  county_med_spatial$lag_scost <= 0) & 
                 (lmoran[, 5] <= alpha), "quad_sig"] <- "low-low"
  # high-low quadrant
  county_med_spatial[(county_med_spatial$scost >= 0 & 
                  county_med_spatial$lag_scost <= 0 & 
                 lmoran[, 5] <= alpha), "quad_sig"] <- "high-low"
  # low-high quadrant
  county_med_spatial@data[(county_med_spatial$scost <= 0 
                     & county_med_spatial$lag_scost >= 0 & 
                      lmoran[, 5] <= alpha), "quad_sig"] <- "low-high"
  # non-significant observations
  county_med_spatial@data[(lmoran[, 5] > alpha), "quad_sig"] <- "not signif."  
  print(dim(county_med_spatial@data[which(county_med_spatial@data$quad_sig != "not signif."),]))
  county_med_spatial$quad_sig <- as.factor(county_med_spatial$quad_sig)
  county_med_spatial@data$id <- rownames(county_med_spatial@data)
  
  # create plot 
  
  plot_data <- fortify(county_med_spatial, region="id")
  pdata <- merge(plot_data,county_med_spatial@data,by="id")
  
  map_plot <- (ggplot(pdata)
                + geom_polygon(aes(x=long,y=lat,group=group,fill=quad_sig),color="black")
                + ggtitle(title)
                + scale_fill_manual(name="Cluster Type",values=c("red","pink","lightblue","blue","grey")))
  plot <- format_plot(map_plot) 
  
  return(list("plot"=plot,"moran"=lmoran,"data"=pdata)) }

# Define 7NN Weights matrix
IDs <- row.names(as(county_med_spatial, "data.frame"))
knn2 <- nb2listw(knn2nb(knearneigh(coordinates(county_med_spatial), k = 7),row.names=IDs))


cost_kn7_p10 <- plot_local_moran(knn2,"Actual Per Capita Costs",0.05,"Per Capita Costs - Local Moran's I")
ggsave("final/kn7_p05_cost.png",cost_kn7_p10$plot)

pca_kn7_p10 <- plot_local_moran(knn2,"PC1",0.05,"First PCA Component - Local Moran's I")
ggsave("final/kn7_p05_pca.png",pca_kn7_p10$plot)

cost_queen_p10 <- plot_local_moran(q2,"Actual Per Capita Costs",0.05,"Actual Per Capita Costs - Local Moran's I (Queen)")
cost_queen <- cost_queen_p10$plot
ggsave("final/moran_queen_cost_0.05.png",cost_queen)


cost_queen_bonf <- plot_local_moran(q2,"Actual Per Capita Costs",0.05,"Actual Per Capita Costs (Bonferroni Corection)",correct = TRUE)
cost_b <- cost_queen_bonf$plot
ggsave("final/moran_bonf_cost.png",cost_b)

pca_queen_p05 <- plot_local_moran(q2,"PC1",0.05)
pca_queen <- pca_queen_p05$plot
ggsave("final/pca_queen.png",pca_queen)

pca_queen_p05 <- plot_local_moran(knn2,"PC1",0.05,"First Principal Component (7NN Weights)")
pca_queen <- pca_queen_p05$plot
ggsave("final/pca_kn7_moran.png",pca_queen)

pca_queen_p05 <- plot_local_moran(q2,"PC1",0.05,"First Principal Component (Queen Contiguity Weights)")
pca_queen <- pca_queen_p05$plot
ggsave("final/pca_q2_moran.png",pca_queen)

hos_kn7_p10 <- plot_local_moran(knn2,"Average HCC Score",0.05,"Average HCC Score - Local Moran's I")
ggsave("final/kn7_p05_hcc.png",hos_kn7_p10$plot)

# ------- Local G --------------
localGvalues <- localG(x = costs$cost, listw = knn2, zero.policy = TRUE)

midwest_spatial@data$localg <- localGvalues
midwest_spatial@data$localg_sig <- "Not Significant"
midwest_spatial@data$localg_sig[which(midwest_spatial@data$localg>1.96)] <- "High"
midwest_spatial@data$localg_sig[which(midwest_spatial@data$localg< (-1.96))] <- "Low"
plot_data <- fortify(midwest_spatial, region="id")
pdata <- merge(plot_data,midwest_spatial@data,by="id")

map_plot <- (ggplot(pdata)
             + geom_polygon(aes(x=long,y=lat,group=group,fill=localg_sig))
             + ggtitle("Per Capita Cost - Local Getis-Ord")
             + scale_fill_manual(name="Cluster Type",values=c("red","blue","grey")))
getisplot <- format_plot(map_plot) 


# ------- global autocorrelation -------------

moran.test(costs$cost,knn2,zero.policy = TRUE,alternative = "two.sided",randomisation = FALSE)

