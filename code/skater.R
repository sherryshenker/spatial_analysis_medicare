#' Constrained clustering of Medicare Midwest Data
#' Spatial Data Science Assignment 7

setwd("~/spatial_analysis_medicare/code")

source("load_medicare_data.R")

subset <- midwest_df[which(midwest_df$order==1&midwest_df$STATEFP=="17"),]

pca.train <- subset[,c(8,20:50)]  

pca.train <- pca.train[rowSums(is.na(pca.train)) == 0, ]

# PCA

pca <- prcomp(pca.train[,-1], scale. = T)
pcs1 <- as.data.frame(pca$x)
pcs1$AREA_ID <- as.integer(pca.train$code_combo)

sdat <- scale(pcs1[,c(1:10)])
sdat_raw <- scale(pca.train[,-1])

#' Implement SKATER spatial clustering
#' Arguments:
#' @param data, dataframe with one row of feature values per data point
#' @param nb, neighbor object
#' @param k, value for the number of cuts, results in k+1 clusters'
#' @param spatial_data, for plotting
#' @img_name string, for saving the image
#' 
#' @return plot, ggplot object
skater_cluster(data,nb,k,spatial_data,img_name){
  #prep inputs
  data <- scale(data)
  lcosts <- nbcosts(nb,data)
  lcosts.w <- nb2listw(nb,lcosts,style="B")
  mst <- mstree(lcosts.w)
  
  #cluster data
  clus4 <- skater(mst[,1:2],data,k)
  ccs4 <- clus4$groups
  
  #plot 
  spatial_data@data$cluster <- ccs4
  spatial_data@data$id <- seq(from=1,to=nrow(spatial_data@data),by=1)
  plt <- fortify(spatial_data,id="id")
  plt <- merge(plt,il@data,by.x="id",by.y=0)
  
  skater_q <- (ggplot(plt) 
               + geom_polygon(aes(x=long,y=lat,
                                  group=group,fill=as.factor(il_plt$cluster)))
               + labs(color="Cluster")
               + scale_fill_manual(name="Cluster",
                                   values=c("maroon","lightblue","seagreen4","peachpuff2"))
               + ggtitle(paste0("SKATER Clustering (k=",k,")")))
  
  skater_q <- format_plot(skater_q)
  ggsave(img_name,skater_q)
}

nb <- poly2nb(midwest_spatial[which(midwest_spatial$STATEFP=="17"),])


il <- midwest_spatial[which(midwest_spatial$STATEFP=="17"),]


clus4 <- skater(medicare.mst[,1:2],sdat,7)
ccs4 <- clus4$groups

il <- midwest_spatial[which(midwest_spatial$STATEFP=="17"),]
il@data$cluster <- ccs4
il_plt <- fortify(il,id="GEOID")
il_plt <- merge(il_plt,il@data,by.x="id",by.y=0)

skater_q6 <- (ggplot(il_plt) 
             + geom_polygon(aes(x=long,y=lat,
                                group=group,fill=as.factor(il_plt$cluster)))
             +  scale_fill_brewer(name="Cluster",palette="Accent")
             + ggtitle("SKATER Clustering (k=8)"))
skater_q6 <- format_plot(skater_q6)
ggsave("skater_q_8.png",skater_q6)
