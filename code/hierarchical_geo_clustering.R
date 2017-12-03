library(ClustGeo)

countys@data$code_combo <- paste0(countys@data$STATEFP,countys@data$COUNTYFP)
nb <- poly2nb(countys[which(countys@data$code_combo %in% pca.train$code_combo),])

mat <- nb2mat(nb,style="B")
diag(mat) <- 1
colnames(mat) <-  pca.train$code_combo
D1<- 1-mat 
D1 <- as.dist(D1)
sdat_raw <- scale(pca.train[,-1])

D <- dist(sdat_raw)

#D1 <- as.dist(mat)
geodata <- countys[which(countys@data$code_combo %in% pca.train$code_combo),]
featw <- dist(sdat_raw)
geow <- D1

t1 <- hclustgeo_med(geodata,featw,geow,0.2,5,"hclustgeo_02_5.png")
t2 <- hclustgeo_med(geodata,featw,geow,0.7,5,"hclustgeo_07_5.png")
t3 <- hclustgeo_med(geodata,featw,geow,0.1,5,"hclustgeo_01_5.png")

t1 <- hclustgeo_med(geodata,featw,geow,0.2,4,"hclustgeo_02_4.png")
t2 <- hclustgeo_med(geodata,featw,geow,0.7,4,"hclustgeo_07_4.png")
t3 <- hclustgeo_med(geodata,featw,geow,0.1,4,"hclustgeo_01_4.png")

#' Compute hierarchical clustering with spatial constraints
#' @param geodata Spatial Polygons obj
#' @param featw dist object with feature dissimilarities
#' @param geow dist object with geographic dissimilaries
#' @param alpha mixing parameter
#' @param k number of clusters
#' @param imgfile name of file to save plot 
hclustgeo_med <- function(geodata,featw,geow,alpha,k,imgfile){
  
  tree <- ClustGeo::hclustgeo(featw,geow,alpha=alpha)
  part <- cutree(tree,k=k)
  geodata@data$cluster <- part
  geodata@data$id <- seq(from=1,to=nrow(geodata@data),by=1)
  plot_clust <- fortify(geodata,region="id")
  
  plot_clust2 <- merge(plot_clust,geodata@data,by.x="id",by.y="id")
  
  hclust_con <- (ggplot(plot_clust2) 
                 + geom_polygon(aes(x=long,y=lat,
                                    group=group,
                                    fill=as.factor(plot_clust2$cluster)))
                 + labs(color="Cluster")
                 + scale_fill_brewer(name="Cluster",palette = "Set1")
                 + ggtitle(paste0("Constrained Hierarchical Clustering \n (k=",k,", alpha = ",alpha,")")))
  
  hclust_con <- format_plot(hclust_con)
  ggsave(imgfile,hclust_con)
  return(hclust_con)
}
