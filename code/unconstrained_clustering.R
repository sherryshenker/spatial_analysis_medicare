# Non-spatially constrained clustering

vds <- scale(pca.train[,-1])
vdist <- dist(vds)
hc1 <- hclust(vdist,method="complete")
plot(hc1,main="Complete linkage",xlab="",sub="")
hc1_4 <- cutree(hc1,4)

pca.train$cluster <- hc1_4

midwest_spatial@data$cluster <- pca.train$cluster

cluster_plot <- fortify(midwest_spatial,region="id")
cluster_plot$id <- as.character(cluster_plot$id)
midwest_spatial@data$id <- as.character(midwest_spatial@data$id)
cluster_plot <- left_join(cluster_plot,midwest_spatial@data,by="id")

cluster_p <- (ggplot(cluster_plot) 
              + geom_polygon(aes(x=long,y=lat,group=group,
                                 fill=as.factor(cluster_plot$cluster)))
              + ggtitle("Unconstrained hierarchical clustering")
              + theme_bw()
              + scale_fill_manual(name="Cluster",values=c("maroon","lightblue","seagreen4","peachpuff2")))

cluster_p <- format_plot(cluster_p)

km1_4 <- kmeans(vds,4,nstart=20)
pca.train$cluster2 <- km1_4$cluster
midwest_spatial@data$cluster2 <- pca.train$cluster2

kmeansp <- (ggplot(cluster_plot) 
              + geom_polygon(aes(x=long,y=lat,group=group,
                                 fill=as.factor(cluster_plot$cluster2)))
              + ggtitle("Unconstrained kmeans clustering")
              + theme_bw()
              + scale_fill_manual(name="Cluster",values=c("maroon","lightblue","seagreen4","peachpuff2")))

kmeansp <-format_plot(kmeansp)
