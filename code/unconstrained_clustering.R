# Non-spatially constrained clustering
il <- midwest_spatial[which(midwest_spatial$STATEFP=="17"),]

subset <- midwest_df[which(midwest_df$order==1 & 
                             midwest_df$state_fips=="17" & 
                             as.character(midwest_df$code_combo) %in% il$code_combo),]

pca.train <- subset[,c(8,20:50)]
vds <- scale(pca.train[,-1])
vdist <- dist(vds)
hc1 <- hclust(vdist,method="complete")
plot(hc1,main="Complete linkage",xlab="",sub="")
hc1_4 <- cutree(hc1,4)

pca.train$cluster <- hc1_4



il@data$cluster <- pca.train$cluster
il@data$id <- seq(from=1,to=102,by=1)
cluster_plot <- fortify(il,region="id")
cluster_plot$id <- as.character(cluster_plot$id)
il@data$id <- as.character(il@data$id)
cluster_plot <- left_join(cluster_plot,il@data,by="id")

cluster_p <- (ggplot(cluster_plot) 
              + geom_polygon(aes(x=long,y=lat,group=group,
                                 fill=as.factor(cluster_plot$cluster)))
              + ggtitle("Unconstrained hierarchical clustering")
              + theme_bw()
              + scale_fill_manual(name="Cluster",
                                  values=c("maroon","lightblue","seagreen4","peachpuff2")))

cluster_p <- format_plot(cluster_p)
ggsave("unconstrained_hclust_4.png",cluster_p)

km1_4 <- kmeans(vds,4,nstart=20)
pca.train$cluster2 <- km1_4$cluster
il@data$cluster2 <- pca.train$cluster2

cluster_plot <- fortify(il,region="id")
cluster_plot$id <- as.character(cluster_plot$id)
il@data$id <- as.character(il@data$id)
cluster_plot <- left_join(cluster_plot,il@data,by="id")

kmeansp <- (ggplot(cluster_plot) 
              + geom_polygon(aes(x=long,y=lat,group=group,
                                 fill=as.factor(cluster_plot$cluster2)))
              + ggtitle("Unconstrained kmeans clustering")
              + theme_bw()
              + scale_fill_manual(name="Cluster",values=c("maroon","lightblue","seagreen4","peachpuff2")))

kmeansp <-format_plot(kmeansp)

ggsave("unconstrained_kmeans.png",kmeansp)
