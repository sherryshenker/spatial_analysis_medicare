# Implement PCA on Medicare data for the midwest

# Keep columns with no nulls for PCA
na_count <-sapply(midwest_data, function(y) sum(length(which(is.na(y)))))
na_count <- as.data.frame(na_count)
na_count$col <- rownames(na_count)
no_nulls <- na_count[which(na_count$na_count==0),]

keep <- c()
for (col in no_nulls$col){
  if (grepl("Costs",col )){
    if (grepl( "Per",col)|grepl("as %",col)){
      keep <- c(keep,col)
    }
    else{
      print(col)
    }
  }
  else{
    keep <- c(keep,col)
  }
}

keep <- c(keep,"State and County FIPS Code")

pca.train <- midwest_data[,which(names(midwest_data) %in% keep)]

pca <- prcomp(pca.train[,c(2:83)], scale. = T)
screeplot(pca, type = "lines", col = "blue",
          main = "Scree plot for PCA",
          xlab = "Principal Component Number",
          )

pcld <- as.data.frame(pca$rotation)
pcld$abs_pc1 <- abs(pcld$PC1)
pcld$abs_pc2 <- abs(pcld$PC2)
pcld$col <- rownames(pcld)
ordered <- pcld %>% arrange(-abs_pc1)
ordered2 <- pcld %>% arrange(-abs_pc2)

pcs1 <- as.data.frame(pca$x)
pcs1$code_combo <- as.integer(pca.train$`State and County FIPS Code`)
pcs1$state <- pca.train$state_fips
# choropleth map of PCA component

pca_plt <- left_join(county_spatial,pcs1,on="code_combo")

midwest_data$PC1 <- pcs1$PC1
pca_map <- format_plot(ggplot(pca_plt) 
            + geom_polygon(aes(x=long,y=lat,
                               group=group,fill=PC1),color="black")
            + scale_fill_distiller(name="First PC",palette="Spectral")
            + ggtitle("Value of First Principal Component by County"))

setwd("~/spatial_analysis_medicare/images")
ggsave("pca_map.png",pca_map)

pca_map2 <- format_plot(ggplot(pca_plt) 
                       + geom_polygon(aes(x=long,y=lat,
                                          group=group,fill=PC2),color="black")
                       + scale_fill_distiller(name="Second PC",palette="Spectral")
                       + ggtitle("Value of Second Principal Component by County"))

ggsave("pca_map2.png",pca_map2)

scores <- (ggplot(pcs1,aes(x=PC1,y=PC2))
           + geom_point(aes(color=as.factor(pcs1$state)),size=1,alpha=0.5)
           + ggtitle("PCA Scores") + 
             theme_bw()
           + scale_color_discrete(name="STATE",guide=FALSE)
           + scale_x_continuous(limits=c(-10,40))
           + scale_y_continuous(limits=c(-15,20))
           + theme(text=element_text(size=16)))
ggsave("pca_scores_final.png",scores)

