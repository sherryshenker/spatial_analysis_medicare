# Principal Component Analysis and Clustering

setwd("~/spatial_analysis_medicare/code")

source("load_medicare_data.R")

subset <- midwest_df[which(midwest_df$order==1),]

# Drop nulls

pca.train <- subset[,c(8,20:50)]  

pca.train <- pca.train[rowSums(is.na(pca.train)) == 0, ]

# PCA

pca <- prcomp(pca.train[,-1], scale. = T)

screeplot(pca, type="lines",col=3)

pca$rotation[,c(1:4)]
biplot(pca, scale = 0,cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)

summary(pca$x)

pcs1 <- as.data.frame(pca$x)
pcs1$code_combo <- as.integer(pca.train$code_combo)

p2 <- left_join(pcs1,subset[,c("code_combo","state_fips")],by="code_combo")

names(pcs1)

scores <- ggplot(pcs1,aes(x=PC1,y=PC2)) + geom_point() + ggtitle("PCA Scores") + theme_bw()
scores <- (ggplot(p2,aes(x=PC1,y=PC2))
           + geom_point(aes(color=as.factor(p2$state_fips)))
           + ggtitle("PCA Scores") + 
             theme_bw()
           + scale_color_discrete(name="STATE",guide=FALSE)
           )
setwd("~/spatial_analysis_medicare")
ggsave("images/pca_scores_color.png",scores)

scaled_data <- scale(pca.train[,-1])
vv <- cor(scaled_data,pca$x)
vv2 <- vv**2
pca1_corr <- as.data.frame(vv2[,c(1)])
names(pca1_corr) <- c("corr")
pca1_corr$var <- rownames(pca1_corr)

sorted <- pca1_corr %>% arrange(-corr)

top15 <- sorted[c(1:10),]

bar_chart <- (ggplot(top15) 
              + geom_col(aes(x=reorder(var, -corr),y=corr,fill=var)) 
              
              + ggtitle("Correlation between First PCA and Data")
              + labs(x="Data column",y="Correlation")
              + guides(fill=FALSE)
              +theme_bw()+
                coord_fixed(ratio=.3) + 
                coord_flip() + 
                
                theme(text = element_text(size=16),
                      axis.text.x = element_text(angle = 90, hjust = 1),
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank()) +
                scale_fill_viridis(option="magma",discrete=TRUE))

ggsave("pca_bar_chart.png",bar_chart)
