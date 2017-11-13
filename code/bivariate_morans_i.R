library(stringr)
library(spdep)
library(rgdal)
library(magrittr)
library(ggplot2)

#' Plot Bivariate Moran's I 

#source of code: https://gist.github.com/rafapereirabr/5348193abf779625f5e8c5090776a228

#======================================================
# Programming some functions

# Bivariate Moran's I
moran_I <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  
  xp <- scale(x)[, 1]
  yp <- scale(y)[, 1]
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  
  list(global = global, local  = as.numeric(local))
}


# Permutations for the Bivariate Moran's I
simula_moran <- function(x, y = NULL, W, nsims = 2000){
  
  if(is.null(y)) y = x
  
  n   = nrow(W)
  IDs = 1:n
  
  #xp <- scale(x)[, 1]
  W[which(is.na(W))] <- 0
  
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  
  print(length(xp))
  print(dim(W))
  
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
 
  local_sims  <- (xp*W%*%y_s)
  
  list(global_sims = global_sims,
       local_sims  = local_sims)
}



#======================================================
# Adjacency Matrix (Queen)

lw <- nb2listw(knn2nb(knearneigh(points, k = 4),row.names=IDs),style = "B", zero.policy = T)

nb <- poly2nb(midwest_spatial, queen=TRUE)
nb <- knn2nb(knearneigh(points, k = 3),row.names=IDs)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W/rowSums(W))
W[which(is.na(W))] <- 0


#======================================================
# Calculating the index and its simulated distribution
# for global and local values

m <- moran_I(costs$cost, costs$rate, W)

# Global Moral
global_moran <- m[[1]][1]
#> 0.2218409

# Local values
m_i <- m[[2]] 

# local simulations
local_sims <- simula_moran(costs$cost, costs$rate, W)$local_sims


# global pseudo p-value  
# get all simulated global moran
global_sims <- simula_moran(costs$cost, costs$rate, W)$global_sims

# Proportion of simulated global values taht are higher (in absolute terms) than the actual index 
moran_pvalue <- sum(abs(global_sims) > abs( global_moran )) / length(global_sims)
#> 0


# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig       <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )



#======================================================
# Preparing for plotting

midwest_spatial@data$sig <- sig


# Identifying the LISA clusters
xp <- scale(costs$cost)[,1]
yp <- scale(costs$rate)[,1]


patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")

patterns[midwest_spatial@data$sig==0] <- "Not significant"
midwest_spatial@data$patterns <- patterns


# Rename LISA clusters
midwest_spatial@data$patterns2 <- factor(midwest_spatial@data$patterns, levels=c("High.High", "High.Low", "Low.High", "Low.Low", "Not significant"),
                           labels=c("High cost - High readmit rate", "High cost - Low readmit rate", "Low cost - High readmit rate","Low cost - Low readmit rate", "Not significant"))



final <- fortify(midwest_spatial,region="id")
final$id <- as.numeric(final$id)
final2 <- left_join(final,midwest_spatial@data,by="id")
### PLOT

bivariate <- (ggplot() +
  geom_polygon(data=final2, aes(x=long,y=lat,group=group,fill=patterns2), color="NA") +
  scale_fill_manual(values = c("red", "pink", "light blue", "dark blue", "grey80")) + 
  guides(fill = guide_legend(title="LISA clusters")) +
  theme_bw() + ggtitle("Bivariate Moran's I")
  + theme(strip.text.x = element_text(size = 14),
          text = element_text(size=16),
          legend.text=element_text(size=12),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  + coord_fixed())
setwd("~/spatial_analysis_medicare")
ggsave("images/bivariate_morans.png",bivariate)
