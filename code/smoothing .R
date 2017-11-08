# Smooth Per Capita Medicare Costs

setwd("~/Desktop/Dropbox/UChicago/4.1/GeoSpatial/Final_Project")

source("load_medicare_data.R")

no_spaces <- make.names(names(midwest_df), unique=TRUE)
names(midwest_df) <- no_spaces
midwest_df <- sapply(midwest_df, as.numeric )
midwest_df <- as.data.frame(midwest_df)
midwest_df$raw_cost <- midwest_df$Standardized.Per.Capita.Costs * midwest_df$Beneficiaries.with.Part.A.and.Part.B

# ----- Define neighbors -----------
contained_codes <-midwest_df$code_combo
countys@data$code_combo <-  paste0(countys@data$STATEFP,countys@data$COUNTYFP)

midwest_spatial <- countys[which(countys$code_combo %in% contained_codes),]
nb_q <- poly2nb(midwest_spatial, queen=TRUE)
nb_r <- poly2nb(midwest_spatial, queen=FALSE)
knb <- knn2nb(knearneigh(coordinates(midwest_spatial), k = 4))

ls<-nb2listw(nb)

eb_data <- (midwest_df %>% 
              group_by(id) %>% 
              summarise(cost = min(raw_cost),
                            pop = min(Beneficiaries.with.Part.A.and.Part.B)) %>%
              arrange(id))

eb_data$pop[which(is.na(eb_data$pop))] <- mean(eb_data$pop[which(!is.na(eb_data$pop))])
eb_data$pop[which(eb_data$pop ==0)] <- 1
eb_data$cost[which(is.na(eb_data$cost))]<- mean(eb_data$cost[which(!is.na(eb_data$cost))])

eblocal <- EBlocal(eb_data$cost,eb_data$pop,nb_q,zero.policy = TRUE)
eblocal$id <- eb_data$id

eblocal_knn <- EBlocal(eb_data$cost,eb_data$pop,knb,zero.policy = TRUE)
eblocal_knn$id <- eb_data$id

merge <- merge(midwest_df,eblocal,by="id")
merge2 <- merge(merge,eblocal_knn,by="id")

eblocal_q <- (ggplot() + geom_polygon(data = merge, 
                         aes(x=long,y=lat,group=group,fill=est),color="black")
  + theme_bw() 
  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  + labs(x = "Longitude", y = "Latitude") 
  #+ ggtitle("Local EB Smoothing (using FOQ Continguitiy)")
  + coord_equal()
  + scale_fill_gradient2("Per Capita Cost", 
                         low = "blue", high = "red", midpoint = 1500))

eblocal_knn <- (ggplot() + geom_polygon(data = merge2, 
                                      aes(x=long,y=lat,group=group,fill=est.y),color="black")
              + theme_bw() 
              + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
              + labs(x = "Longitude", y = "Latitude") 
              + ggtitle("Local EB Smoothing (4-NN)")
              + coord_equal()
              + scale_fill_gradient2("Smoothed Per Capita Cost", 
                                     low = "blue", high = "red", midpoint = 1500))
ggsave("eblocal_knn.png",eblocal_knn)
