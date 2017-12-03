# Generate plots of Medicare Costs per Capita for Midwest and Northeast

setwd("~/Desktop/Dropbox/UChicago/4.1/GeoSpatial/Final_Project")

source("load_medicare_data.R")

sd(midwest_df$Actual.Per.Capita.Costs,na.rm=TRUE)/mean(midwest_df$Actual.Per.Capita.Costs,na.rm=TRUE)
#0.5967032

sd(midwest_df$Average.HCC.Score,na.rm=TRUE)/mean(midwest_df$Average.HCC.Score,na.rm=TRUE)
#0.08739848




scaled <- as.data.frame(scale(midwest_df[,c("Actual.Per.Capita.Costs","Average.HCC.Score")]))

var(scaled$Actual.Per.Capita.Costs[which(!is.na(scaled$Actual.Per.Capita.Costs))])

mean <- mean(midwest_df$Actual.Per.Capita.Costs[which(!is.na(midwest_df$Actual.Per.Capita.Costs))])

per_capita_costs <- (ggplot(midwest_df) 
                     + geom_polygon(aes(x = long,y=lat,group=group,fill=Actual.Per.Capita.Costs),color="black")
                     + coord_fixed()
                     + scale_fill_gradient2("Per Capita Cost", 
                                            low = "blue", high = "red", midpoint = mean)
                     + ggtitle("Per Capita Medicare Costs by County"))

setwd("~/spatial_analysis_medicare/images")

p2 <- format_plot(per_capita_costs)
ggsave("per_capita_midwest2.png",p2)

mean <- mean(midwest_df$Standardized.Per.Capita.Costs[which(!is.na(midwest_df$Standardized.Per.Capita.Costs))])
standardized_costs <- (ggplot(midwest_df) 
                     + geom_polygon(aes(x = long,y=lat,group=group,fill=Standardized.Per.Capita.Costs),color="black")
                     + coord_fixed()
                     + scale_fill_gradient2("Per Capita Cost", 
                                            low = "blue", high = "red", midpoint = mean)
                     + ggtitle("Standardized Per Capita Medicare Costs by County"))
st <- format_plot(standardized_costs)

ggsave("standardized.png",st)

med_midwest <- medicare[which(medicare$state_fips %in% midwest),]

med_midwest <- sapply(med_midwest, as.numeric )
med_midwest <- as.data.frame(med_midwest)

#remove spaces from names
med_midwest <- midwest_data
no_spaces <- make.names(names(med_midwest), unique=TRUE)
names(med_midwest) <- no_spaces



norm <- as.data.frame(med_midwest$Actual.Per.Capita.Costs)
norm$type <- "Actual"
names(norm) <- c("Price","Type")
standard <-  as.data.frame(med_midwest$Standardized.Per.Capita.Costs)
standard$type <- "Standardized"
names(standard) <- c("Price","Type")

distributions <- rbind(norm,standard)
distributions$Price <- as.numeric(distributions$Price)

compare <- (ggplot(distributions)
            + geom_histogram(aes(Price),fill="lightblue",color="black") 
            + facet_wrap(~Type)
            + geom_vline(xintercept = mean(distributions$Price),color="red")
            + ggtitle("Actual vs Standardized Per Capita Medicare Costs")
            + labs(x="Cost",y="Frequency")
            + theme_bw()
            + theme(strip.text.x = element_text(size = 14),
                    text = element_text(size=16),
                    legend.text=element_text(size=16))
            )
ggsave("compare_actual_standard.png",compare)


beneficiaries_costs <- (ggplot(med_midwest) 
                        + geom_point(aes(x=Beneficiares.with.Part.A.and.Part.B,
                                         y=Standardized.Per.Capita.Costs,color=State))
                                  + theme_bw()
                        + ggtitle("Relationship between Number of Medicare Beneficiaries and Costs"))

ggsave("beneficiaries_costs.png",beneficiaries_costs)

visits_costs <- (ggplot(med_midwest) 
                        + geom_point(aes(x=Emergency.Department.Visits,
                                         y=Standardized.Per.Capita.Costs,color=as.factor(med_midwest$State)))
                        + theme_bw()
                 + labs(color="State FIPS")
                 + scale_y_continuous(labels=comma)
                 + scale_x_continuous(labels=comma)
                        + ggtitle("Relationship between Emergency Department Visits and Costs"))

ggsave("er_visits_costs.png",visits_costs)

lm <- lm(`Standardized Per Capita Costs`~`Hospital Readmission Rate`,data=midwest_data)

lm <- lm(`Standardized Per Capita Costs`~`Beneficiaries with Part A and Part B`,data=midwest_data)

lm <- lm(`Standardized Per Capita Costs`~`Average HCC Score`,data=midwest_data)


long_data <- med_midwest[,c("Standardized.Per.Capita.Costs",
                            "Beneficiaries.with.Part.A.and.Part.B",
                            "Emergency.Department.Visits.per.1000.Beneficiaries",
                            "Hospital.Readmission.Rate",
                            "Average.HCC.Score")] %>% gather(indicator, value,
                                                                c("Beneficiaries.with.Part.A.and.Part.B",
                                                                    "Emergency.Department.Visits.per.1000.Beneficiaries",
                                                                    "Hospital.Readmission.Rate",
                                                                  "Average.HCC.Score"))
long_data$indicator <- as.factor(long_data$indicator)


scatterplot <- (ggplot(long_data) 
                 + geom_point(aes(x=value,
                                  y=`Standardized.Per.Capita.Costs`),fill="lightblue",color="black",shape=21)
                 +  geom_smooth(method='lm',formula=y~x,aes(x=value,
                                                            y=`Standardized.Per.Capita.Costs`))
                  + theme_bw()
                 + labs(color="State FIPS")
                 + scale_y_continuous(labels=comma)
                 + scale_x_continuous(labels=comma)
                + facet_wrap(~indicator,ncol=1,scales="free")
                + theme(strip.text.x = element_text(size = 14),
                      text = element_text(size=16),
                      legend.text=element_text(size=16))
                 + ggtitle("(Poor) Predictors of Medicare Costs"))
ggsave("scatter_matrix.png",scatterplot)

# ----------- which variables are most correlated with per capita costs --------

med_mat <- as.matrix(med_midwest[,!names(med_midwest) %in% c("state_fips","county_fips",
                                     "State","State.and.County.FIPS.Code","County")])

cor_mat <- cor(med_mat)
cor_df <- as.data.frame(cor_mat)

cost_corr <- as.data.frame(cor_df$Standardized.Per.Capita.Costs) 
rownames(cost_corr) <- rownames(cor_df)

names(cost_corr) <- c("correlation")

ordered <- cost_corr[order(-abs(cost_corr$correlation)), , drop = FALSE]


sub <- ordered[-grep("Costs", rownames(ordered)),c("correlation"),drop=FALSE]

correlation_dist <- (ggplot(as.data.frame(sub)) 
                     + geom_histogram(aes(correlation),fill="lightblue",color='black')
                     + theme_bw()
                     + labs(x="Correlation w. Cost",y="Number of Variables")
                     + theme(strip.text.x = element_text(size = 14),
                             text = element_text(size = 16),
                             legend.text = element_text(size = 16))
                     + ggtitle("Distribution of Correlations w. Medicare Costs"))
                     
ggsave("correlation_hist.png",correlation_dist)

# ------- demographics

mean <- mean(midwest_df$age[which(!is.na(midwest_df$age))])

age <- (ggplot(midwest_df) 
                     + geom_polygon(aes(x = long, y = lat, 
                                        group = group, fill = age), 
                                         color="black")
                     + coord_fixed()
                     + scale_fill_gradient2("Average Age", 
                                            low = "blue", high = "red", midpoint = mean)
                     + ggtitle("Average Age by County"))

setwd("~/spatial_analysis_medicare/images")

p2 <- format_plot(age)
ggsave("age_midwest.png",p2)

mean <- mean(midwest_df$Average.HCC.Score[which(!is.na(midwest_df$Average.HCC.Score))])

hcc <- (ggplot(midwest_df) 
        + geom_polygon(aes(x = long,y=lat,group=
                             group,fill=Average.HCC.Score), color="black")
        + coord_fixed()
        + scale_fill_gradient2("Average HCC Score", 
                               low = "blue", high = "red", midpoint = mean)
        + ggtitle("Average HCC Score by County"))

setwd("~/spatial_analysis_medicare/images")

p2 <- format_plot(hcc)
ggsave("hcc_midwest.png",p2)


mean <- mean(midwest_df$`Actual Per Capita Costs`[which(!is.na(midwest_df$`Actual Per Capita Costs`))])

per_capita_costs <- (ggplot(midwest_df) 
                     + geom_polygon(aes(x = long,y=lat,group=group,fill=`Actual Per Capita Costs`))
                     + coord_fixed()
                     + scale_fill_gradient2("Per Capita Cost", 
                                            low = "blue", high = "red", midpoint = mean)
                     + ggtitle("Per Capita Medicare Costs by County"))

setwd("~/Desktop/Dropbox/UChicago/4.1/GeoSpatial/Final_Project/images")

p2 <- format_plot(per_capita_costs)
ggsave("per_capita_midwest.png",p2)
