# Test on an actual data set

# dataset the functions for computation
code_dir <- 'E:/BB101/_專題/real_estate/code/nc_matrix.R'
source(code_dir)

# Import dependencies

library(pryr)           # memory usage
library(ggplot2)        # visualization
library(pscl)           # model diagnostics
library(ROCR)           # classifier performance evaluation
library(caret)          # training and evaluating models
library(nnet)           # multinomial logistic regression
library(GDAtools)       # Gaussian Discriminant Analysis
library(geosphere)      # geodistances
library(RMySQL)         # MySQL connection
library(RevoUtilsMath)  # multi-threading
library(mixtools)       # mixture distribution
library(MASS)

# multi-threading
setMKLthreads(4)
getMKLthreads()
memory.size(max=32710)
memory.limit()

# Connect to MySQL
db_name <- 'house'
conn <- dbConnect(MySQL(), user='', password='', 
                  dbname=db_name, host='')

# Encoding
# enc_query <- "SET NAMES udf8;"
enc_query <- "SET NAMES big5;"
dbSendQuery(conn,enc_query)

# Table queries
dataset_rental <- dbGetQuery(conn,
                  "SELECT url, Area,label, lng, lat
                  FROM rental
                  WHERE 
                    lng BETWEEN 121.45 AND 121.7
                    AND lat BETWEEN 24.9 AND 25.2
                  ;")

colnames(dataset_rental)[1] <- "ID"
# dataset <- rbind(dataset_rental,dataset_envir)
dataset <- dataset_rental

# longitude conversion for computation
dataset$lng_original <- dataset$lng
dataset$lat_original <- dataset$lat
dataset$lng <- dataset$lng - 90


# Add all necessary columns
bi_labels <- c("H")
bi_IDs <- 1
multi_labels <- get_unique_cats(dataset$label)
multi_IDs <- 1:length(multi_labels)
binary <- apply_cat_id(dataset$label,bi_labels,bi_IDs)
cat_id <- apply_cat_id(dataset$label,multi_labels,multi_IDs)
dataset <- cbind(dataset,binary,cat_id)


# Plot dataset
plot_dataset <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) +
  geom_point(aes(colour = factor(label), shape = factor(label)),size=0.5) +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
plot_dataset

plot_dataset_sub <- ggplot(data = dataset[which(dataset$cat_id == 2),],
                            aes(x=lng_original,y=lat_original)) +
  geom_point(aes(colour = factor(label), shape = factor(label)),size=0.5) +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
plot_dataset_sub


# Use agglomerated clustering to determine an appropriate k

# Trees deliver woefully poor performances...
library(cluster)
clust_result <- hclust(distm(subset(dataset,cat_id==2)[,c("lng_original","lat_original")],
                        method="euclidean"),method="ward.D2")
fit <- cutree(clust_result, k=10)
hier_result <- cbind(subset(dataset,cat_id==2),fit)


# Try k-means instead
# *** Having the appropriate set of initial values is key

km_input <- subset(dataset,cat_id==2)[,c("lng_original","lat_original")]

# manual parameters
n_clus_lng <- 5
n_clus_lat <- 2

# create evenly-spaced initial centers
lng_range <- range(km_input$lng_original)
lng_max <- lng_range[2]
lng_min <- lng_range[1]
lng_incr <- (lng_max - lng_min)/(n_clus_lng)
lat_range <- range(km_input$lat_original)
lat_max <- lat_range[2]
lat_min <- lat_range[1]
lat_incr <- (lat_max - lat_min)/(n_clus_lat)

lng_clus_coord <- seq(lng_min + lng_incr/2,lng_max - lng_incr/2,lng_incr)
lat_clus_coord <- seq(lat_min + lat_incr/2,lat_max - lat_incr/2,lat_incr)
lng_clus_coord <- rep(lng_clus_coord,n_clus_lat)
lat_clus_coord <- rep(lat_clus_coord,each=n_clus_lng)

init_centers <- matrix(c(lng_clus_coord,lat_clus_coord),ncol=2)

km_fit <- kmeans(km_input, centers=init_centers)
km_result <- cbind(subset(dataset,cat_id==2),km_fit$cluster)
kms <- silhouette(km_fit$cluster,dist(km_input))

plot_dataset_km <- ggplot(data = km_result,aes(x=lng_original,y=lat_original)) +
  geom_point(aes(colour = factor(km_result[,10]),
                 shape = factor(km_result[,10])),size=0.5) +
  scale_shape_manual(values=seq(0,10))
plot_dataset_km

mu_list <- list()
for(i in 1:nrow(km_fit$centers)){
  mu_list[[i]] <- km_fit$centers[i,]
}
mu_list




# Mixture distribution fit
sample <- dataset[which(dataset$cat_id == 2),c("lng_original","lat_original")]

mv_start_time <- Sys.time()
mv_result <- mvnormalmixEM(sample,k=10,mu=mu_list,epsilon = 10^(-8),maxit=10)
mv_end_time <- Sys.time()
mv_time_used <- mv_end_time - mv_start_time
mv_time_used

summary(mv_result)
plot(mv_result,which=2,alpha=c(0:5)/5)

# mv_result_pos <- mv_result$posterior
# mv_pos_den <- dmvnorm(mv_result$posterior[,1],
#                       mv_result$mu[[1]],
#                       mv_result$sigma[[1]])
# mv_result_pos <- cbind(mv_result_pos,mv_pos_den)
# mv_result_pos <- data.frame(mv_result_pos)

# plot_gen_2 <- ggplot(data = mv_result_pos,aes(x=comp.1,y=comp.2,z=density)) +
#   geom_contour()
# plot_gen_2

# perform the fit for all other labels.

# Perform the fit
# WARNING: this will be very time-consuming!

mv_list <- list()

for(i in 1:length(multi_labels)){
  sample <- dataset[which(dataset$cat_id == i),c("lng_original","lat_original")]
  sample <- data.matrix(sample)
  mv_start_time <- Sys.time()
  mv_result <- mvnormalmixEM(sample,k=3,epsilon = (10^(-7))/2,maxit=15)
  mv_end_time <- Sys.time()
  mv_time_used <- mv_end_time - mv_start_time
  mv_list[[i]] <- mv_result
  print(paste("Label",i,"done."))
  print(paste("Time used:",mv_time_used))
}

summary(mv_list)
# plot(mv_list[[3]],which=2,alpha=c(0:5)/5)

