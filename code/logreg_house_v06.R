# Set working directory
wd <- 'E:/BB101/_專題/real_estate/data'
setwd(wd)

# Load data
source <- read.csv('test_data_03.csv',header=T,na.strings=c(""))


# longitude conversion for computation
source$lng_original <- source$lng
source$lat_original <- source$lat
source$lng <- source$lng - 90


# Shuffle up the data order for a more representative outcome
set.seed(1)

# Download packages if necessary.
# Import the relevant packages.

# For: checking memory usage
library(pryr)

# install.package('sp')
# install.package('geosphere')
# For: converting latitude and longitude to distance
library(sp)
library(geosphere)

# install.packages('ggplot2')
# For: data visualization
library('ggplot2')
# install.packages('pscl')
# For: model diagnostics
library(pscl)
# install.packages('ROCR')
# For: classifier performance evaluation
library(ROCR)
# install.packages('caret')
# For: training and evaluating models
library(caret)
# install.packages('nnet')
# For: multinomial logistic regression
library(nnet)
# install.packages('mlbench')
# For: generating random variables
# library(mlbench)
# install.packages('e1071')
# library(e1071)

# install.packages('GDAtools')
# For: Gaussian Discriminant Analysis
# library(GDAtools)

# install.packages('MASS')
library(MASS)

# Define category id mapping

# Binary classification

cat_id_bi <- function(category){
  if(category == 'rental'){
    return(1)
  }
  else{
    return(0)
  }
}

# Multiple classification (more than 2 classes)

cat_id_multi <- function(category){
  if(category == 'rental'){
    return(1)
  }
  else if(category == 'parking_lot'){
    return(2)
  }
  else if(category == 'commercial'){
    return(3)
  }
  else{
    return(0)
  }
}

# Add a new column for binary classification

binary <- sapply(source$category,cat_id_bi)
cat_id <- sapply(source$category,cat_id_multi)
source <- cbind(source,binary,cat_id)


# Scale lng and lat

# source$lng <- scale(source$lng)
# source$lat <- scale(source$lat)


# Plot source

plot_source <- ggplot(data = source,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = factor(category), shape = factor(category)),size=0.9)
plot_source

# plot_source_2 <- ggplot(data = source[source$cat_id == 2,],aes(x=lng,y=lat)) + 
#   geom_point(aes(colour = factor(category), shape = factor(category)),size=0.9)
# plot_source_2



# K-means clustering
# bclust(source[c("lng","lat")],centers=3)$centers
# e1071::cmeans(source[c("lng","lat")],centers=4)


# Functions

# get_unique_cats
get_unique_cats <- function(cats_vec,extra=NULL){
  if(is.null(extra)){
    return(sort(unique(cats_vec)))
  }
  else{
    return(sort(unique(c(cats_vec,extra))))
  }
}

# get_cats_mat
# Return a matrix showing, for each observation in cats_vec,
# which group that observation belongs to.
# 'extra' supplements additional categories that may not have
# already be in cats_vec.

get_cats_mat <- function(cats_vec,extra=NULL){
  
  cats <- get_unique_cats(cats_vec,extra)
  n_cats <- length(cats)
  cats_vec <- as.matrix(cats_vec)
  
  cats_mat <- apply(cats_vec,1,function(e) e == cats)
  cats_mat <- as.matrix(cats_mat)
  
  if(n_cats > 1){
    cats_mat <- t(cats_mat)
  }
  
  return(cats_mat)
  
}


# get_count_mat
# For each observation and category, count the number of neighbours that
# belong to that category (except for that observation)
get_count_mat <- function(cats_vec,extra=NULL){
  cats_mat <- get_cats_mat(cats_vec,extra)
  n_obs <- nrow(cats_mat)
  count_mat <- (1 - diag(n_obs)) %*% cats_mat
  return(count_mat)
}

# get_dist_mat
# Geosphere distances; unit in meters (or a ratio of Earth's radius?)
# need data.frame and correct column names

get_dist_mat <- function(from_points,to_points){
  from_points <- data.matrix(from_points)
  to_points <- data.matrix(to_points)
  return(distm(to_points[,c("lng","lat")],from_points[,c("lng","lat")]))
}

get_nc_mat <- function(from_points,to_points,batch_size=nrow(from_points),radius=1000,tolerance=0.5){
  
  limit <- nrow(from_points)
  lower <- 1
  upper <- min(batch_size,limit)
  r <- -log(1-tolerance)/radius
  exit <- FALSE
  
  # union of categories between the points between from_points to to_points
  cats_mat <- get_cats_mat(to_points$cat_id,extra=from_points$cat_id)
  
  # nc_mat <- matrix(0,nrow=nrow(from_points),ncol=ncol(cats_mat))
  nc_mat <- matrix(0,nrow=0,ncol=ncol(cats_mat))
  
  while(!exit){

    # Calculate distances
    
    dist_mat <- get_dist_mat(from_points[lower:upper,],to_points)
    dist_mat <- exp(-r*dist_mat)
    # dist_mat <- ifelse(is.nan(dist_mat),0,dist_mat)
    dist_mat <- t(dist_mat) %*% cats_mat
    
    # Prevent counting the point itself in
    # nc_mat[lower:upper,] <- dist_mat - get_cats_mat(from_points[lower:upper,]$cat_id,
    # extra=to_points$cat_id)
    
    nc_mat <- rbind(nc_mat, dist_mat - get_cats_mat(from_points[lower:upper,]$cat_id,
                                                    extra=to_points$cat_id))
    
    if(lower + batch_size > limit){
      exit <- TRUE
    }
    else{
      lower <- lower + batch_size
      upper <- min(upper + batch_size,limit)
    }
    
  }
  
  cats <- get_unique_cats(to_points$cat_id,extra=from_points$cat_id)
  colnames(nc_mat) <- paste(c("nc"),rep(sapply(cats,deparse)),sep="_")
  
  return(nc_mat)
  
}


# Test get_nc_mat
# The greater the cutoff, the 

nc_mat_start_time <- Sys.time()
nc_mat <- get_nc_mat(source[,],source,batch_size = 1000,radius = 500,tolerance=0.3)
nc_mat_end_time <- Sys.time()
nc_mat_time_take <- nc_mat_end_time - nc_mat_start_time

summary(nc_mat)
nrow(nc_mat)
dim(nc_mat)
nc_mat_time_take

object_size(nc_mat)
mem_used()


# Percentages

if(ncol(nc_mat) > 1){
  nc_mat_per <- t(apply(nc_mat,1,function(e) e / sum(e)))
  colnames(nc_mat_per) <- paste(c(colnames(nc_mat)),"per",sep = "_")
}

write.table(nc_mat, paste(wd,"/nc_data.csv",sep = ""), sep=",")




# Bind nc_mat to source for analysis
source <- cbind(source,nc_mat)
source <- cbind(source,nc_mat_per)
summary(source)

# Plot neighbourhood characteristics

# rental
plot_nc_1 <- ggplot(data = source,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_1, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black",high = "red")
plot_nc_1

# parking lot
plot_nc_2 <- ggplot(data = source,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_2, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "green")
plot_nc_2

# commercial
plot_nc_3 <- ggplot(data = source,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_3, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "orange")
plot_nc_3

# Percentages
# rental
plot_nc_1_per <- ggplot(data = source,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_1_per, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black",high = "red")
plot_nc_1_per

# parking lot
plot_nc_2_per <- ggplot(data = source,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_2_per, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "green")
plot_nc_2_per

# commercial
plot_nc_3_per <- ggplot(data = source,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_3_per, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "orange")
plot_nc_3_per


# Modelling

# Assign training set (70%) and test set (30%)

set.seed(1)  # Remember to remove this during production.

n <- nrow(source)
train_percent <- 0.3
test_percet = 1 - train_percent
train_size <- ceiling(test_percet * n)
test_size <- n - train_size
train_index <- sample(n, train_size,replace = F)
train <- source[train_index,]
test <- source[-train_index,]


# Binomial logistic regression
lg_fit <- glm(binary ~ lat + lng + nc_1 + nc_2 +  nc_3
              ,data=train,family=binomial(link='logit'))
summary(lg_fit)

# Diagnostics

# McFadden R^2
# Library PSCL required
pR2(lg_fit)

# ANOVA
anova(lg_fit, test="Chisq")

# McFadden R^2
# Library PSCL required
pR2(lg_fit)

# Anova test
anova(lg_fit, test="Chisq")


# Prediction and assessment
# MCE: Misclassification error

prob_test <- predict(lg_fit,test,type="response")
plot(prob_test)
pred_test <- ifelse(prob_test > 0.8,1,0)
mce <-  mean(test$binary != pred_test)
accuracy <-  1 - mce
accuracy

# Plot the ROC curve and find the AUC
# (Area Under the Curve)

roc_pred <- predict(lg_fit, test, type="response")
roc_pred_pos <- prediction(roc_pred, test$binary)
roc_perf <- performance(roc_pred_pos, measure = "tpr", x.measure = "fpr")
plot(roc_perf)

auc <- performance(roc_pred_pos, measure = "auc")
auc <- auc@y.values[[1]]
auc

  
  
