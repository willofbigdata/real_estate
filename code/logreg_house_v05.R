# Set working directory
setwd('E:/BB101/_專題/real_estate/data')

# Load data
source <- read.csv('test_data_03.csv',header=T,na.strings=c(""))

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

plot_source <- ggplot(data = source,aes(x=lng,y=lat)) + 
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
get_unique_cats <- function(cats_vec){
  return(sort(unique(cats_vec)))
}

# get_cats_mat
get_cats_mat <- function(cats_vec){
  
  n_cats <- length(cats_vec)
  cats <- get_unique_cats(cats_vec)
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
get_count_mat <- function(cats_vec){
  cats_mat <- get_cats_mat(cats_vec)
  n_obs <- nrow(cats_mat)
  count_mat <- (1 - diag(n_obs)) %*% cats_mat
  return(count_mat)
}

# get_sum_sqs
get_sum_sqs <- function (cats_vec,x){

  cats_mat <- get_cats_mat(cats_vec)
  x <- as.matrix(x)
  n_row <- nrow(cats_mat)
  n_col <- ncol(cats_mat)
  
  sum_sqs <- matrix(0,nrow=n_row,ncol=n_col)

  vessel <- matrix(1,ncol=n_row) %*% cats_mat
  sum_sqs <- sum_sqs + x^2 %*% vessel
  vessel <- t(x) %*% cats_mat
  sum_sqs <- sum_sqs - (2 * x) %*% vessel
  vessel <- t(x^2) %*% cats_mat
  sum_sqs <- sum_sqs + matrix(1,nrow=n_row) %*% vessel
  
  return(sum_sqs)
  
}

# get_stdev_mat
get_stdev_mat <- function(cats_vec,x){
  return(sqrt(get_sum_sqs(cats_vec,x) / get_count_mat(cats_vec)))
}

# get_adj
# Adjustment factor; change the adjustment function as necessary
get_adj <- function(cats_vec,x,y=NULL,x_scale=1,y_scale=1){
  if(is.null(y)){
    exp_adj <- exp(-x_scale*(get_stdev_mat(cats_vec,x)))
  }
  else{
    exp_adj <- exp( -((x_scale * get_stdev_mat(cats_vec,x)) + 
                      (y_scale * get_stdev_mat(cats_vec,y))))
  }
  return(ifelse(is.nan(exp_adj),0,exp_adj))
}

# get_nc_mat
get_nc_mat <- function(cats_vec,x,y=NULL,x_scale=1,y_scale=1){
  
  count_mat <- get_count_mat(cats_vec)
  nc_mat <- get_adj(cats_vec,x,y,x_scale,y_scale)
  nc_mat <- count_mat * nc_mat
  
  # name the columns
  cats <- get_unique_cats(cats_vec)
  colnames(nc_mat) <- paste(c("nc"),rep(sapply(cats,deparse)),sep="_")
  
  return(nc_mat)
}

# get_nc_perc_mat
# Reweight each row of nc_mat as a percentage of the row total
get_nc_per_mat <- function(cats_vec,x,y=NULL,x_scale=1,y_scale=1){
  nc_per_mat <- get_nc_mat(cats_vec,x,y,x_scale,y_scale)
  
  if(ncol(nc_mat)>1){
    nc_per_mat <- t(apply(nc_mat,1,function(row) row / sum(row)))
  }
 
  # name the columns
  cats <- get_unique_cats(cats_vec)
  colnames(nc_per_mat) <- paste(c("nc_per"),rep(sapply(cats,deparse)),sep="_")
  
  return(nc_per_mat)
   
}



# Test on an actual data set

cats_vec <- source$cat_id
xScale <- 5 * 10^2
yScale <- xScale

# Get neighbourhood characteristics

nc_mat_start_time <- Sys.time()
cats_mat <- get_cats_mat(cats_vec)
count_mat <- get_count_mat(cats_vec)
sum_sqs <- get_sum_sqs(cats_vec,source$lng)
stdev_mat <- get_stdev_mat(cats_vec,source$lng)
nc_mat <- get_nc_mat(cats_vec,source$lng,source$lat,x_scale=xScale,y_scale=yScale)
nc_mat_end_time <- Sys.time()
nc_mat_time_take <- nc_mat_end_time - nc_mat_start_time

summary(nc_mat)
dim(nc_mat)
nc_mat_time_take

object_size(nc_mat)
mem_used()

# Get percentages

start.time <- Sys.time()
nc_per_mat <- get_nc_per_mat(cats_vec,source$lng,source$lat,x_scale=xScale,y_scale=yScale)
end.time <- Sys.time()
time.taken <- end.time - start.time

summary(nc_per_mat)
dim(nc_per_mat)
time.taken

object_size(nc_per_mat)
mem_used()


# Bind nc_mat to source for analysis
source <- cbind(source,nc_mat)
source <- cbind(source,nc_per_mat)
summary(source)

# Plot neighbourhood characteristics

# rental
plot_nc_1 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_1, shape = factor(category)),size=0.9) +
  scale_colour_gradient(low = "black",high = "red")
plot_nc_1

# parking lot
plot_nc_2 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_2, shape = factor(category)),size=0.9) +
  scale_colour_gradient(low = "black", high = "green")
plot_nc_2

# commercial
plot_nc_3 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_3, shape = factor(category)),size=0.9) +
  scale_colour_gradient(low = "black", high = "orange")
plot_nc_3

# Percentages
# rental
plot_nc_per_1 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_per_1, shape = factor(category)),size=0.9) +
  scale_colour_gradient(low = "black",high = "red")
plot_nc_per_1

# parking lot
plot_nc_per_2 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_per_2, shape = factor(category)),size=0.9) +
  scale_colour_gradient(low = "black", high = "green")
plot_nc_per_2

# commercial
plot_nc_per_3 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_per_3, shape = factor(category)),size=0.9) +
  scale_colour_gradient(low = "black", high = "orange")
plot_nc_per_3





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




# Gaussian discriminant analysis
cat_lda <- lda(cat_id ~ .,data=source[,c("cat_id","lat","lng",
                                         "nc_1","nc_2","nc_3")])
cat_lda_vals <- predict(cat_lda,test,type="response")
ldahist(data = cat_lda_vals$x[,2], g=cat_id)    # visualize contributions
cat_lda_pred <- cat_lda_vals$class
cat_lda

mce <-  mean(test$cat_id != cat_lda_pred)
accuracy <-  1 - mce
accuracy


# Multinomial regression

mnlg_fit <- multinom(cat_id ~ lat + lng + nc_1 + nc_2 + nc_3
                     ,data = train)

# z scores
z <-  summary(mnlg_fit)$coefficients /
      summary(mnlg_fit)$standard.errors
print(z)

# 2-tailed z test

p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p)

pR2(mnlg_fit)











# Binomial logistic regression
lg_fit <- glm(binary ~ lat + lng + nc_1 + nc_2 + nc_3
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

  
  
