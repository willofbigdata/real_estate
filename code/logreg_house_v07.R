# source the functions for computation
code_dir <- 'E:/BB101/_專題/real_estate/code/nc_matrix.R'
source(code_dir)

# Set data directory
data_dir <- 'E:/BB101/_專題/real_estate/data'
setwd(data_dir)

# Load data
dataset <- read.csv('test_data_03.csv',header=T,na.strings=c(""))

# longitude conversion for computation
dataset$lng_original <- dataset$lng
dataset$lat_original <- dataset$lat
dataset$lng <- dataset$lng - 90


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


# Add a new column for binary classification

bi_labels <- c("rental")
bi_IDs <- c(1)
multi_labels <- c("commercial","parking_lot","rental")
multi_IDs <- c(1,2,3)
binary <- apply_cat_id(dataset$category,bi_labels,bi_IDs)
cat_id <- apply_cat_id(dataset$category,multi_labels,multi_IDs)
dataset <- cbind(dataset,binary,cat_id)


# Plot the dataset

plot_dataset <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = factor(category), shape = factor(category)),size=0.9)
plot_dataset




# K-means clustering
# bclust(dataset[c("lng","lat")],centers=3)$centers
# e1071::cmeans(dataset[c("lng","lat")],centers=4)


# Test get_nc_mat
# The greater the cutoff, the 

nc_mat_start_time <- Sys.time()
nc_mat <- get_nc_mat(dataset[,],dataset,batch_size = 1000,radius = 300,tolerance=0.2)
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


# Bind nc_mat to dataset for analysis
dataset <- cbind(dataset,nc_mat)
dataset <- cbind(dataset,nc_mat_per)
summary(dataset)

# Plot neighbourhood characteristics

# rental
plot_nc_1 <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_1, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black",high = "red")
plot_nc_1

# parking lot
plot_nc_2 <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_2, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "green")
plot_nc_2

# commercial
plot_nc_3 <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_3, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "orange")
plot_nc_3

# Percentages
# rental
plot_nc_1_per <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_1_per, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black",high = "red")
plot_nc_1_per

# parking lot
plot_nc_2_per <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_2_per, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "green")
plot_nc_2_per

# commercial
plot_nc_3_per <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_3_per, shape = factor(cat_id)),size=0.9) +
  scale_colour_gradient(low = "black", high = "orange")
plot_nc_3_per


# Modelling

# Assign training set (70%) and test set (30%)

set.seed(1)  # Remember to remove this during production.

n <- nrow(dataset)
train_percent <- 0.3
test_percet = 1 - train_percent
train_size <- ceiling(test_percet * n)
test_size <- n - train_size
train_index <- sample(n, train_size,replace = F)
train <- dataset[train_index,]
test <- dataset[-train_index,]


# Binomial logistic regression
lg_fit <- glm(binary ~ lat + lng + nc_1_per +  nc_3_per
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

  
  
