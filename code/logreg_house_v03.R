# Set working directory
setwd('E:/BB101/_專題/real_estate/data')

# Load data
source <- read.csv('test_data_03.csv',header=T,na.strings=c(""))

# Shuffle up the data order for a more representative outcome
set.seed(1)
source <- source[sample(nrow(df1)),]


# Download packages if necessary.
# Import the relevant packages.

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

# Plot source

plot_source <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = factor(category), shape = factor(category)),size=0.8)
plot_source


# K-means clustering
# bclust(source[c("lng","lat")],centers=3)$centers
# e1071::cmeans(source[c("lng","lat")],centers=4)


# Functions

# get_dists_vec

get_dist_vec <- function(subset, all){
  subset <- data.matrix(subset)
  all <- data.matrix(all)
    return(distm(all[,c("lng","lat")],subset[i,c("lng","lat")]))
}

get_dist_vec <- function(subset, all){
  subset <- data.matrix(subset)
  all <- data.matrix(all)
  return(distm(all[,c("lng","lat")],subset[i,c("lng","lat")]))
}


# distm(source[,c("lng","lat")],source[1:2,c("lng","lat")])

# test <- get_dist_vec(1,source[,c("lat","lng")])

# get_each_nc
# Calculate the neighbourhood components for observation i

get_each_nc <- function(i,source,cat_ids,cats_unique){
  
  # Identify the categories

  n_cats <- length(cats_unique)
  cats_matrix <- matrix(rep(cat_ids,n_cats),ncol=n_cats)
  cats_matrix <- apply(cats_matrix,1,function(e) e == cats_unique)
  cats_matrix <- matrix(cats_matrix,ncol=n_cats,byrow=TRUE)
  cats_matrix[i,] <- rep(FALSE,n_cats)    # Exclude self

  # Transformation

  dist_vec <- get_dist_vec(i,source[,c("lat","lng")])
  dist_vec <- dist_vec / sd(dist_vec)    # This also helps with cancelling the distance unit
  dist_vec <- exp(-dist_vec)

  # Turn all possible NaN values to 0
  # This should be done if there are values (eg. average) that cannot be computed
  # when no neighbour belongs to a certain category
  
  dist_vec <- ifelse(is.nan(dist_vec),0,dist_vec)
  
  # Dimension check
  
  
  dist_vec <- t(dist_vec)
  
  if(ncol(dist_vec)!=nrow(cats_matrix)){
    dist_vec <- t(dist_vec)
  }

  return(dist_vec %*% cats_matrix)
  
}

# Neighbourhood component matrix

# Inpuut: A data.frame cll which contains cat_id, 
# lat and lng columns for all observations
# Output: neighbourhood component matrix for each category

get_nc_matrix <- function(source){
  
  begin_time <- Sys.time()
  
  n_obs <- nrow(source)
  cat_ids <- source$cat_id
  cats_unique <- sort(unique(cat_ids))
  n_cats <- length(cats_unique)
  nc_matrix <- matrix(NA,nrow=n_obs,ncol=n_cats)

  for(i in 1:n_obs){
    nc_matrix[i,] <- get_each_nc(i,source,cat_ids,cats_unique)
    
    if(i %% 100 == 0){
      print(paste(i," mark"))
      print(paste("Time used: ",Sys.time() - begin_time))
    }
    
  }

  return(nc_matrix)
}

# Test out the function

start.time <- Sys.time()
nc_matrix <- get_nc_matrix(source)
end.time <- Sys.time()
time.taken <- end.time - start.time

summary(nc_matrix)
time.taken



















source <- cbind(source,nc_matrix)

# Plot model outputs

plot_nc_1 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_1, shape = factor(category)),size=0.8) +
  scale_colour_gradient(low = "black",high = "blue")
plot_nc_1

plot_nc_2 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_2, shape = factor(category)),size=0.8) +
  scale_colour_gradient(low = "black", high = "green")
plot_nc_2

plot_nc_3 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_3, shape = factor(category)),size=0.8) +
  scale_colour_gradient(low = "black", high = "red")
plot_nc_3


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

cat_lda <- lda(cat_id ~ .,data=source[,c("cat_id","lat","lng","nc_1","nc_3")])
cat_lda_vals <- predict(cat_lda,test,type="response")
ldahist(data = cat_lda_vals$x[,2], g=cat_id)    # visualize contributions
cat_lda_pred <- cat_lda_vals$class
cat_lda

mce <-  mean(test$cat_id != cat_lda_pred)
accuracy <-  1 - mce
accuracy



# Binomial logistic regression

lg_fit <- glm(binary ~ lat + lng + nc_1 + nc_3
              ,data=train,family=binomial(link='logit'))
summary(lg_fit)
pR2(lg_fit)


# Multinomial regression

mnlg_fit <- multinom(cat_id ~ lat + lng + nc_2 + nc_3
                     ,data = train)

# z scores

z <-  summary(mnlg_fit)$coefficients /
      summary(mnlg_fit)$standard.errors
print(z)

# 2-tailed z test

p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p)

pR2(mnlg_fit)

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

  
  
