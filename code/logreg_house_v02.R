# Set working directory
setwd('E:/BB101/_專題/real_estate/data')

# Load data
source <- read.csv('test_data_03.csv',header=T,na.strings=c(""))

# Download packages if necessary.
# Import the relevant packages.

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




# Function
# Neighbourhood component

# Inpuut: A data.frame cll which contains cat_id, 
# lat and lng columns for all observations
# Output: neighbourhood component matrix for each category

get_nc_matrix <- function(cll){
  
  # Construct the building blocks
  
  nObs <- nrow(cll)
  cat_unique <- sort(unique(cll[,c("cat_id")]))
  n_cats <- length(cat_unique)
  dists <- dist(cll[,c("lat","lng")])
  dists <- as.matrix(dists)
  
  # Transform so that the measure is in [0,1]
  # and increases as neighbours get closer.
  # Also scale the distances at each points 
  # to reflect the density of its neighbourhood
  
  # dists <- t(apply(dists,1,function(e) e / sd(e)))
  dists <- exp(-400*dists)
  
  # Find the TRUE-FALSE matrix which expresses whether each observation
  # belong to each category or not.
  # row: observation
  # col: category
  
  cats_matrix <- matrix(rep(cll[,c("cat_id")],n_cats),ncol=n_cats)
  cats_matrix <- t(apply(cats_matrix,1,function(e) e == cat_unique))

  # For each column, count how many (except self) is TRUE
  
  anti_iden <- 1 - diag(nObs)
  count_matrix <- anti_iden %*% cats_matrix
  
  # For each observation, compute the average distance of locations
  # which belong to each class (not counting the observation itself in)
  
  # nc_matrix <- (dists %*% cats_matrix) / count_matrix
  nc_matrix <- (dists %*% cats_matrix)

  # In case no neighbour belongs to a certain category, assign 0 to
  # its neighbourhood composition value for that category.
  
  nc_matrix <- ifelse(is.nan(nc_matrix),0,nc_matrix)
  
  # Scale each row to compute percentages
  
  nc_row_sums <- nc_matrix %*% rep(1,n_cats)
  nc_row_sums <- matrix(nc_row_sums,nrow=nObs,ncol=n_cats)
  nc_matrix <- nc_matrix / nc_row_sums

  # Name the columns
  
  colnames(nc_matrix) <- 
    paste(c("nc"),rep(sapply(cat_unique,deparse)),sep="_")
  
  # Return the results
  
  return(nc_matrix)

}


# Compute the neighbourhood characteristics 
# on the entire data set!!!!!!

start.time <- Sys.time()
nc_matrix <- get_nc_matrix(source[,c("cat_id","lat","lng")])
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

cat_lda <- lda(cat_id ~ .,data=source[,c("cat_id","lat","lng")])
cat_lda_vals <- predict(cat_lda,test,type="response")
ldahist(data = cat_lda_vals$x[,2], g=cat_id)    # visualize contributions
cat_lda_pred <- cat_lda_vals$class

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

  
  
