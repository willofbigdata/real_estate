# Set working directory
setwd('E:/BB101/_專題/real_estate/data')


# Download packages if necessary.
# Import the relevant packages.

# install.packages('ggplot2')
library('ggplot2')

# For diagnostics:
# install.packages('pscl')
library(pscl)
# install.packages('ROCR')
library(ROCR)
# install.packages('caret')
library(caret)


# Load data
source <- read.csv('test_data_02.csv',header=T,na.strings=c(""))


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


# Add other columns that might be helpful for making predictions.

# Distance of neighbouring elements

# dists <- dist(matrix(c(x,y),ncol=2), method ="minkowski", p=2)


# data.frame(uniq_cat_id=unique(source$cat_id))

# mean_lng <- mean(source[source$cat_id == c(1,2,3),"lng"])
# mean_lat <- mean(source[source$cat_id == 1,"lat"])
# sd_lng <- sd(source[source$cat_id == 1,"lng"])
# sd_lat <- sd(source[source$cat_id == 1,"lat"])
# 

# The center-and-spread data frame used for model-building
# ***** This approach requires all categories to have at least 2 observations
# since ths approach uses the standard deviation.

# Obtain the mean and stdev values of lat and lng from each category
cns <- data.frame(uniq_cat_id=sort(unique(source$cat_id)),
           mean_lng = tapply(source$lng,source$cat_id,mean),
           mean_lat = tapply(source$lat,source$cat_id,mean),
           sd_lng = tapply(source$lng,source$cat_id,sd),
           sd_lat = tapply(source$lat,source$cat_id,sd)
           )

# Re-express mean and stdev as vectors
# to be fed into tapply below

ll_mean <- c(t(data.matrix(cns[,c('mean_lat','mean_lng')])))
ll_sd <- c(t(data.matrix(cns[,c('sd_lat','sd_lng')])))

# Use tapply to acquire the standardized difference
# ln lat and lng between each point and the center
# of each category

ll_mat <- data.matrix(source[,c('lat','lng')])
ll_cols <- t(apply(ll_mat,1,function(e) ((e - ll_mean)^2)/ll_sd))

# Name the columns of ll_cols

colnames(ll_cols) <- 
  paste(c("lat_std","lng_std"),rep(sapply(cns$uniq_cat_id,deparse),each=2),sep="_")

# Column-bind source with ll_cols

source <- cbind(source,ll_cols)

summary(source)



# Plot 

# plot_source <- ggplot(data = NULL,aes(x=lng,y=lat)) +
#   geom_point(data = source,aes(colour=binary,shape=".")) +
#   scale_colour_gradient(low = "white", high = "black") +
#   theme(panel.background = element_rect(fill = rgb(0.7,0.7,0.8)))
# 
# plot_source

plot_source_2 <- ggplot(data = source,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = factor(category), shape = factor(category)),size=0.8) +
  geom_point(data = cns, aes(x=mean_lng,y=mean_lat))

plot_source_2

plot(source$lat_std_2,source$lat_std_3)



# plot_source <- plot_source +geom_point(data = predict_on_train,aes(colour=prob_one,shape="."),alpha=1)




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

# Fit
# lg_fit <- glm(binary ~ lat + lng,data=train,family=binomial(link='logit'))
lg_fit <- glm(binary ~ lat_std_1 + lng_std_1 
              + lat_std_2 + lng_std_2
              ,data=train,family=binomial(link='logit'))
summary(lg_fit)




# 
# 
# Formula <- "binary ~ lat + lng"
# Method <- "qda"
# TrControl <- trainControl(method = "repeatedcv",
#                           number = V,
#                           repeats = T)
# Model <- train(as.formula(Formula), data = source, method = Method, trControl = TrControl)







# McFadden R^2
# Library PSCL required
pR2(lg_fit)

# Diagnostics

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

  
  
