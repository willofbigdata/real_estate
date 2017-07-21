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
dataset_envir <- dbGetQuery(conn,
                            " SELECT ID, Area, label, lng, lat
                            FROM envir 
                            WHERE 
                            City = \"Taipei City\"
                            AND lng BETWEEN 121.45 AND 121.7 
                            AND lat BETWEEN 24.9 AND 25.2
                            ;")
dataset_rental <- dbGetQuery(conn,
                             "SELECT url, Area,label, lng, lat
                             FROM rental
                             WHERE 
                             lng BETWEEN 121.45 AND 121.7
                             AND lat BETWEEN 24.9 AND 25.2
                             ;")

colnames(dataset_rental)[1] <- "ID"
dataset <- rbind(dataset_rental,dataset_envir)


# longitude conversion for computation
dataset$lng_original <- dataset$lng
dataset$lat_original <- dataset$lat
dataset$lng <- dataset$lng - 90


# Add all necessary columns
bi_labels <- c("store")
bi_IDs <- c(1)
multi_labels <- catudf$catID
multi_IDs <- 1:length(catudf$catID)
binary <- apply_cat_id(dataset$label,bi_labels,bi_IDs)
cat_id <- apply_cat_id(dataset$label,multi_labels,multi_IDs)
dataset <- cbind(dataset,binary,cat_id)


# Plot dataset
plot_dataset <- ggplot(data = dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = factor(label), shape = factor(label)),size=0.5) +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
plot_dataset



# dataset the functions for computation
code_dir <- 'E:/BB101/_專題/real_estate/code/nc_matrix.R'
source(code_dir)

# Reduce the dataset a bit via a random sample
# set.seed(1)
# sample_dataset <- dataset[sample(1:nrow(dataset),size=100),]

# Getting serious
sample_dataset <- dataset

# Test get_nc_mat
nc_mat_start_time <- Sys.time()
# diffs_lng <- diff(sort(unique(sample_dataset$lng)))
# diffs_lat <- diff(sort(unique(sample_dataset$lat)))
# lng_delta <- median(diffs)    # adjustable
# lat_delta <- median(diffs)    # adjustable
nc_mat <- get_nc_mat(sample_dataset,sample_dataset,
                     lng_range=10^(-7),lat_range=10^(-7),r=0)
nc_mat_end_time <- Sys.time()
nc_mat_time_take <- nc_mat_end_time - nc_mat_start_time

summary(nc_mat)
dim(nc_mat)
nc_mat_time_take

object_size(nc_mat)
mem_used()









# Percentages
if(ncol(nc_mat) > 1){
  nc_mat_per <- (apply(nc_mat,2,function(e) e / sum(e)))
  colnames(nc_mat_per) <- paste(c(colnames(nc_mat)),"per",sep = "_")
}


# Bind nc_mat to dataset for analysis
sample_dataset <- cbind(sample_dataset,nc_mat)
sample_dataset <- cbind(sample_dataset,nc_mat_per)
summary(sample_dataset)


# Plot sample dataset
plot_sample <- ggplot(data = sample_dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = factor(cat_id),shape = factor(cat_id)),size=0.5) +
  scale_shape_manual(values=seq(0,length(unique(sample_dataset$label))))
# plot_sample

# Plot neighbourhood characteristics
plot_nc <- ggplot(data = sample_dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_9, shape = factor(cat_id)),size=0.5) +
  scale_colour_gradient(low = "white",high = "red") +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
# plot_nc

# Plot neighbourhood characteristics percentages
plot_nc_per <- ggplot(data = sample_dataset,aes(x=lng_original,y=lat_original)) + 
  geom_point(aes(colour = nc_9_per, shape = factor(cat_id)),size=0.5) +
  scale_colour_gradient(low = "white",high = "red") +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
# plot_nc_per

# Juxtapose all lng-lat plots
multiplot(plot_sample, plot_nc, plot_nc_per, cols=2)



# Modelling

# Assign training set (70%) and test set (30%)

set.seed(1)  # Remember to remove this during production.

n <- nrow(sample_dataset)
train_percent <- 0.3
test_percet = 1 - train_percent
train_size <- ceiling(test_percet * n)
test_size <- n - train_size
train_index <- sample(n, train_size,replace = F)
train <- sample_dataset[train_index,]
test <- sample_dataset[-train_index,]


# Binomial logistic regression
lg_fit <- glm(binary ~   nc_2 + nc_3 + nc_4 + nc_5 + nc_6 +
                         nc_7 + nc_8 + nc_9 + nc_10
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










# Write tables
# dbWriteTable(conn, name='table_name', value=data.frame.name)

# Disconnect
dbDisconnect(conn)

