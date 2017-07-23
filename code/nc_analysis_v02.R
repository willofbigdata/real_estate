# prepare the functions for computation
code_dir <- 'C:/Users/Will/Documents/資策會/_專題/real_estate/code/'
source(paste(code_dir,"univ_fn.R",sep = ""))
source(paste(code_dir,"nc_bins.R",sep = ""))

# Download and install dependencies
# install.packages("pryr")           # memory usage
# install.packages("ggplot2")        # visualization
# install.packages("pscl")           # model diagnostics
# install.packages("ROCR")           # classifier performance evaluation
# install.packages("caret")          # training and evaluating models
# install.packages("nnet")           # multinomial logistic regression
# install.packages("GDAtools")       # Gaussian Discriminant Analysis
# install.packages("geosphere")      # geodistances
# install.packages("RMySQL")         # MySQL connection
# install.packages("RevoUtilsMath")  # multi-threading
# install.packages("ash")

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
library(ash)

# multi-threading
setMKLthreads(4)
getMKLthreads()
memory.size(max=32710)
memory.limit()


# Connect to MySQL
# db_name <- 'house'
# conn <- dbConnect(MySQL(), user='', password='',
#                  dbname=db_name, host='')

# Encoding
# enc_query <- "SET NAMES udf8;"
# enc_query <- "SET NAMES big5;"
# dbSendQuery(conn,enc_query)

# Table queries
# dataset_envir <- dbGetQuery(conn,
#                             " SELECT ID, Area, label, lng, lat
#                             FROM envir
#                             WHERE
#                             (City = \"Taipei City\"
#                             AND lng BETWEEN 121.45 AND 121.7
#                             AND lat BETWEEN 24.9 AND 25.2)
#                             OR
#                             (label = \"mrt\")
#                             ;")
# dataset_rental <- dbGetQuery(conn,
#                              "SELECT url, Area,label, lng, lat
#                              FROM rental
#                              WHERE
#                              lng BETWEEN 121.45 AND 121.7
#                              AND lat BETWEEN 24.9 AND 25.2
#                              ;")

# colnames(dataset_rental)[1] <- "ID"
# dataset <- rbind(dataset_rental,dataset_envir)

# longitude conversion for computation
# dataset$lng_mod <- dataset$lng - 90
# dataset$lat_mod <- dataset$lat

# Add all necessary columns
# bi_labels <- c("store")
# bi_IDs <- c(1)
# multi_labels <- get_unique_cats(dataset$label)
# multi_IDs <- 1:length(multi_labels)
# binary <- apply_cat_id(dataset$label,bi_labels,bi_IDs)
# cat_id <- apply_cat_id(dataset$label,multi_labels,multi_IDs)
# dataset <- cbind(dataset,binary,cat_id)

# Load data from localhost if applicable.
setwd("C:/Users/Will/Documents/資策會/_專題/real_estate/data")
dataset <- read.csv(file="nc_dataset.csv",header=TRUE,
                    stringsAsFactors = FALSE)

summary(dataset)

# write.csv(dataset,file="nc_dataset.csv",row.names=FALSE)


# Plot dataset
plot_dataset <- ggplot(data = dataset,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = factor(label), shape = factor(label)),size=0.5) +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
plot_dataset

plot_subset <- ggplot(data = subset(dataset,label=="green"),aes(x=lng,y=lat)) + 
  geom_point(aes(colour = factor(label), shape = factor(label)),size=0.5) +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
plot_subset



##########
# Part 2: generate the neighbourhood characteristics data.frame
setwd("C:/Users/Will/Documents/資策會/_專題/real_estate/code")
source("nc_bins.R")

# specify the level of refinement.

n_bin_lng <- 100
n_bin_lat <- n_bin_lng

nc_list_start_time <- Sys.time()
nc_list <- get_nc_list(dataset,n_bin_lng,n_bin_lat,percent=FALSE)
nc_list_end_time <- Sys.time()
nc_list_time_taken <- nc_list_end_time - nc_list_start_time

summary(nc_list)
nc_list_time_taken
object_size(nc_list)
mem_used()


# For the j-th lng_bin and i-th lat_bin and given integer n,
# generate vectors that represent the number of
# bins away from bin (i,j).

nc_adj_start_time <- Sys.time()
nc_adj <- get_nc_adj(nc_list,n_bin_lng,n_bin_lat,
                     r_lng=10^3,r_lat=10^3,
                     exclude_self=TRUE, weight=0.5)
nc_adj_end_time <- Sys.time()
nc_adj_time_taken <- nc_adj_end_time - nc_adj_start_time

summary(nc_adj)
nc_adj_time_taken
object_size(nc_adj)
mem_used()


# Plot neighbourhood characteristics
plot_nc_frame <- ggplot(data = nc_list$nc_frame,
                        aes(x=lng_bin_ID,y=lat_bin_ID)) + 
  geom_tile(aes(fill = NC_green),colour = "white") + 
  scale_fill_gradient(low = "white",high = "steelblue")
# plot_nc_frame

# Plot adjusted neighbourhood characteristics
plot_nc_adj <- ggplot(data = nc_adj,
                        aes(x=lng_bin_ID,y=lat_bin_ID)) + 
  geom_tile(aes(fill = NC_green),colour = "white") + 
  scale_fill_gradient(low = "white",high = "steelblue")
# plot_nc_adj

# Juxtapose all lng-lat plots
multiplot(plot_nc_frame, plot_nc_adj, cols=2)



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

