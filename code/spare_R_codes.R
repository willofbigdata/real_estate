
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





# Add other columns that might be helpful for making predictions.

# Distance of neighbouring elements

# dists <- dist(matrix(c(x,y),ncol=2), method ="minkowski", p=2)


# data.frame(uniq_cat_id=unique(source$cat_id))




# # The neighbourhood characteristic approach
# # For each observation p and category k
# # compute the k-t neighbouring characteristics value nc(i)
# 
# set.seed(1)
# dists_2 <- dist(matrix(1:10,ncol=2))
# dists_2 <- as.matrix(dists_2)
# cat_vec <- sample(1:5,size=5,replace=TRUE)
# cats <- sort(unique(cat_vec))
# 
# cat_vec_true <- cat_vec == 1
# 
# # Find the zero-one matrix which expresses whether each observation
# # belong to each category or not.
# # row: observation
# # col: category
# 
# n_cats <- length(cats)
# cats_matrix <- matrix(rep(cat_vec,n_cats),ncol=n_cats)
# cats_matrix <- t(apply(cats_matrix,1,function(e) e == cats))
# 
# # For each column, count how many (except self) is TRUE
# count_others <- function(cats_matrix){
#   nrows <- nrow(cats_matrix)
#   anti_iden <- 1 - diag(nrows)
#   return(anti_iden %*% cats_matrix)
# }
# 
# count_matrix <- count_others(cats_matrix)
# 
# c_matrix <- (dists_2 %*% cats_matrix) / count_matrix
# c_matrix <- exp(-c_matrix)
# c_matrix <- ifelse(is.nan(c_matrix),0,c_matrix)









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













# generate sample
sample_size <- 1000
weight <- 0.2


sample <- ifelse(runif(sample_size,0,1)<weight,1,2)
sample_1_count <- sum(sample==1)
sample_2_count <- sum(sample==2)
sample[which(sample==1)] <- rnorm(sample_1_count,mean=2,1)
sample[which(sample==2)] <- rnorm(sample_2_count,mean=5,1)
hist(sample,breaks=100)

# sample <- 0.5 * rnorm(sample_size,mean=-5000,0.00001) + 0.5 * rnorm(sample_size,5000,0.00005)
hist(sample,breaks=100)

# import
library(mixtools)
# wait = faithful$waiting
# mixmdl = normalmixEM(wait)
# plot(mixmdl,which=2)
# lines(density(wait), lty=2, lwd=2)

# test on simulated data set
mix_own <- normalmixEM(sample)
plot(mix_own,which=2)
lines(density(sample), lty=2, lwd=2)
summary(mix_own)





# Test out multivariate normal distributions
# Try 2-dimensional distributions
library(mixtools)
library(MASS)
library(ggplot2)
# generate sample
sample_size <- 1000
weight <- 0.6

# Define the distribution parameters
# each sigma must be a positive-definite symmetric matrix
mu_1 <- c(0,0)
sigma_1 <- matrix(c(1,0.2,0.2,1),nrow=2)
mu_2 <- c(2,2)
sigma_2 <- matrix(c(1,-0.3,-0.3,1)/2,nrow=2)

draw <- ifelse(runif(sample_size,0,1)<weight,1,2)
sample <- matrix(draw,nrow=sample_size,ncol=3)
sample_1_count <- sum(draw == 1)
sample_2_count <- sum(draw == 2)
sample[which(sample[,1] == 1),c(2,3)] <- mvrnorm(sample_1_count,mu_1,sigma_1)
sample[which(sample[,1] == 2),c(2,3)] <- mvrnorm(sample_2_count,mu_2,sigma_2)
sample <- as.data.frame(sample)

# Perform the fit
mv_result <- mvnormalmixEM(sample[,c(2,3)])
summary(mv_result)

geom_raster(aes(fill = density))

plot_gen <- ggplot(data = sample,aes(x=V2,y=V3)) +
  geom_point(aes(colour = V1,size=0.5))
plot_gen


mv_result_pos <- mv_result$posterior
mv_pos_den <- dmvnorm(mv_result$posterior,
                      mv_result$mu[[1]],
                      mv_result$sigma[[1]])
mv_result_pos <- cbind(mv_result_pos,mv_pos_den)
mv_result_pos <- data.frame(mv_result_pos)

# plot_gen_2 <- ggplot(data = mv_result_pos,aes(x=comp.1,y=comp.2,z=density)) +
#   geom_contour()
# plot_gen_2

plot(mv_result,which=2,alpha=c(0:50)/50)
lines(density(sample), lty=2, lwd=2)
# ellipse(mv_result$mu[[1]],mv_result$sigma[[1]],alpha=c(0.01,0.02),
#         newplot = FALSE,type="l",lwd=0.01)







height <- n_bin_lat
width <- n_bin_lng
n_cat <- nc_list$n_cat
lng_gap <- nc_list$gap[1]
lat_gap <- nc_list$gap[2]

# Distance discount rate
r_lng <- 10^3
r_lat <- 10^3

# exclude self?
exclude_self <- TRUE

# Prepare the batches used to calculate the
# number of blocks relative to each lng and lat bin.

lng_batch <- block_lng_batch(n_bin_lat,n_bin_lng)
lat_batch <- block_lng_batch(n_bin_lat,n_bin_lng)

if(exclude_self){
  self_lng <- ifelse(lng_batch == 0,1,0)
  self_lat <- ifelse(lat_batch == 0,1,0)
}

# Transform to compute hrizontal and vertical distances.

lng_batch <- lng_gap * lng_batch
lat_batch <- lat_gap * lat_batch

# Scale using the natural exponent.
# Subtract self from the calculation if required.

lng_batch <- exp(-r_lng * lng_batch)
lat_batch <- exp(-r_lat * lat_batch)

if(exclude_self){
  lng_batch <- lng_batch - self_lng
  lat_batch <- lat_batch - self_lat
}

# Obtain adjusted neighbourhood characteristics
# for lng and lat.

nc_frame <- nc_list$nc_frame[,(1:n_cat+5)]
nc_frame <- as.matrix(nc_frame)
nc_lng_adj <- lng_batch %*% nc_frame
nc_lat_adj <- lat_batch %*% nc_frame

# Compute the adjusted nc as a weighted average 
# of nc_lng_adj and nc_lat_adj.

weight_lng <- 0.5
weight_lat <- 1 - weight_lng

nc_adj <-  apply((nc_list$bin_map),1,function(row) 
  weight_lng * nc_lng_adj[row[2],] +
    weight_lat * nc_lat_adj[row[3],])
nc_adj <- t(nc_adj)
nc_adj <- as.data.frame(nc_adj)
nc_adj <- cbind(nc_list$bin_map,nc_adj)









# Plot neighbourhood characteristics
plot_nc <- ggplot(data = nc_adj,
                  aes(x=lng_bin_ID,y=lat_bin_ID)) + 
  geom_tile(aes(fill = V2),colour = "white") + 
  scale_fill_gradient(low = "white",high = "steelblue")
plot_nc




# Plot adjusted neighbourhood characteristics
plot_nc <- ggplot(data = nc_sample,aes(x=n_bin_lng,
                                       y=n_bin_lat)) + 
  geom_point(aes(colour = V1, shape = factor(cat_id)),size=0.5) +
  scale_colour_gradient(low = "white",high = "red") +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
# plot_nc

# Plot neighbourhood characteristics percentages
plot_nc_per <- ggplot(data = sample_dataset,aes(x=lng,y=lat)) + 
  geom_point(aes(colour = nc_9_per, shape = factor(cat_id)),size=0.5) +
  scale_colour_gradient(low = "white",high = "red") +
  scale_shape_manual(values=seq(0,length(unique(dataset$label))))
# plot_nc_per