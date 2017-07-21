# Functions

# Load all dependencies

load_all_deps <- function(){
  library(pryr)       # memory usage
  library(ggplot2)    # visualization
  library(pscl)       # model diagnostics
  library(ROCR)       # classifier performance evaluation
  library(caret)      # training and evaluating models
  library(nnet)       # multinomial logistic regression
  library(GDAtools)   # Gaussian Discriminant Analysis
  library(geosphere)  # geodistances
  library(RMySQL)     # MySQL connection
}


# Multiple classifications (more than 2 classes)

apply_cat_id <- function(category,cat_names,ids){
  
  n_obs <- length(c(category))
  n_names <- length(cat_names)
  n_ids <- length(ids)
  cats_vec <- rep(0,n_obs)
    
  if(min(n_names,n_ids) > 0 && n_names == n_ids){
    for(i in 1:n_names){  
      cats_vec[which(category == cat_names[i])] <- ids[i]
    }
  }
  else{
    print("The number of category names does not match the number of IDs,
          or there are not cat_names or IDs.")
  }

  return(cats_vec)
}


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
# Geosphere distances in meters
# need data.frame and correct column names

get_dist_mat <- function(from_points,to_points){
  from_points <- data.matrix(from_points[,c("lng","lat")])
  to_points <- data.matrix(to_points[,c("lng","lat")])
  return(distm(to_points,from_points))
}





# filter_by_lng_lat
# Given any point and ranges for latitude and longitude
# filter for target points which fall within both ranges

filter_by_lng_lat <- function(from_point,to_points,lng_range=NULL,lat_range=NULL){
  if(!is.null(lng_range)){
    to_points <- subset(to_points,abs(lng - from_point$lng) <= lng_range)
  }
  if(!is.null(lat_range)){
    to_points <- subset(to_points,abs(lat - from_point$lat) <= lat_range)
  }
  # print("=====")
  # print(dim(from_point))
  # print(dim(to_points))
  # return(to_points)
}


# Obtain the matrix of neighbourhood characteristics
# by filtering out latitude and longitude first

get_nc_mat <- function(from_points,to_points,lng_range=NULL,lat_range=NULL,r=1){
  
  n_from_points <- nrow(from_points)
  all_cats <- get_unique_cats(from_points$cat_id,extra=to_points$cat_id)
  n_cats <- length(all_cats)
  
  # create a variable that will store and return the final result
  nc_mat <- matrix(0,nrow=0,ncol=n_cats)
  
  for(i in 1:n_from_points){
    
    # filter out points with the same coordinates as from_point[i]
    to_points_temp <- subset(to_points,lng != from_points$lng[i])
    to_points_temp <- subset(to_points_temp,lat != from_points$lat[i])

    
    
    # filter for points that are within a certain amount of longutude and latitude
    to_points_temp <- filter_by_lng_lat(from_points[i,],to_points_temp,
                                        lng_range,lat_range)
    
    # if(nrow(to_points_temp) == 0){
    #   print(i)
    #   print(dim(to_points_temp))
    # }
    
    # compute neighbourhood characteristics and store the results
    # account for the case where to_points_temp has no row

    if(nrow(to_points_temp) > 0){
     
    #   if(any(from_points$lng[i] == to_points_temp$lng)){
    #     print(paste("same lng at",i))
    #     print(from_points[i,])
    #     print(to_points_temp[which(from_points$lng[i] == to_points_temp$lng),])
    #     print("   ")
    #   }
    # 
    #   if(any(from_points$lat[i] == to_points_temp$lat)){
    #     print(paste("same lat at",i))
    #     print(from_points[i,])
    #     print(to_points_temp[which(from_points$lat[i] == to_points_temp$lat),])
    #     print("   ")
    #   }
      
      if(r == 0){
        dist_mat <- matrix(1,nrow=nrow(to_points_temp),ncol=1)
      }
      else{
        dist_mat <- get_dist_mat(from_points[i,],to_points_temp)
        dist_mat <- exp(-r*dist_mat)
      }

      cats_mat <- get_cats_mat(to_points_temp$cat_id,extra=all_cats)
      nc_mat <- rbind(nc_mat,t(dist_mat) %*% cats_mat)
    }
    else{
      nc_mat <- rbind(nc_mat,rep(0,n_cats))
    }
    
    # progress report    
    if(i %% 1000 == 0){
      print(paste(i,"points done."))
    }
    
  }
  
  # Name the columns
  cats <- get_unique_cats(to_points$cat_id,extra=from_points$cat_id)
  colnames(nc_mat) <- paste(c("nc"),rep(sapply(cats,deparse)),sep="_")
  
  return(nc_mat)
  
}




# multiplot: for visualization
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
