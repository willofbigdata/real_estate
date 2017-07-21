# This is a collection of universal functions
# that can be applied to most of the project-related codes.

# apply_cat_id
# Transform labels into distinct integers

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
# Obtain a sorted vector of unique elements from a vector.
# Particularly applicable to a vector of categories.

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
