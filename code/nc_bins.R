# get_nc_frame
# Given a properly-transformed dataset,
# return a data.frame object nc_frame that partitions the lng x lat space
# into tiles and counts the number of instances from each category.

get_nc_list <- function(dataset,n_bin_lng=20,n_bin_lat=20,percent=FALSE){

  # manually construct breaks for the histogram
  lng_range <- range(dataset$lng)
  lng_gap <- diff(range(dataset$lng)) / (n_bin_lng - 1)
  lng_breaks <- lng_range[1] + lng_gap * (c(0:n_bin_lng) - 0.5)
  lat_range <- range(dataset$lat)
  lat_gap <- diff(range(dataset$lat)) / (n_bin_lat - 1)
  lat_breaks <- lat_range[1] + lat_gap * (c(0:n_bin_lat) - 0.5)
  lng_bins <- hist(dataset$lng,breaks=lng_breaks,plot=FALSE)
  lat_bins <- hist(dataset$lat,breaks=lat_breaks,plot=FALSE)

  # Place all observations into the bins
  # and translate the lng lat bin pair to integer IDs
  lng_bin_ID <- cut(dataset$lng,breaks=lng_breaks,labels=FALSE)
  lat_bin_ID <- cut(dataset$lat,breaks=lat_breaks,labels=FALSE)
  lng_lat_bin_ID <- matrix(c(lng_bin_ID,lat_bin_ID),ncol=2)
  bin_ID <- apply(lng_lat_bin_ID,1,function(row) row[1]+(n_bin_lat)*(row[2]-1))
  
  # Map the lng and lat bin IDs to an overall ID
  lng_bins_rep <- rep(1:n_bin_lng,n_bin_lat)
  lat_bins_rep <- rep(1:n_bin_lat,each=n_bin_lng)
  bin_map <- matrix(c(1:(n_bin_lng * n_bin_lat),
                      lng_bins_rep,lat_bins_rep),ncol=3)
  
  # midpoints of the bin to which each point belongs
  lng_mids <- rep(lng_bins$mids,n_bin_lat)        # midpoints
  lat_mids <- rep(lat_bins$mids,each=n_bin_lng)   # midpoints
  lng_bin_mid <- sapply(lng_bin_ID,function(e) lng_mids[e])
  lat_bin_mid <- sapply(lat_bin_ID,function(e) lat_mids[e])

  dataset <- cbind(dataset,bin_ID,lng_bin_mid,lat_bin_mid)
  
  # Start counting for each bin
  # supplement bins which do not contain any observation
  
  empty_bins <- 1:(n_bin_lng * n_bin_lat)
  empty_bins <- setdiff(empty_bins,unique(bin_ID))
  empty_NA <- rep(NA,length(empty_bins))
  counts <- table(c(dataset$bin_ID,empty_bins),
                  c(dataset$cat_id,empty_NA),
                  useNA="no")
  nc_frame <- as.data.frame.matrix(counts)
  
  # Return each category's percentage relative to the
  # overall count iff percent=TRUE
  
  if(percent == TRUE){
    nc_frame <- apply(nc_frame,2,
                      function(column) column / sum(column))
    nc_frame <- as.data.frame(nc_frame)
  }
  
  
  # Complete nc_frame
  # Attach data on bin assignment, coordinates of bin centers.
  # Name the columns for some finishing touch.
  
  nc_frame <- cbind(bin_map,lng_mids,lat_mids,nc_frame)
  colnames(nc_frame) <- c("bin_ID","lng_bin_ID","lat_bin_ID",
                          "lng_bin_mid","lat_bin_mid",
                          paste("NC",multi_labels,sep = "_"))
  
  # Return a list of relevant info.
  # nc_list <- list(dataset=dataset,
  #                 nc_frame=nc_frame,
  #                 bin_map=bin_map,
  #                 lng_bins_mids=lng_bins$mids,
  #                 lat_bins_mids=lat_bins$mids)
  
  nc_list <- list(nc_frame = nc_frame,
                  bin_map = bin_map,
                  lng_breaks = lng_breaks,
                  lat_breaks = lat_breaks,
                  lng_bins_mids = lng_bins$mids,
                  lat_bins_mids = lat_bins$mids)
  
  return(nc_list)
  
}