library(data.table)
library(dplyr)
library(caret)
library(RevoUtilsMath)
setMKLthreads(4)
getMKLthreads()
memory.size(max=32710)
memory.limit()
#Sys.setlocale("LC_ALL","Chinese")
Sys.setlocale(category="LC_CTYPE", locale = "cht") 
setwd("E:\\Desk\\r_test")
#-------------------------------------------------------------------------------------------------------------------------------------------

library(magrittr)
library(data.table)
library(rvest) 
library(jsonlite)

data <- fread("E:\\Desk\\r_test\\data\\tpe_temple.csv", header=TRUE, sep=",") %>% as.data.table()
data

#address convert to latlng
getLocation = function(addr){
  library(magrittr)
  library(jsonlite)
  library(data.table)
  addr %<>% as.character
  url = paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",addr) 
  res = fromJSON(URLencode(url), flatten = TRUE) 
  lat = res$results$geometry.location.lat
  lng = res$results$geometry.location.lng
  result = data.table("addr" = addr,
                      "lat" = lat,
                      "lng" = lng)
  return(result)  
}

table = lapply(data$addr, getLocation) %>% do.call(rbind, .) 

data = merge(data, table, by = "addr", all = FALSE) %>% 
  .[,.("name" = name,
       "deities" = deities,
       "addr" = addr, 
       "lat" = lat, 
       "lng" = lng)] %>% 
  .[!duplicated(., by=c("addr"))]

