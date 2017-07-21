library(spacetime)
library(plotGoogleMaps)
library(rgdal)
library(sp)


NCU <- data.frame(lon = c(121.1946, 121.240952), lat = c(24.968972, 24.958232))
coordinates(NCU) = ~lon+lat
proj4string(NCU) <- CRS('+proj=longlat')
plotGoogleMaps(NCU, mapTypeId='ROADMAP',filename='myMap1.htm')


