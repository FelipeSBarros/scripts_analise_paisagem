################################################################
##Script for sampling hexagons in a polygon shapefile
##Author: Felipe S Barros
###############################################################

############### LOAD FUNCTIONS AND LIBRARIES BEGING ###############
#Loading library
library(sp)
library(rgdal)

#defining where the shapefile is
dsn_oldgrowth <- '/input')
#importing the shapefile
shape <- readOGR(dsn=dsn, layer=shapefile_name) #change 'shapefile_name'
#confirming the class of imported shape
class(shape)
#plotting the polygon
plot(shape, axes=TRUE)

#Creating the hexagon sampling, in point format, using spsample (package 'sp') to be converted in polygon
shape_pts <- spsample(shape, n = 1000, "hexagonal")
plot(shape_pts)
#for more information:
?spsample

#converting the hexagons created to polygons
HexPols <- HexPoints2SpatialPolygons(shape_pts)
plot(HexPols)
