# build (rapidly) two neighbor objects for use in testing the custom union function


# load the libraries
library(spdep)           # builds neighbors, and much, much more
library(sf)              # reads and handels spatial data
library(mapview)         # makes a fun web map, to vizualize the dat
library(dplyr)           # data wrangling

rm(list=ls())            # clear


# Benoit's ideas (paraphrased by Dexter)

# build global IDs (block group, road, etc - the larger grouping unit)
# subset for those IDs (subset.nb?)
# calculate a distance such that all units within that group are neighs
#    use the row.names (the global ID) to ensure uniqueness
# recombine the nb lists for each IDs (union.nb?)

getwd()             # for Dexter this goes to "/nfs/dlocke-data/LMc/LMc_Bos"
list.files("data/")

# yards data
system.time(sf <- st_read('data/bf_chm.shp', quiet = F))
a <- object.size(sf); print(a, units='Mb'); rm(a)

# check out the IDs
rownames(sf)

# filter down to just two block groups
sf_slim   <- sf %>% filter(sample == 2); rownames(sf_slim)

# make an sp version of sf_slim (ie not a simple feature), then map it
sp <- as(sf_slim, "Spatial"); class(sp)

# filter down again to just front yards
spFront <- sp[sp$YARD == 'Front', ];dim(spFront); dim(sp)/2 #0 



# poly2nb KEEPS THE GLOBAL ID!!
system.time(nb_Q1 <- poly2nb(spFront[spFront@data$'ID_CBG' == 250251105011,],
                             queen=TRUE))
summary(nb_Q1); str(nb_Q1)

system.time(nb_Q2 <- poly2nb(spFront[spFront@data$'ID_CBG' == 250251105012,],
                             queen=TRUE))
summary(nb_Q2); str(nb_Q2)

# make short cut
nb.obj1 <- nb_Q1
nb.obj2 <- nb_Q2



### end of test, scrpas below




# get centroids as pre-cursor for KNN
cents <- SpatialPointsDataFrame(coords = coordinates(spFront),
                                data = spFront@data,
                                proj4string=CRS("+proj=utm +zone=19 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


knn <- knearneigh(cents[cents$'ID_CBG' == 250251105011,],
           k = 4)

# maintain the associatoin between global ID and the 'region.id" spdep builds
lt <- cbind(rownames(knn$x), 1:knn$np)

knnOne <- knn2nb(knn)



knnOne <- knn2nb(knearneigh(cents[cents$'ID_CBG' == 250251105011,],
                            k = 4))


knnTwo <- knn2nb(knearneigh(cents[cents$'ID_CBG' == 250251105012,],
                            k = 4))

# make short cut
nb.obj1 <- knnOne
nb.obj2 <- knnTwo







# what about distance-based neighbors?
myDist <- 25
system.time(nb_d<- dnearneigh(cents[cents@data$'ID_CBG' == 250251105011,],
                              d1=0, d2=myDist)); summary(nb_d)
# like knearniegh dnearneigh seems to drop the Global ID