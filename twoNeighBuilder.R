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

# does subsetting with the method done above maintain the row ids
rownames(spFront@data)

# build spatial weights matricies to see if this nb-type maintains ids too.
system.time(nb_Q1 <- poly2nb(spFront[spFront@data$'ID_CBG' == 250251105011,],
                             queen=TRUE))
summary(nb_Q1); str(nb_Q1)

# because "attr(*, "region.id")" goes higher than the number of elements
# (523) it appears as though the inital rowname is kept

# make another nb with a different subset
system.time(nb_Q2 <- poly2nb(spFront[spFront@data$'ID_CBG' == 250251105012,],
                             queen=TRUE))
summary(nb_Q2); str(nb_Q2)

# again the number of elements 342 is less than some of the region.id's
# therefore, we assume the original row number is maintained in region.id
# when subsetting with something like this 
# spFront[spFront@data$'ID_CBG' == 250251105012,]

# make short cut
nb.obj1 <- nb_Q1
nb.obj2 <- nb_Q2


### end of test subsetting, switch to custom.nb function in
### combiningSpatialWeights.R script
