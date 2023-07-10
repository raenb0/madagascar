#Calculating annual deforestation based on Hansen data and Vieilledent 2000 forest cover
# Nov 1 2022

library(terra)
library(tidyverse)

setwd("C:/Users/raenb/Documents/GitHub/madagascar")

# load forest cover data 2000 (30m resolution) -------------

for2000 <- rast("data/forest/for2000.tif")
#for2000 #resolution 30x30, crs WGS 84 / UTM zone 38S (EPSG:32738)
#plot(for2000)

#load hansen defor data (already masked to Vieilledent forest cover in 2000 in QGIS)

defor_2000_2021 <- rast("data/defor_hansen/defor_2000-2021.tif")
#defor_2000_2021
#plot(defor_2000_2021)

#create version of for2000 that is all 0s to repace NA values in deforestation data
m <- c(0, 2, 0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE) #reclassification matrix
for2000_0 <- classify(for2000, rclmat, include.lowest=TRUE)
#plot(for2000_0) #check to make sure it looks OK
writeRaster(for2000_0,"data/defor_hansen/for2000_0.tif", overwrite=TRUE) #save to TIF

#load for2000_0 if needed
#for2000_0 <- rast("data/defor_hansen/for2000_0.tif")

#combine (cover) deforestation data with 0s to replace NA values with 0
defor_2000_2021_0 <- cover(defor_2000_2021, for2000_0, values=NA)
#plot(defor_2000_2021)
writeRaster(defor_2000_2021_0,"data/defor_hansen/defor_2000_2021_0.tif", overwrite=TRUE) #save to TIF

#load defor_2000_2021_0 if needed
#defor_2000_2021_0 <- rast("data/defor_hansen/defor_2000_2021_0.tif")

# Generate annual forest loss layers --------------------------------------
# split hansen defor raster (values 1:21) into 20 rasters with values 0 (no defor) or 1 (defor)

#split up the layers

# fast way: for loop

output.list <- list() # run before the loop

for(i in 1:21){ #testing, change to 1:21
  year_i <- 2000+i
  m <- c(0,0,0,
         1,i,1,
         i,21,0)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  defor_i <- classify(defor_2000_2021, rclmat, include.lowest=TRUE) #change to defor_2000_2021_0
  output.list[[i]] <- defor_i #save the object to a list
  names(output.list)[i] <- as.character(paste0("defor_",year_i)) #give the object a name
  f <- file.path("data/defor_hansen/annual/", paste0("defor_", year_i, ".tif"))
  writeRaster(defor_i, f, overwrite = TRUE)
}

#check selected output
plot(output.list$defor_2021) #throws an error but appears to work: Error in x$.self$finalize() : attempt to apply non-function


# Calculate forest cover in each year ----------------

# load forest cover data 2000 (30m resolution) -------------
for2000 <- rast("data/forest/for2000.tif")

# load annual deforestation data generated in the previous step
filelist_temp <- list.files(path = "data/defor_hansen/annual", full.names = TRUE) #lists the files
defor_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields

#subtract defor_stack from 2000 forest cover #TAKES A LONG TIME
#forest_cover_stack <- for2000 - defor_stack #insufficient disk space

#split up the defor layers
defor2001 <- subset(defor_stack,1)
defor2002 <- subset(defor_stack,2)
defor2003 <- subset(defor_stack,3)
defor2004 <- subset(defor_stack,4)
defor2005 <- subset(defor_stack,5)
defor2006 <- subset(defor_stack,6)
defor2007 <- subset(defor_stack,7)
defor2008 <- subset(defor_stack,8)
defor2009 <- subset(defor_stack,9)
defor2010 <- subset(defor_stack,10)
defor2011 <- subset(defor_stack,11)
defor2012 <- subset(defor_stack,12)
defor2013 <- subset(defor_stack,13)
defor2014 <- subset(defor_stack,14)
defor2015 <- subset(defor_stack,15)
defor2016 <- subset(defor_stack,16)
defor2017 <- subset(defor_stack,17)
defor2018 <- subset(defor_stack,18)
defor2019 <- subset(defor_stack,19)
defor2020 <- subset(defor_stack,20)
defor2021 <- subset(defor_stack,21)

#slow way, one at a time
for2001 <- for2000 - defor2001

#try a different for loop
output.list.forest <- list() # run before the loop
for(i in 1:21){ #runs out of memory, do a few years at a time
  year_i <- 2000+i
  forest_i <- for2000 - subset(defor_stack,i)
  output.list.forest[[i]] <- forest_i #save the object to a list
  names(output.list.forest)[i] <- as.character(paste0("forest_",year_i)) #give the object a name
  f <- file.path("data/forest_cover_hansen/", paste0("forest_", year_i, ".tif"))
  writeRaster(forest_i, f, overwrite = TRUE)
}


# Aggregate to 90m --------------------------------------------------------

#aggregate deforestation data to 90m

# load annual deforestation data generated in the previous step
filelist_temp <- list.files(path = "data/defor_hansen/annual", full.names = TRUE) #lists the files
defor_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields

#test for a single layer first
#note this "mean" function loses a lot of "edge" forest due to NA values
defor2001 <- subset(defor_stack,1) #calls a single SpatRaster
defor2001 #resolution: 30,30
defor_2001_90m <- aggregate(subset(defor_stack,1), fact=3, fun="mean") #aggregates by a factor of 3, using mean

#try by replacing NA values with 0 across entire raster (single raster test)
#avoids losing "edge" forest due to NA values
#if anything, now over-estimates extent of defor on forest edges (maybe OK for this analysis?)
defor2001_0 <- classify(defor2001, cbind(NA, NA, 0), include.lowest=TRUE) #reclassifies NA values as 0
defor2001_0_90m <- aggregate(defor2001_0, fact=3, fun="mean") #aggregates by a factor of 3, using mean
writeRaster(defor2001_0_90m, "data/defor_hansen/annual_90m/defor2001_0_90m.tif", overwrite = TRUE) #edge of forest not lost

#reclassify NA values as 0 across entire raster stack #takes a while to run
defor_stack_0 <- classify(defor_stack, cbind(NA, NA, 0), include.lowest=TRUE)

#generate annual defor at 90m using the version with 0s instead of NAs
#note this method might slightly over-estimate defor along forest edges (NA values replaced with 0s)
output.list.forest.90m <- list() # run before the loop
for(i in 1:21){ #testing, change to 1:21
  year_i <- 2000+i
  defor_90m_i <- aggregate(subset(defor_stack_0,i), fact=3, fun="mean")
  output.list.defor.90m[[i]] <- defor_90m_i #save the object to a list
  names(output.list.defor.90m)[i] <- as.character(paste0("defor_90m_",year_i)) #give the object a name
  f <- file.path("data/defor_hansen/annual_90m", paste0("defor", year_i, "_90m.tif"))
  writeRaster(defor_90m_i, f, overwrite = TRUE)
}


#aggregate forest cover data to 90m

# load annual forest cover data generated previously
forest_temp <- list.files(path = "data/forest_cover_hansen", full.names = TRUE) #lists the files
forest_stack <- rast(forest_temp) #creates a single SpatRaster with 20 fields

#reclassify NA values as 0 across entire raster stack #takes a while to run
forest_stack_0 <- classify(forest_stack, cbind(NA, NA, 0), include.lowest=TRUE)

#generate annual forest cover at 90m using the version with 0s instead of NAs
#note this method might slightly over-estimate forest along forest edges (NA values replaced with 0s)
output.list.forest.90m <- list() # run before the loop
for(i in 11:21){ #testing, change to 1:21 #runs out of memory, run 10 at a time
  year_i <- 2000+i
  forest_90m_i <- aggregate(subset(forest_stack_0,i), fact=3, fun="mean")
  output.list.forest.90m[[i]] <- forest_90m_i #save the object to a list
  names(output.list.forest.90m)[i] <- as.character(paste0("forest_90m_",year_i)) #give the object a name
  f <- file.path("data/forest_cover_hansen_90m", paste0("forest", year_i, "_90m.tif"))
  writeRaster(forest_90m_i, f, overwrite = TRUE)
}


# Calculate distance from forest edge (doesn't work) -------------------------------------

# load annual 90m forest cover data generated previously
# forest_temp <- list.files(path = "data/forest_cover_hansen_90m", full.names = TRUE) #lists the files
# forest_stack_90m <- rast(forest_temp) #creates a single SpatRaster with 20 fields

# split up the forest cover layers
# forest2001_90m <- subset(forest_stack_90m,1)

# distance to cells that are not NA #doesn't work
# forest2001_1 <- classify(forest2001_90m, cbind(1, NA)) #reclassify forest cover so it only takes values of 1 and NA
# distance2001 <- gridDistance(forest2001_90m, 0, chunk=TRUE) #calculate distance from each cell to nearest 0 cell
#Error : cannot allocate vector of size 18.0 Gb
#Error: [gridDistance] gridDistance failed. Perhaps the raster is too large (set 'chunk=TRUE')?
#Error: cannot allocate vector of size 3.2 Gb
#Error during wrapup: memory exhausted (limit reached?)
#Error: no more error handlers available (recursive errors?); invoking 'abort' restart

