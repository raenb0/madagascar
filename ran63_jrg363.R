#Set directory to location of .tif file
setwd("C:\\Users\\Jacob\\Downloads")
#Needed packages
library(dplyr)
library(sf)
library(raster)
library(rgdal)
library(glue)
library(stringr)

template_90m <- raster("for1990.tif") %>%
  # this removes all the values from the raster
  # just keeps the extent, resolution, and projection
  raster()
# change the resolution
res(template_90m) <- 90

e <- extent(template_90m)
crs <- projection(template_90m)

gdalwarp(srcfile = "for1990.tif", dstfile = "for1990_90l.tif", 
         t_srs = '+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs', 
         tr = c(90, 90), te = c(298440, 7155930, 1100790, 8682420), r = "average", output_Raster = TRUE)
