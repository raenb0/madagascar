library(raster)
library(tidyverse)

setwd("C:/Users/raenb/Documents/GitHub/madagascar") #new data subfolder (default)


# just grab a single layer to use to create templates
template_30m <- raster("data/forest/for1990.tif") %>% 
  # this removes all the values from the raster
  # just keeps the extent, resolution, and projection
  raster()

res(template_30m) #check

template_90m <- raster("data/forest/for1990.tif") %>% 
  # this removes all the values from the raster
  # just keeps the extent, resolution, and projection
  raster()
# change the resolution to 90m
res(template_90m) <- 90

res(template_90m) #check

template_300m <- raster("data/forest/for1990.tif") %>% 
  # this removes all the values from the raster
  # just keeps the extent, resolution, and projection
  raster()
# change the resolution
res(template_300m) <- 300

res(template_300m) #check

# now reproject changing resolution using gdal (90 m )
e <- extent(template_90m)
crs <- projection(template_90m)
# this is your gdalwarp command
cmd <- str_glue("gdalwarp -tr 90 90 -te {e@xmin} {e@ymin} {e@xmax} {e@ymax} ",
                "-t_srs '{projection(template_90m)}' ",
                # aggregate using the average, other options available here
                # https://gdal.org/programs/gdalwarp.html
                "-r average ",
                "data/forest/for1990.tif data/results/for1990_90m.tif")
# run this at the command line directly
#print(cmd)
# or run the command using r with
system(cmd)

# you can then aggregate to your 300m resolution option with
r30m <- raster("data/forest/for1990.tif")
r90m <- aggregate(r30m, fact = 3, fun = sum)#, filename = "data/results/forest1990_90m.tif")
r90m_pct <- r90m / 9 #this takes the above summed raster and divides by 9 (count of 30m grid cells per 90m grid cell) to get forest cover as a percentage
writeRaster(r90m_pct, filename = "data/results/forest1990_90m_pct.tif", overwrite=TRUE)

r300m <- aggregate(r30m, fact = 10, fun = sum)#, filename = "data/results/forest1990_300m.tif")
r300m_pct <- r300m / 100 #divide by 100 (count of 30m grid cells per 300m grid cell)
writeRaster(r300m_pct, filename = "data/results/forest1990_300m_pct.tif", overwrite=TRUE)
