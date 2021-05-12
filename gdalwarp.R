library(raster)
library(tidyverse)

# just grab a single layer to use to create templates
template_1km <- raster("forest/for1990.tif") %>% 
  # this removes all the values from the raster
  # just keeps the extent, resolution, and projection
  raster()
# change the resolution
res(template_1km) <- 1000

# now reproject changing resolution using gdal
e <- extent(template_1km)
crs <- projection(template_1km)
# this is your gdalwarp command
cmd <- str_glue("gdalwarp -tr 1000 1000 -te {e@xmin} {e@ymin} {e@xmax} {e@ymax} ",
                "-t_srs '{projection(template_1km)}' ",
                # aggregate using the average, other options available here
                # https://gdal.org/programs/gdalwarp.html
                "-r average ",
                "forest/for1990.tif for1990_1km.tif")
# run this at the command line directly
print(cmd)
# or run the command using r with
system(cmd)

# you can then aggregate to your 10km resolution option with
r1km <- raster("for1990_1km.tif")
r10km <- aggregate(r1km, fact = 10, fun = mean, filename = "forest1990_10km.tif")
