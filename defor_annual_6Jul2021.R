library(sp)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(tidyverse) #note this overwrites some raster package functions extract() and select()

# setwd("C:/Users/raenb/Google Drive/Documents/GIS/_Madagascar_Political_Conflict_Analyses/_working_folder") #original data folder

setwd("C:/Users/raenb/Documents/GitHub/madagascar") #new data subfolder


# misc code chunks --------------------------------------------------------

#%>%
#st_transform(crs = projection(ag)) # match projection of ag data



# Load forest, Community Forest Management (CFM) forest, Protected Area (PA) forest, population data  -----------------------------------

# Periodic data (1990, 2000, 2005, 2010, 2014) -----------------------------------------

# total forest area in each time period (resolution: 30 x 30)

# for90 <- raster("data/forest/for1990.tif")
# for00 <- raster("data/forest/for2000.tif")
# for05 <- raster("data/forest/for2005.tif")
# for10 <- raster("data/forest/for2010.tif")
# for14 <- raster("data/forest/for2014.tif")

# repeat but using a raster stack instead of individual rasters

#forest_stack <- list.files("data/forest/", pattern = "*.tif$", full.names = TRUE) %>%
#  stack()

# Annual data (2000-2017) (resolution: 90 x 90)--------------------------------
# shouldn't need full forest cover data for anything at this stage

# forest_90m_stack <- list.files("data/forest_annual_90m/", pattern = "*.tif$", full.names = TRUE) %>%
#  stack()

# forest masked to pre-2005 CFM areas (resolution: 90 x 90)

cfm_for90_90m <- raster("data/cfm_forest_90m/cfm_for90_90m.tif") #note I left out pre-05 from object names
cfm_for00_90m <- raster("data/cfm_forest_90m/cfm_for00_90m.tif")
cfm_for05_90m <- raster("data/cfm_forest_90m/cfm_for05_90m.tif")
cfm_for10_90m <- raster("data/cfm_forest_90m/cfm_for10_90m.tif")
cfm_for14_90m <- raster("data/cfm_forest_90m/cfm_for14_90m.tif")

# as raster stack

cfm_forest_stack_90m <- list.files("data/cfm_forest_90m/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()

# forest masked to PAs in each time period (resolution: 30 x 30)

# pa_for90 <- raster("data/pa_forest_30m/PA_for90.tif")
# pa_for00 <- raster("data/pa_forest_30m/PA_for00.tif")
# pa_for05 <- raster("data/pa_forest_30m/PA_for05.tif")
# pa_for10 <- raster("data/pa_forest_30m/PA_for10.tif")
# pa_for14 <- raster("data/pa_forest_30m/PA_for14.tif")

# repeat using a raster stack
# pa_for_stack <- list.files("data/pa_forest_30m/", pattern = "*.tif$", full.names = TRUE) %>%
#   stack()

# forest masked to PAs in each time period (resolution: 90 x 90)

pa_for90_90m <- raster("data/pa_forest_90m/PA_for90_90m.tif")
pa_for00_90m <- raster("data/pa_forest_90m/PA_for00_90m.tif")
pa_for05_90m <- raster("data/pa_forest_90m/PA_for05_90m.tif")
pa_for10_90m <- raster("data/pa_forest_90m/PA_for10_90m.tif")
pa_for14_90m <- raster("data/pa_forest_90m/PA_for14_90m.tif")

# as a stack

pa_forest_stack_90m <- list.files("data/pa_forest_90m/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()


# population (resolution: 919.4886, 919.4886***)

# annual population (resolution: 919.4886, 919.4886) #doesn't work, rasters are different extents
#pop_stack <- list.files("data/population_annual/", pattern = "*.tif$", full.names = TRUE) %>%
#   stack()

pop00 <- raster("data/population_annual/lspop2000.tif")
pop01 <- raster("data/population_annual/lspop2001.tif")
pop02 <- raster("data/population_annual/lspop2002.tif")
pop03 <- raster("data/population_annual/lspop2003.tif")
pop04 <- raster("data/population_annual/lspop2004.tif")
pop05 <- raster("data/population_annual/lspop2005.tif")
pop06 <- raster("data/population_annual/lspop2006.tif")
pop07 <- raster("data/population_annual/lspop2007.tif")
pop08 <- raster("data/population_annual/lspop2008.tif")
pop09 <- raster("data/population_annual/lspop2009.tif")
pop10 <- raster("data/population_annual/lspop2010.tif")
pop11 <- raster("data/population_annual/lspop2011.tif")
pop12 <- raster("data/population_annual/lspop2012.tif")
pop13 <- raster("data/population_annual/lspop2013.tif")
pop14 <- raster("data/population_annual/lspop2014.tif")

extent(pop00)
extent(pop01)
extent(pop05) #extents don't match

res(pop00)

#re-project population rasters to get extents to be identical to create a raster stack 
pop01 <- projectRaster(from=pop01, to = pop00)
pop02 <- projectRaster(from=pop02, to = pop00)
pop03 <- projectRaster(from=pop03, to = pop00)
pop04 <- projectRaster(from=pop04, to = pop00)
pop05 <- projectRaster(from=pop05, to = pop00)
pop06 <- projectRaster(from=pop06, to = pop00)
pop07 <- projectRaster(from=pop07, to = pop00)
pop08 <- projectRaster(from=pop08, to = pop00)
pop09 <- projectRaster(from=pop09, to = pop00)
pop10 <- projectRaster(from=pop10, to = pop00)
pop11 <- projectRaster(from=pop11, to = pop00)
pop12 <- projectRaster(from=pop12, to = pop00)
pop13 <- projectRaster(from=pop13, to = pop00)
pop14 <- projectRaster(from=pop14, to = pop00)

# create a raster stack
pop_stack <- stack(c(pop00, pop01, pop02,  pop03, pop04, pop05, pop06, pop07, pop08, pop09, pop10, pop11, pop12, pop13, pop14))

# check raster attributes -----------------------------------------------

#for90
#ncell = 1,360,943,464
#resolution = 30, 30
# extent     : 298440, 1100820, 7155900, 8682420  (xmin, xmax, ymin, ymax)

#cfm_for90
#ncell = 1,128,032,256
#resolution = 30, 30
#values: 0, 255 (min, max) ? this seems wrong, should be 0, 1

#NAvalue(cfm_for90)
# -Inf #negative infinity is the NA value

#res(pop00)
# resolution of population data = 919.4886, 919.4886

# plot rasters ------------------------------------------------------------

# plot(for90)
# plot(for14)
# 
# par(mfrow=c(1,5)) #doesn't work very well
# plot(cfm_for90, col="darkgreen", main = "Forest in pre05 CFM 1990")
# plot(cfm_for00, col="darkgreen", main = "Forest in pre05 CFM 2000")
# plot(cfm_for05, col="darkgreen", main = "Forest in pre05 CFM 2005")
# plot(cfm_for10, col="darkgreen", main = "Forest in pre05 CFM 2010")
# plot(cfm_for14, col="darkgreen", main = "Forest in pre05 CFM 2014")
# 
# par(mfrow=c(1,5))
# plot(pa_for90, col="darkgreen", main = "Forest in PA 1990")
# plot(pa_for00, col="darkgreen", main = "Forest in PA 2000")
# plot(pa_for05, col="darkgreen", main = "Forest in PA 2005")
# plot(pa_for10, col="darkgreen", main = "Forest in PA 2010")
# plot(pa_for14, col="darkgreen", main = "Forest in PA 2014")


# check parallel trends assumption in forest cover ----------------------------------------

# calculate sum of rasters (count of forested pixels) 

# count of total forest pixels in each time period

sum_for90 <- cellStats(for90, 'sum')
sum_for00 <- cellStats(for00, 'sum')
sum_for05 <- cellStats(for05, 'sum')
sum_for10 <- cellStats(for10, 'sum')
sum_for14 <- cellStats(for14, 'sum')

# count of forest pixels in CFM areas in each time period

sum_cfm_for90 <- cellStats(cfm_for90, 'sum') #5,755,413
sum_cfm_for00 <- cellStats(cfm_for00, 'sum') #5,169,092
sum_cfm_for05 <- cellStats(cfm_for05, 'sum') #5,060,500
sum_cfm_for10 <- cellStats(cfm_for10, 'sum') #4,879,095
sum_cfm_for14 <- cellStats(cfm_for14, 'sum') #4,668,882

#count of forest pixels in PA areas in each time period

sum_pa_for90 <- cellStats(pa_for90, 'sum') #13,604,528
sum_pa_for00 <- cellStats(pa_for00, 'sum') #13,329,679
sum_pa_for05 <- cellStats(pa_for05, 'sum') #13,255,320
sum_pa_for10 <- cellStats(pa_for10, 'sum') #13,126,507
sum_pa_for14 <- cellStats(pa_for14, 'sum') #12,942,051

# plot deforestation trends to check parallel trends assumption

defor_trends <- data.frame("Year" = c("1990","2000", "2005", "2010","2014"), 
                           "CFM" = c(sum_cfm_for90, sum_cfm_for00, sum_cfm_for05, sum_cfm_for10, sum_cfm_for14),
                           "PA" = c(sum_pa_for90, sum_pa_for00, sum_pa_for05, sum_pa_for10, sum_pa_for14))
defor_trends
write.csv(defor_trends,"results/defor_trends.csv")

defor_trends <- read_csv("results/defor_trends.csv")

ggplot(data = defor_trends) +
  geom_point(mapping = aes(x = Year, y = CFM), color = "blue") +
  geom_line(mapping = aes(x = Year, y = CFM), group = 1, color = "blue") +
  geom_point(mapping = aes(x = Year, y = PA), color = "red")+
  geom_line(mapping = aes(x = Year, y = PA), group = 1, color = "red") +
  ylab("Count of forested 30 m grid cells")

# also tried y = log10(CFM) etc. to make more comparable

ggplot(data = defor_trends) +
  geom_point(mapping = aes(x = Year, y = log10(CFM)), color = "blue") +
  geom_line(mapping = aes(x = Year, y = log10(CFM)), group = 1, color = "blue") +
  geom_point(mapping = aes(x = Year, y = log10(PA)), color = "red")+
  geom_line(mapping = aes(x = Year, y = log10(PA)), group = 1, color = "red")  +
  ylab("Log10(Count of forested 30 m grid cells)")


# load covariates -------------------------------------------------------

# agricultural suitability (resolution 9198.335, 9198.335) #no longer using this data

# ag <- raster("data/covariates/agr_su_utm.tif")

#res(ag) #check resolution

# cfm_id (resolution 30, 30)

cfm_id <- raster("data/covariates/cfm_id.tif")

res(cfm_id) #check resolution

# distance from cart tracks (resolution 91.8397, 91.8397)

dist_cart <- raster("data/covariates/dist_cart_utm.tif")

res(dist_cart)

# distance from roads (resolution 91.8397, 91.8397)

dist_road <- raster("data/covariates/dist_road_utm.tif")

res(dist_road)

# distance from urban areas (resolution 91.8397, 91.8397)

dist_urban <- raster("data/covariates/dist_urb_utm.tif")

res(dist_urban)

# distance from villages (resolution 91.8397, 91.8397)

dist_vil <- raster("data/covariates/dist_vil_utm.tif")

res(dist_vil)

# elevation (resolution 91.8397, 91.8397)

elev <- raster("data/covariates/elev_utm.tif")

res(elev)

# protected area ID (resolution 30, 30)

pa_id <- raster("data/covariates/pa_id.tif")

res(pa_id)

# suitability for rice (0 is unsuitable, 1 is suitable) (resolution 91.8397, 91.8397)

rice <- raster("data/covariates/paddy_thr_utm.tif")

res(rice)

# annual avg precipitation (resolution 924.8285, 924.8285**)

precip <- raster("data/covariates/prec_yr_utm.tif")

res(precip)

# slope (resolution 91.8397, 91.8397)

slope <- raster("data/covariates/slope_utm.tif")

res(slope)

# vegetation type (resolution 30, 30 based on polygon)

veg_type <- raster("data/covariates/veg_type_rstr.tif")

res(veg_type)



# load polygons for cluster analysis ---------------

# vegetation types #no longer needed, already rasterized and added above

# veg_types <- read_sf("data/covariates/veg_types_UTM.shp") 

# protected areas

protected_areas <- read_sf("data/cfm_pa/MNP_PA_UTM.shp")

# CFM areas (all) #not planning to include this

#cfm_all <- read_sf("data/cfm_pa/CFM_all_UTM.shp") #there's also CFM_all_fix.shp if this one has issues

# CFM areas (pre-2005)

cfm_pre05 <- read_sf("data/cfm_pa/CFM_pre05_UTM.shp")

# communes #Ranaivo said not to include this

# communes <- read_sf("data/covariates/communes_UTM.shp")



# create templates at different resolutions ------------------------------------------------
# https://github.com/mstrimas/natcap/blob/master/02_planning-units.R

# 90m template

template_90m <- raster(xmn = 298440, xmx = 1100820, ymn = 7155900, ymx = 8682420, crs = "+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs", resolution = 90) #vals=0, template = for90

# 300m template

template_300m <- raster(xmn = 298440, xmx = 1100820, ymn = 7155900, ymx = 8682420, crs = "+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs", resolution = 300)


# re-sample (warp) all rasters to the same extent and resolution ----------

# code from Jacob Grippin (CISER)

#Needed packages
library(dplyr)
library(sf)
library(raster)
library(rgdal)
library(glue)
library(stringr)
library(gdalUtils) #Rachel added this one

template_90m <- raster("data/forest/for1990.tif") %>%
  # this removes all the values from the raster
  # just keeps the extent, resolution, and projection
  raster()
# change the resolution
res(template_90m) <- 90

e <- extent(template_90m)
crs <- projection(template_90m)

gdalwarp(srcfile = "data/forest/for1990.tif", dstfile = "data/results/for1990_90m.tif", 
         t_srs = '+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs', 
         tr = c(90, 90), te = c(298440, 7155930, 1100790, 8682420), r = "average", output_Raster = TRUE)

# r = "average" is Character. resampling_method("near"|"bilinear"|"cubic"|"cubicspline"|"lanczos"|"average"|"mode"|"max"|"min"|"med"|"q1"|"q3")


# Code from Matt S-M, revised to 90m and 300m from 1km and 10km

# just grab a single layer to use to create templates
# this removes all the values from the raster
# just keeps the extent, resolution, and projection

#template_90m <- raster("data/forest/for1990.tif") %>% 
  #raster()

  # change the resolution
#res(template_90m) <- 90

# now reproject changing resolution using gdal

#e <- extent(template_90m)
#crs <- projection(template_90m)

# this is your gdalwarp command

# cmd <- str_glue("gdalwarp -tr 90 90 -te {e@xmin} {e@ymin} {e@xmax} {e@ymax} ",
#                 "-t_srs '{projection(template_90m)}' ",
#                 # aggregate using the average, other options available here
#                 # https://gdal.org/programs/gdalwarp.html
#                 "-r average ",
#                 "forest/for1990.tif for1990_90m.tif")

# run this at the command line directly

#print(cmd)

# or run the command using r with

#system(cmd)

# you can then aggregate to your 300m resolution option with
for1990_30m <- raster("data/forest/for1990.tif")
for1990_300m <- aggregate(for90, fact = 10, fun = mean, filename = "forest1990_300m.tif")



# create a random sample of grid cells (Matt's code) ------------------------------------

#In terms of creating a random sample of cells, you can stack() all the layers once they're all aligned and in the 
# same projection. Make sure the layers are named sensibly in the stack, the names come from the filenames, so just 
# make sure the files are named nicely. Then use

library(raster)
library(sf)

#forest_annual_stack <- list.files("data/forest_annual_30m/", pattern = "*.tif$", full.names = TRUE) %>%
#  stack()

cfm_sample <- sampleRandom(cfm_for05_90m, size = 100, cells = TRUE, sp = TRUE) %>% #returns values < 1, I only want grid cells with values of 1
  st_as_sf() #also note this only samples the CFM for05 data, not all the raster data

write_sf(cfm_sample, "outputs/cfm_for05_sample_90m.gpkg")

pa_sample <- sampleRandom(pa_for05_90m, size = 100, cells = TRUE, sp = TRUE) %>% #returns values < 1, I only want grid cells with values of 1
  st_as_sf()

write_sf(pa_sample, "outputs/pa_for05_sample_90m.gpkg")

# This will create a GeoPackage storing the sampled cells in a spatial format. If you don't care about the spatial 
# component, you could convert to a data frame with st_drop_geometry(sample).




