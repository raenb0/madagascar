library(sp)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(tidyverse) #note this overwrites some raster package functions extract() and select()

# setwd("C:/Users/raenb/Google Drive/Documents/GIS/_Madagascar_Political_Conflict_Analyses/_working_folder") #original data folder

setwd("C:/Users/raenb/Documents/GitHub/madagascar") #new data subfolder (default)


# misc code chunks --------------------------------------------------------

#%>%
#st_transform(crs = projection(ag)) # match projection of ag data



# Load forest, CFM forest, PA forest, population data  -----------------------------------

# total forest area in each time period (resolution: 30 x 30)

for90 <- raster("data/forest/for1990.tif")
for00 <- raster("data/forest/for2000.tif")
for05 <- raster("data/forest/for2005.tif")
for10 <- raster("data/forest/for2010.tif")
for14 <- raster("data/forest/for2014.tif")

# repeat but using a raster stack instead of individual rasters

forest_stack <- list.files("data/forest/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()

# forest masked to CFM areas in each time period (resolution: 30 x 30)

cfm_for90 <- raster("data/cfm_pre05_forest_utm/cfm_p05_for90.tif") #note I left out pre-05 from object names
cfm_for00 <- raster("data/cfm_pre05_forest_utm/cfm_p05_for00.tif")
cfm_for05 <- raster("data/cfm_pre05_forest_utm/cfm_p05_for05.tif")
cfm_for10 <- raster("data/cfm_pre05_forest_utm/cfm_p05_for10.tif")
cfm_for14 <- raster("data/cfm_pre05_forest_utm/cfm_p05_for14.tif")

# repeat using a raster stack
cfm_p05_for_stack <- list.files("data/cfm_pre05_forest_utm/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()

# forest masked to PAs in each time period (resolution: 30 x 30)

pa_for90 <- raster("data/pa_forest/PA_for90.tif")
pa_for00 <- raster("data/pa_forest/PA_for00.tif")
pa_for05 <- raster("data/pa_forest/PA_for05.tif")
pa_for10 <- raster("data/pa_forest/PA_for10.tif")
pa_for14 <- raster("data/pa_forest/PA_for14.tif")

# repeat using a raster stack
pa_for_stack <- list.files("data/pa_forest/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()

# population in each time period (resolution: 919.4886, 919.4886 so needs to be resampled)

pop00 <- raster("data/covariates/lspop2000_UTM.tif")
pop05 <- raster("data/covariates/lspop2005_UTM.tif")
pop10 <- raster("data/covariates/lspop2010_UTM.tif")
pop14 <- raster("data/covariates/lspop2014_UTM.tif")

#re-project population rasters to get extents to be identical to create a raster stack 
pop05 <- projectRaster(from=pop05, to = pop00)
pop10 <- projectRaster(from=pop10, to = pop00)
pop14 <- projectRaster(from=pop14, to = pop00)

# create a raster stack
pop_stack <- stack(c(pop00, pop05, pop10, pop14))

# check raster attributes -----------------------------------------------

for90
#ncell = 1,360,943,464
#resolution = 30, 30
# extent     : 298440, 1100820, 7155900, 8682420  (xmin, xmax, ymin, ymax)

cfm_for90
#ncell = 1,128,032,256
#resolution = 30, 30
#values: 0, 255 (min, max) ? this seems wrong, should be 0, 1

NAvalue(cfm_for90)
# -Inf #negative infinity is the NA value

res(pop00)
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

# agricultural suitability (resolution 9198.335, 9198.335)

ag <- raster("data/covariates/agr_su_utm.tif")

#ag #check resolution, projection

# distance from cart tracks (resolution 91.8397, 91.8397)

dist_cart <- raster("data/covariates/dist_cart_utm.tif")

# distance from roads (resolution 91.8397, 91.8397)

dist_road <- raster("data/covariates/dist_road_utm.tif")

# distance from urban areas (resolution 91.8397, 91.8397)

dist_urban <- raster("data/covariates/dist_urb_utm.tif")

# distance from villages (resolution 91.8397, 91.8397)

dist_vil <- raster("data/covariates/dist_vil_utm.tif")

# suitability for rice (0 is unsuitable, 1 is suitable) (resolution 91.8397, 91.8397)

rice <- raster("data/covariates/paddy_thr_utm.tif")

# slope (resolution 91.8397, 91.8397)

slope <- raster("data/covariates/slope_utm.tif")



# load polygons for cluster analysis ---------------

# vegetation types

veg_types <- read_sf("data/covariates/veg_types_UTM.shp") 

# protected areas

protected_areas <- read_sf("data/cfm_pa/MNP_PA_UTM.shp")

# CFM areas (all)

cfm_all <- read_sf("data/cfm_pa/CFM_all_UTM.shp") #there's also CFM_all_fix.shp if this one has issues

# CFM areas (pre-2005)

cfm_pre05 <- read_sf("data/cfm_pa/CFM_pre05_UTM.shp")

# communes 

communes <- read_sf("data/covariates/communes_UTM.shp")



# create templates at different resolutions ------------------------------------------------
# https://github.com/mstrimas/natcap/blob/master/02_planning-units.R

# 1 km template

template_1km <- raster(xmn = 298440, xmx = 1100820, ymn = 7155900, ymx = 8682420, crs = "+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs", resolution = 1000) #vals=0, template = for90

# 10 km template

template_10km <- raster(xmn = 298440, xmx = 1100820, ymn = 7155900, ymx = 8682420, crs = "+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs", resolution = 10000)


# re-sample (warp) all rasters to the same extent and resolution ----------



# create a random sample of grid cells ------------------------------------

#example from https://gis.stackexchange.com/questions/291446/generating-random-points-inside-raster-boundary-using-r

# which cells are not NA? 
notna <- which(!is.na(values(cfm_for90)))

# grab 2000 cell index numbers at random
samp <- base::sample(notna, 2000, replace = FALSE)

# and their values
sampdata_cfm_for90 <- cfm_for90[samp]
sampdata_cfm_for00 <- cfm_for00[samp]
sampdata_cfm_for05 <- cfm_for05[samp]
sampdata_cfm_for10 <- cfm_for10[samp]
sampdata_cfm_for14 <- cfm_for14[samp]

# and their location coordinates
samplocs <- xyFromCell(cfm_for90, samp)

# convert to data frames
samp_cfm_for90 <- as.data.frame(cbind(samplocs, samp, sampdata_cfm_for90))
names(samp_cfm_for90) <- c('x', 'y', 'index', 'value')

samp_cfm_for00 <- as.data.frame(cbind(samplocs, samp, sampdata_cfm_for00))
names(samp_cfm_for00) <- c('x', 'y', 'index', 'value')

samp_cfm_for05 <- as.data.frame(cbind(samplocs, samp, sampdata_cfm_for05))
names(samp_cfm_for05) <- c('x', 'y', 'index', 'value')

samp_cfm_for10 <- as.data.frame(cbind(samplocs, samp, sampdata_cfm_for10))
names(samp_cfm_for10) <- c('x', 'y', 'index', 'value')

samp_cfm_for14 <- as.data.frame(cbind(samplocs, samp, sampdata_cfm_for14))
names(samp_cfm_for14) <- c('x', 'y', 'index', 'value')

# and optionally, a spatial object, in sf or sp
library(sp)
samp_sp <- samp_cfm_for90
coordinates(samp_sp) <- ~x + y
crs(samp_sp) <- CRS('+init=epsg:4326')

library(sf)
samp_sf <- st_as_sf(as.data.frame(samp_cfm_for90), coords = c('x', 'y'), crs = 4326)



