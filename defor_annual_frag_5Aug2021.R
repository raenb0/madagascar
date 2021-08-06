library(sp)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(tidyverse) #note this overwrites some raster package functions extract() and select()

# setwd("C:/Users/raenb/Google Drive/Documents/GIS/_Madagascar_Political_Conflict_Analyses/_working_folder") #original data folder

setwd("C:/Users/raenb/Documents/GitHub/madagascar")


# Load protected areas, community forest managed areas (polygons) ---------------

# protected areas

protected_areas <- read_sf("data/cfm_pa/MNP_PA_UTM.shp")

# CFM areas (pre-2005)

cfm_pre05 <- read_sf("data/cfm_pa/CFM_pre05_UTM.shp")



# Load forest, CFM forest, PA forest, population (rasters)  -----------------------------------

# Periodic data (1990, 2000, 2005, 2010, 2014) -----------------------------------------

# total forest area in each time period (resolution: 30 x 30)

for90 <- raster("data/forest/for1990.tif")
for00 <- raster("data/forest/for2000.tif")
for05 <- raster("data/forest/for2005.tif")
for10 <- raster("data/forest/for2010.tif")
for14 <- raster("data/forest/for2014.tif")

# forest as a raster stack

forest_stack_30m <- list.files("data/forest/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()


# forest fragmentation (density) in each time period (resolution: 30 x 30)

frag90 <- raster("data/fragmentation/fordens1990.tif")
frag00 <- raster("data/fragmentation/fordens2000.tif")
frag05 <- raster("data/fragmentation/fordens2005.tif")
frag10 <- raster("data/fragmentation/fordens2010.tif")
frag14 <- raster("data/fragmentation/fordens2014.tif")

# fragmentation as a raster stack

frag_stack <- list.files("data/fragmentation/", pattern = "*.tif$", full.names = TRUE) %>%
 stack()


# forest masked to pre-2005 CFM areas (resolution: 30 x 30)
#note I left out pre-05 from object names

cfm_for90 <- raster("data/cfm_forest_30m/cfm_p05_for90.tif")
cfm_for00 <- raster("data/cfm_forest_30m/cfm_p05_for00.tif")
cfm_for05 <- raster("data/cfm_forest_30m/cfm_p05_for05.tif")
cfm_for10 <- raster("data/cfm_forest_30m/cfm_p05_for10.tif")
cfm_for14 <- raster("data/cfm_forest_30m/cfm_p05_for14.tif")

# CFM forest (30m) as raster stack

cfm_forest_stack_30m <- list.files("data/cfm_forest_30m/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()

# forest masked to pre-2005 CFM areas (resolution: 90 x 90)
#note I left out pre-05 from object names

cfm_for90_90m <- raster("data/cfm_forest_90m/cfm_for90_90m.tif")
cfm_for00_90m <- raster("data/cfm_forest_90m/cfm_for00_90m.tif")
cfm_for05_90m <- raster("data/cfm_forest_90m/cfm_for05_90m.tif")
cfm_for10_90m <- raster("data/cfm_forest_90m/cfm_for10_90m.tif")
cfm_for14_90m <- raster("data/cfm_forest_90m/cfm_for14_90m.tif")

# CFM forest (90m) as raster stack

cfm_forest_stack_90m <- list.files("data/cfm_forest_90m/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()

# forest masked to PAs in each time period (resolution: 30 x 30)

pa_for90 <- raster("data/pa_forest_30m/PA_for90.tif")
pa_for00 <- raster("data/pa_forest_30m/PA_for00.tif")
pa_for05 <- raster("data/pa_forest_30m/PA_for05.tif")
pa_for10 <- raster("data/pa_forest_30m/PA_for10.tif")
pa_for14 <- raster("data/pa_forest_30m/PA_for14.tif")

# PA forest (30m) as a raster stack
pa_for_stack_30m <- list.files("data/pa_forest_30m/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()

# forest masked to PAs in each time period (resolution: 90 x 90)

pa_for90_90m <- raster("data/pa_forest_90m/PA_for90_90m.tif")
pa_for00_90m <- raster("data/pa_forest_90m/PA_for00_90m.tif")
pa_for05_90m <- raster("data/pa_forest_90m/PA_for05_90m.tif")
pa_for10_90m <- raster("data/pa_forest_90m/PA_for10_90m.tif")
pa_for14_90m <- raster("data/pa_forest_90m/PA_for14_90m.tif")

# PA forest (90m) as a raster stack

pa_forest_stack_90m <- list.files("data/pa_forest_90m/", pattern = "*.tif$", full.names = TRUE) %>%
  stack()


# Annual data (2000-2017)-----------------------------------------------------------


# all forest (30m)

#forest_30m_stack <- list.files("data/forest_annual_30m/", pattern = "*.tif$", full.names = TRUE) %>%
#  stack() #Error in compareRaster(rasters) : different extent

# all forest (90m)

forest_90m_stack <- list.files("data/forest_annual_90m/", pattern = "*.tif$", full.names = TRUE) %>%
 stack()


# Calculate and plot trends in forest cover (30m) -------------------------------------

# count of total forest pixels in each time period (30m) using the raster stack

forest_30m_stack_sum <- cellStats(forest_stack_30m, 'sum') #takes some time

forest_30m_sum <- data.frame(forest_30m_stack_sum) #convert to data frame
forest_30m_sum$raster_year <- row.names(forest_30m_sum) # add rownames as column
forest_30m_sum <- rename(forest_30m_sum,
                                national_forest_sum = forest_30m_stack_sum)  #rename column
forest_30m_sum$year <- c(1990,2000,2005,2010,2014)

# count of forest pixels in CFM areas (30m, raster stack)

cfm_forest_stack_30m_sum <- cellStats(cfm_forest_stack_30m, 'sum') #takes some time

cfm_forest_30m_sum <- data.frame(cfm_forest_stack_30m_sum) #convert to data frame
cfm_forest_30m_sum$raster_year <- row.names(cfm_forest_30m_sum) # add rownames as column
cfm_forest_30m_sum <- rename(cfm_forest_30m_sum,
                         cfm_forest_sum = cfm_forest_stack_30m_sum)  #rename column
cfm_forest_30m_sum$year <- c(2000,2005,2010,2014,1990) #note order needs to be different

# count of forest pixels in PA areas (30m, raster stack)
pa_forest_stack_30m_sum <- cellStats(pa_for_stack_30m, 'sum') #takes some time

pa_forest_30m_sum <- data.frame(pa_forest_stack_30m_sum) #convert to data frame
pa_forest_30m_sum$raster_year <- row.names(pa_forest_30m_sum) # add rownames as column
pa_forest_30m_sum <- rename(pa_forest_30m_sum,
                             pa_forest_sum = pa_forest_stack_30m_sum)  #rename column
pa_forest_30m_sum$year <- c(2000,2005,2010,2014,1990) #note order needs to be checked

# plot deforestation trends to check parallel trends assumption (updated code)

defor_trends_periodic <- left_join(forest_30m_sum, pa_forest_30m_sum, by = "year")
defor_trends_periodic <- left_join(defor_trends_periodic, cfm_forest_30m_sum, by = "year")

#write to CSV

write.csv(defor_trends_periodic,"outputs/defor_trends_periodic_5Aug2021.csv") #update date

defor_trends_periodic <- read_csv("outputs/defor_trends_periodic_5Aug2021.csv")

# reshape and plot

library(reshape2)

defor_trends_select <- defor_trends_periodic %>%
  select(year, national_forest_sum, pa_forest_sum, cfm_forest_sum)
defor_trends_periodic_reshape = melt(defor_trends_select, id=c("year")) #reshape data

defor_trends_periodic_plot <- ggplot(defor_trends_periodic_reshape) + 
  geom_line(aes(x=year, y=value, colour=variable)) +
  geom_point(aes(x=year, y=value, colour=variable)) +
  scale_colour_manual(values=c("red","blue", "green"))  +
  ylab("Forest loss (count of 30m pixels)")

defor_trends_periodic_plot


#create new data frame for forest cover as a % of 1990 forest cover (baseline)

defor_trends_periodic_pct <- data.frame(year=defor_trends_periodic$year,
                                        national_pct = defor_trends_periodic$national_forest_sum / (defor_trends_periodic$national_forest_sum[1]),
                                        cfm_pct = defor_trends_periodic$cfm_forest_sum / (defor_trends_periodic$cfm_forest_sum[1]),
                                        pa_pct = defor_trends_periodic$pa_forest_sum / (defor_trends_periodic$pa_forest_sum[1]))

View(defor_trends_periodic_pct)

# reshape and plot

defor_trends_periodic_pct_reshape = melt(defor_trends_periodic_pct, id=c("year")) #reshape data

defor_trends_periodic_pct_plot <- ggplot(defor_trends_periodic_pct_reshape) + 
  geom_line(aes(x=year, y=value, colour=variable)) +
  geom_point(aes(x=year, y=value, colour=variable)) +
  scale_colour_manual(values=c("red","blue", "green"))  +
  ylab("Forest loss (% of 1990 forest cover)")

defor_trends_periodic_pct_plot

#save plots
ggsave("./outputs/forest_loss_periodic_5Aug2021.png", plot = defor_trends_periodic_plot)
ggsave("./outputs/forest_loss_periodic_pct_5Aug2021.png", plot = defor_trends_periodic_pct_plot)

# Calculate ANNUAL forest loss in Madagascar nationally (90m) --------------------

forest_madagascar_sum <- cellStats(forest_90m_stack, stat='sum') %>% #takes time to run
  as.data.frame()

forest_madagascar_sum <- data.frame(year = row.names(forest_madagascar_sum), forest_madagascar_sum) #add rownames as column

names(forest_madagascar_sum)

forest_madagascar_sum <- rename(forest_madagascar_sum, #rename columns
                            forest_sum = .)

forest_madagascar_sum$year<-gsub("for_","",as.character(forest_madagascar_sum$year)) #remove text "for_" from "year" column

forest_madagascar_sum$year<-gsub("_90m","",as.character(forest_madagascar_sum$year)) #remove text "_90m" from "year" column

forest_madagascar_sum$year <- as.numeric(forest_madagascar_sum$year)

#calculate annual forest cover as a percent of 1990 forest cover (baseline)

forest_madagascar_pct <- data.frame(year=forest_madagascar_sum$year,
                                    forest_pct = forest_madagascar_sum$forest_sum/(forest_madagascar_sum$forest_sum[1]))

View(forest_madagascar_pct)

#write to CSV files
write.csv(forest_madagascar_sum, file = "./outputs/forest_madagascar_sum.csv")
write.csv(forest_madagascar_pct, file = "./outputs/forest_madagascar_pct.csv")

#plot

forest_madagascar_sum_plot <- ggplot(forest_madagascar_sum) + 
  geom_line(aes(x=year, y=forest_sum)) +
  geom_point(aes(x=year, y=forest_sum)) +
  ylab("Sum of forested pixels (90m)")

forest_madagascar_sum_plot

forest_madagascar_pct_plot <- ggplot(forest_madagascar_pct) + 
  geom_line(aes(x=year, y=forest_pct)) +
  geom_point(aes(x=year, y=forest_pct)) +
  ylab("Forest cover as a percent of 1990 forest cover (90m)")

forest_madagascar_pct_plot

# save plots

ggsave("./outputs/forest_madagascar_sum.png", plot = forest_madagascar_sum_plot)
ggsave("./outputs/forest_madagascar_pct.png", plot = forest_madagascar_pct_plot)



# Calculate ANNUAL forest loss in CFM and PAs (90m) ---------------------

library(prioritizr)

forest_90m_cfm <- fast_extract(forest_90m_stack, cfm_pre05, fun = "sum") %>%
  as.data.frame() %>%
  setNames(names(forest_90m_stack))

forest_90m_pa <- fast_extract(forest_90m_stack, protected_areas, fun = "sum") %>%
  as.data.frame() %>%
  setNames(names(forest_90m_stack))

forest_pa_sum <- as.data.frame(colSums(forest_90m_pa)) #calculate sum across all PAs
forest_pa_sum <- data.frame(year = row.names(forest_pa_sum), forest_pa_sum) #add rownames as column
forest_cfm_sum <- as.data.frame(colSums(forest_90m_cfm)) #calculate sum across all CFM
forest_cfm_sum <- data.frame(year = row.names(forest_cfm_sum), forest_cfm_sum) #add rownames as column

forest_pa_cfm_sum <- inner_join(forest_pa_sum,forest_cfm_sum) #join PA and CFM data

names(forest_pa_cfm_sum)

forest_pa_cfm_sum <- rename(forest_pa_cfm_sum, #rename columns
                            forest_pa = colSums.forest_90m_pa.,
                            forest_cfm = colSums.forest_90m_cfm.)

forest_pa_cfm_sum$year<-gsub("for_","",as.character(forest_pa_cfm_sum$year)) #remove text "for_" from "year" column

forest_pa_cfm_sum$year<-gsub("_90m","",as.character(forest_pa_cfm_sum$year)) #remove text "_90m" from "year" column

forest_pa_cfm_sum$year <- as.numeric(forest_pa_cfm_sum$year)

View(forest_pa_cfm_sum)

#calculate pa and cfm forest cover as a percent of 2000 forest cover (baseline)

forest_pa_cfm_pct <- data.frame(year=forest_pa_cfm_sum$year,
                                      forest_pa_pct = forest_pa_cfm_sum$forest_pa/(forest_pa_cfm_sum$forest_pa[1]),
                                   forest_cfm_pct = forest_pa_cfm_sum$forest_cfm/(forest_pa_cfm_sum$forest_cfm[1])
)

View(forest_pa_cfm_pct)

#join national, pa and cfm forest pct tables

forest_trends_annual_pct <- inner_join(forest_madagascar_pct, forest_pa_cfm_pct, by="year")

View(forest_trends_annual_pct)

library(reshape2)

forest_trends_pct_reshape = melt(forest_trends_annual_pct, id=c("year")) #reshape data

forest_trends_pct_plot <- ggplot(forest_trends_pct_reshape) + 
  geom_line(aes(x=year, y=value, colour=variable)) +
  geom_point(aes(x=year, y=value, colour=variable)) +
  scale_colour_manual(values=c("red","green", "blue"))  +
  ylab("Forest loss as a percentage of 2000 forest cover (%)")

forest_trends_pct_plot

ggsave("./outputs/forest_trends_annual_pct.png", plot = forest_trends_plot)


# Calculate trends in forest fragmentation in CFM and PAs (periodic) ---------------------
library(prioritizr)

frag_cfm <- fast_extract(frag_stack, cfm_pre05, fun = "mean") %>%
  as.data.frame() %>%
  setNames(names(frag_stack))

frag_pa <- fast_extract(frag_stack, protected_areas, fun = "mean") %>%
  as.data.frame() %>%
  setNames(names(frag_stack))

frag_pa_mean <- as.data.frame(colMeans(frag_pa)) #calculate mean across all PAs
frag_pa_mean <- data.frame(year = row.names(frag_pa_mean), frag_pa_mean) #add rownames as column
frag_cfm_mean <- as.data.frame(colMeans(frag_cfm))
frag_cfm_mean <- data.frame(year = row.names(frag_cfm_mean), frag_cfm_mean)

frag_pa_cfm_means <- inner_join(frag_pa_mean,frag_cfm_mean) #join PA and CFM data

frag_pa_cfm_means <- rename(frag_pa_cfm_means, #rename columns
                       frag_pa = colMeans.frag_pa.,
                       frag_cfm = colMeans.frag_cfm.)

frag_pa_cfm_means$year<-gsub("fordens","",as.character(frag_pa_cfm_means$year)) #remove text "fordens" from "year" column

frag_pa_cfm_means$year <- as.numeric(frag_pa_cfm_means$year)

# calculate the mean fragmentation in Madagascar forests nationally
frag90_mean <- cellStats(frag90, stat = 'mean')
frag00_mean <- cellStats(frag00, stat = 'mean')
frag05_mean <- cellStats(frag05, stat = 'mean')
frag10_mean <- cellStats(frag10, stat = 'mean')
frag14_mean <- cellStats(frag14, stat = 'mean')

# calculate the mean fragmentation in Madagascar forests nationally (raster stack)

frag_stack_mean <- cellStats(frag_stack, 'mean') #takes some time

frag_mean <- data.frame(frag_stack_mean) #convert to data frame
frag_mean$raster_year <- row.names(frag_mean) # add rownames as column
frag_mean <- rename(frag_mean,
                         national_frag_mean = frag_stack_mean)  #rename column
frag_mean$year <- c(1990,2000,2005,2010,2014) #check order


frag_madagascar <- c(frag90_mean, frag00_mean, frag05_mean, frag10_mean, frag14_mean) %>%
  as.data.frame()
frag_madagascar <- rename(frag_madagascar,
         frag_national = .)
frag_madagascar$year <- c(1990,2000,2005,2010,2014) #add a column for year

# try using a raster stack
#frag_madagascar_mean <- cellStats(frag_stack, stat='mean') %>%
#  as.data.frame()

frag_means <- inner_join(frag_pa_cfm_means, frag_madagascar)

#write to CSV file
write.csv(frag_means, file = "./outputs/frag_means.csv")

#plot fragmentation trends

library(reshape2)
frag_means_reshape = melt(frag_means, id=c("year")) #reshape data

frag_means_plot <- ggplot(frag_means_reshape) + 
  geom_line(aes(x=year, y=value, colour=variable)) +
  geom_point(aes(x=year, y=value, colour=variable)) +
  scale_colour_manual(values=c("red","blue", "green"))  +
  ylab("Mean forest intactness (%)")

ggsave("./outputs/frag_means.png", plot = frag_means_plot)



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




