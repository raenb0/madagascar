#Extract raster values to points
#January 6 2023

library(terra)
library(tidyverse)

setwd("C:/Users/raenb/Documents/GitHub/madagascar")

# Load and harmonize all raster datasets ----------------------------------

# load raw data ---------------------------------------------------------------

#load sample points
cfm_points <- vect('data/sample_points/cfm_for05_pts_id.shp')
cfm_rnw_points <- vect('data/sample_points/cfm_rnw_pts_id.shp')
pa_points <- vect('data/sample_points/pa_for05_pts_id.shp')
cfm_points
cfm_rnw_points
pa_points

#load raster to use as template
raster_template <- rast("data/defor_hansen/annual_90m/defor2005_90m.tif")
raster_template #resolution 90m, EPSG:32738
#raster_template_30m <- rast("data/defor_hansen/annual/defor_2005.tif")
#raster_template_30m

#load covariates
cfm_id <- rast('data/covariates/cfm_id.tif')
dist_cart <- rast('data/covariates/dist_cart.tif')
dist_road <- rast('data/covariates/dist_road.tif')
dist_urb <- rast('data/covariates/dist_urb.tif')
dist_vil <- rast('data/covariates/dist_vil.tif')
edge_05 <- rast('data/covariates/edge_05.tif')
edge_10 <- rast('data/covariates/edge_10.tif')
edge_14 <- rast('data/covariates/edge_14.tif')
elev <- rast('data/covariates/elev.tif')
pa_id <- rast('data/covariates/pa_id.tif')
rain <- rast('data/covariates/rain.tif') #note renamed this from "precip" to avoid confusion later
materials <- rast('data/covariates/q1_materials.tif')
rice <- rast('data/covariates/rice.tif')
slope <- rast('data/covariates/slope.tif')
security <- rast('data/covariates/v7_security.tif')
veg_type <- rast('data/covariates/veg_type.tif')

# load annual deforestation data
filelist_temp <- list.files(path = "data/defor_hansen/annual_90m", full.names = T) #lists the files
defor_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/defor_hansen/annual_90m", full.names = F)
names(defor_stack) <- names_temp

# load population data
filelist_temp <- list.files(path = "data/population", full.names = T) #lists the files
pop_stack <- rast(filelist_temp)
names_temp <- list.files(path = "data/population", full.names = F)
names(pop_stack) <- names_temp

# load forest edge distance data
filelist_temp <- list.files(path = "data/distance_90m", full.names = T)
distance_stack <- rast(filelist_temp)
names_temp <- list.files(path = "data/distance_90m", full.names = F)
names(distance_stack) <- names_temp

#load drought data
filelist_temp <- list.files(path = "data/climate/drought", full.names = T) 
drought_stack <- rast(filelist_temp)
names_temp <- list.files(path = "data/climate/drought", full.names = F)
names(drought_stack) <- names_temp

#load precip data
filelist_temp <- list.files(path = "data/climate/precip", full.names = T) 
precip_stack <- rast(filelist_temp)
names_temp <- list.files(path = "data/climate/precip", full.names = F) 
names(precip_stack) <- names_temp

#load temperature data
filelist_temp <- list.files(path = "data/climate/temp", full.names = T) 
temp_stack <- rast(filelist_temp)
names_temp <- list.files(path = "data/climate/temp", full.names = F) 
names(temp_stack) <- names_temp

#load wind data
filelist_temp <- list.files(path = "data/climate/wind", full.names = T) 
wind_stack <- rast(filelist_temp)
names_temp <- list.files(path = "data/climate/wind", full.names = F) 
names(wind_stack) <- names_temp

#load rice price data NOTE DIFFERENT LOCATION
setwd('C:/Users/raenb/Box/Documents/GIS/_Madagascar_Political_Conflict_Analyses/_working_folder/rice_prices')
# load rice price (avg) data
filelist_temp <- list.files(path = "rice_av", full.names = T)
rice_av_stack <- rast(filelist_temp)
names_temp <- list.files(path = "rice_av", full.names = F)
names(rice_av_stack) <- names_temp
# load rice price (sd) data
filelist_temp <- list.files(path = "rice_sd", full.names = T)
rice_sd_stack <- rast(filelist_temp)
names_temp <- list.files(path = "rice_sd", full.names = F)
names(rice_sd_stack) <- names_temp
setwd("C:/Users/raenb/Documents/GitHub/madagascar") #reset working directory

# #harmonize covariate rasters --------------------------------------------

startTime <- Sys.time() #to record time required to run this
cfm_id <- resample(cfm_id, raster_template, method="near") 
dist_cart <- resample(dist_cart, raster_template, method="near")
dist_road <- resample(dist_road, raster_template, method="near")
dist_urb <- resample(dist_urb, raster_template, method="near")
dist_vil <- resample(dist_vil, raster_template, method="near")
edge_05 <- resample(edge_05, raster_template, method="near")
edge_10 <- resample(edge_10, raster_template, method="near")
edge_14 <- resample(edge_14, raster_template, method="near")
elev <- resample(elev, raster_template, method="near")
materials <- resample(materials, raster_template, method="near")
pa_id <- resample(pa_id, raster_template, method="near")
rain <- resample(rain, raster_template, method="near") #note renamed this from "precip" to avoid confusion later
rice <- resample(rice, raster_template, method="near")
slope <- resample(slope, raster_template, method="near")
security <- resample(security, raster_template, method="near")
veg_type <- resample(veg_type, raster_template, method="near")
endTime <- Sys.time()
print(endTime - startTime) #5 minutes

#create a covariate stack for ease of processing later
covariate_list <- list(cfm_id, dist_cart, dist_road, dist_urb, dist_vil, edge_05, edge_10, edge_14, elev, materials, pa_id, rain, rice, slope, security, veg_type)
covariate_stack <- rast(covariate_list)
covariate_stack
names(covariate_stack)

#save harmonized covariate rasters as TIF files #only do this once
#f <- paste0("data/results/covariates/", names(covariate_stack), ".tif") #set file path and file names based on layers
#r <- writeRaster(covariate_stack, f, overwrite=FALSE) #write to TIF files
writeRaster(covariate_stack, "data/results/covariates/covariate_stack.tif", overwrite=TRUE)

# harmonize raster stacks  -----------------------------------------------

# **TAKES 15 MINUTES TO RUN** #only do this once
#not necessary for covariates (already harmonized above)
#not necessary for defor (already harmonized)

startTime <- Sys.time() #to record time required to run this

distance_stack <- resample(distance_stack, raster_template, method="near")
drought_stack <- resample(drought_stack, raster_template, method="near")
pop_stack <- resample(pop_stack, raster_template, method="near")
precip_stack <- resample(precip_stack, raster_template, method="near")
rice_av_stack <- resample(rice_av_stack, raster_template, method="near")
rice_sd_stack <- resample(rice_sd_stack, raster_template, method="near")
temp_stack <- resample(temp_stack, raster_template, method="near")
wind_stack <- resample(wind_stack, raster_template, method="near")

endTime <- Sys.time()
print(endTime - startTime) #15 minutes

#save harmonized rasters as TIF files #takes 8 min to run #only do this once
#defor layers already harmonized, harmonized covariate layers already saved above
# 
# startTime <- Sys.time()
# 
# #distance
# f <- paste0("data/results/distance/", names(distance_stack)) #set file path and file names based on layers
# r <- writeRaster(distance_stack, f, overwrite=FALSE) #write to TIF files
# #drought
# f <- paste0("data/results/drought/", names(drought_stack))
# r <- writeRaster(drought_stack, f, overwrite=FALSE)
# #population
# f <- paste0("data/results/population/", names(pop_stack))
# r <- writeRaster(pop_stack, f, overwrite=FALSE)
# #precip
# f <- paste0("data/results/precip/", names(precip_stack))
# r <- writeRaster(precip_stack, f, overwrite=FALSE)
# #rice price average
# f <- paste0("data/results/rice_av/", names(rice_av_stack))
# r <- writeRaster(rice_av_stack, f, overwrite=FALSE)
# #rice price SD
# f <- paste0("data/results/rice_sd/", names(rice_sd_stack))
# r <- writeRaster(rice_sd_stack, f, overwrite=FALSE)
# #temperature
# f <- paste0("data/results/temperature/", names(temp_stack))
# r <- writeRaster(temp_stack, f, overwrite=FALSE)
# #wind
# f <- paste0("data/results/wind/", names(wind_stack))
# r <- writeRaster(wind_stack, f, overwrite=FALSE)
# 
# endTime <- Sys.time()
# print(endTime - startTime) #8 minutes


# Extract raster values to points -----------------------------------------

# load harmonized 90m data if necessary -----------------------------------
setwd("C:/Users/raenb/Documents/GitHub/madagascar")

#load covariate data
covariate_stack <- rast('data/results/covariates/covariate_stack.tif')

# load annual deforestation data (90m)
filelist_temp <- list.files(path = "data/defor_hansen/annual_90m", full.names = T) #lists the files
defor_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/defor_hansen/annual_90m", full.names = F)
names(defor_stack) <- names_temp

#load distance data
filelist_temp <- list.files(path = "data/results/distance", full.names = T) #lists the files
distance_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/distance", full.names = F)
names(distance_stack) <- names_temp

#load drought data
filelist_temp <- list.files(path = "data/results/drought", full.names = T) #lists the files
drought_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/drought", full.names = F)
names(drought_stack) <- names_temp

#load population data
filelist_temp <- list.files(path = "data/results/population", full.names = T) #lists the files
pop_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/population", full.names = F)
names(pop_stack) <- names_temp

#load precip data
filelist_temp <- list.files(path = "data/results/precip", full.names = T) #lists the files
precip_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/precip", full.names = F)
names(precip_stack) <- names_temp

#load rice_av data
filelist_temp <- list.files(path = "data/results/rice_av", full.names = T) #lists the files
rice_av_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/rice_av", full.names = F)
names(rice_av_stack) <- names_temp

#load rice_sd data
filelist_temp <- list.files(path = "data/results/rice_sd", full.names = T) #lists the files
rice_sd_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/rice_sd", full.names = F)
names(rice_sd_stack) <- names_temp

#load temperature data
filelist_temp <- list.files(path = "data/results/temperature", full.names = T) #lists the files
temp_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/temperature", full.names = F)
names(temp_stack) <- names_temp

#load wind data
filelist_temp <- list.files(path = "data/results/wind", full.names = T) #lists the files
wind_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results/wind", full.names = F)
names(wind_stack) <- names_temp


# extract raster values to CFM sample points -----------------------------------------

#test for a single raster
cfm_points_cfmid <- terra::extract(cfm_id, cfm_points, xy=TRUE) #note got a couple "NaN" values - this shouldn't happen
sum(is.na(cfm_points_cfmid$cfm_id)) #91 NA values, why? #fixed this below

#run for raster stacks (cfm points)

startTime <- Sys.time() #to record time required to run this

#covariates
cfm_points_covariates <- terra::extract(covariate_stack, cfm_points, xy=TRUE)
write_csv(cfm_points_covariates, 'outputs/cfm_points_covariates.csv')
#defor
cfm_points_defor <- terra::extract(defor_stack, cfm_points)
write_csv(cfm_points_defor, 'outputs/cfm_points_defor.csv')
#distance
cfm_points_distance <- terra::extract(distance_stack, cfm_points)
write_csv(cfm_points_distance, 'outputs/cfm_points_distance.csv')
#drought
cfm_points_drought <- terra::extract(drought_stack, cfm_points)
write_csv(cfm_points_drought, 'outputs/cfm_points_drought.csv')
#population
cfm_points_pop <- terra::extract(pop_stack, cfm_points)
write_csv(cfm_points_pop, 'outputs/cfm_points_pop.csv')
#precip
cfm_points_precip <- terra::extract(precip_stack, cfm_points)
write_csv(cfm_points_precip, 'outputs/cfm_points_precip.csv')
#rice price avg
cfm_points_rice_av <- terra::extract(rice_av_stack, cfm_points)
write_csv(cfm_points_rice_av, 'outputs/cfm_points_rice_av.csv')
#rice price sd
cfm_points_rice_sd <- terra::extract(rice_sd_stack, cfm_points)
write_csv(cfm_points_rice_sd, 'outputs/cfm_points_rice_sd.csv')
#temperature
cfm_points_temp <- terra::extract(temp_stack, cfm_points)
write_csv(cfm_points_temp, 'outputs/cfm_points_temp.csv')
#wind
cfm_points_wind <- terra::extract(wind_stack, cfm_points)
write_csv(cfm_points_wind, 'outputs/cfm_points_wind.csv')

endTime <- Sys.time()
print(endTime - startTime) #2.6 minutes

#join cfm_points data tables - nested left join

cfm_points_data <- left_join(cfm_points_covariates, cfm_points_defor, by='ID') %>%
  left_join(., cfm_points_distance, by='ID') %>%
  left_join(., cfm_points_drought, by='ID') %>%
  left_join(., cfm_points_pop, by='ID') %>%
  left_join(., cfm_points_precip, by='ID') %>%
  left_join(., cfm_points_rice_av, by='ID') %>%
  left_join(., cfm_points_rice_sd, by='ID') %>%
  left_join(., cfm_points_temp, by='ID') %>%
  left_join(., cfm_points_wind, by='ID')

#remove "_90m.tif" and ".tif" from column names
colnames(cfm_points_data)<-gsub("_90m.tif","",colnames(cfm_points_data))
colnames(cfm_points_data)<-gsub(".tif","",colnames(cfm_points_data))

#add corrected CFM ID
cfm_points_df <- as.data.frame(cfm_points) %>% 
  rename("cfm_id_corrected" = "correctedI") #creates a data frame with the correct CFM IDs
cfm_points_data_corrected <- data.frame(cfm_points_df, cfm_points_data)

write_csv(cfm_points_data_corrected, 'outputs/cfm_points_data_6Jan2023.csv') #update date


# extract raster values to PA sample points -------------------------------

startTime <- Sys.time() #to record time required to run this

#covariates
pa_points_covariates <- terra::extract(covariate_stack, pa_points, xy=TRUE)
write_csv(pa_points_covariates, 'outputs/pa_points_covariates.csv')
#defor
pa_points_defor <- terra::extract(defor_stack, pa_points)
write_csv(pa_points_defor, 'outputs/pa_points_defor.csv')
#distance
pa_points_distance <- terra::extract(distance_stack, pa_points)
write_csv(pa_points_distance, 'outputs/pa_points_distance.csv')
#drought
pa_points_drought <- terra::extract(drought_stack, pa_points)
write_csv(pa_points_drought, 'outputs/pa_points_drought.csv')
#population
pa_points_pop <- terra::extract(pop_stack, pa_points)
write_csv(pa_points_pop, 'outputs/pa_points_pop.csv')
#precip
pa_points_precip <- terra::extract(precip_stack, pa_points)
write_csv(pa_points_precip, 'outputs/pa_points_precip.csv')
#rice price avg
pa_points_rice_av <- terra::extract(rice_av_stack, pa_points)
write_csv(pa_points_rice_av, 'outputs/pa_points_rice_av.csv')
#rice price sd
pa_points_rice_sd <- terra::extract(rice_sd_stack, pa_points)
write_csv(pa_points_rice_sd, 'outputs/pa_points_rice_sd.csv')
#temperature
pa_points_temp <- terra::extract(temp_stack, pa_points)
write_csv(pa_points_temp, 'outputs/pa_points_temp.csv')
#wind
pa_points_wind <- terra::extract(wind_stack, pa_points)
write_csv(pa_points_wind, 'outputs/pa_points_wind.csv')

endTime <- Sys.time()
print(endTime - startTime) #5.7 minutes

#join pa_points data tables - nested left join

pa_points_data <- left_join(pa_points_covariates, pa_points_defor, by='ID') %>%
  left_join(., pa_points_distance, by='ID') %>%
  left_join(., pa_points_drought, by='ID') %>%
  left_join(., pa_points_pop, by='ID') %>%
  left_join(., pa_points_precip, by='ID') %>%
  left_join(., pa_points_rice_av, by='ID') %>%
  left_join(., pa_points_rice_sd, by='ID') %>%
  left_join(., pa_points_temp, by='ID') %>%
  left_join(., pa_points_wind, by='ID')

#remove "_90m.tif" and ".tif" from column names
colnames(pa_points_data)<-gsub("_90m.tif","",colnames(pa_points_data))
colnames(pa_points_data)<-gsub(".tif","",colnames(pa_points_data))

#add corrected PA ID
pa_points_df <- as.data.frame(pa_points) %>% 
  rename("pa_id_corrected" = "ID") #creates a data frame with the correct PA IDs
pa_points_data_corrected <- data.frame(pa_points_df, pa_points_data)

write_csv(pa_points_data_corrected, 'outputs/pa_points_data_6Jan2023.csv') #update date


# extract raster values to RENEWED CFM sample points -----------------------------------------

startTime <- Sys.time() #to record time required to run this

#covariates
cfm_rnw_points_covariates <- terra::extract(covariate_stack, cfm_rnw_points, xy=TRUE)
write_csv(cfm_rnw_points_covariates, 'outputs/cfm_rnw_points_covariates.csv')
#defor
cfm_rnw_points_defor <- terra::extract(defor_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_defor, 'outputs/cfm_rnw_points_defor.csv')
#distance
cfm_rnw_points_distance <- terra::extract(distance_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_distance, 'outputs/cfm_rnw_points_distance.csv')
#drought
cfm_rnw_points_drought <- terra::extract(drought_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_drought, 'outputs/cfm_rnw_points_drought.csv')
#population
cfm_rnw_points_pop <- terra::extract(pop_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_pop, 'outputs/cfm_rnw_points_pop.csv')
#precip
cfm_rnw_points_precip <- terra::extract(precip_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_precip, 'outputs/cfm_rnw_points_precip.csv')
#rice price avg
cfm_rnw_points_rice_av <- terra::extract(rice_av_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_rice_av, 'outputs/cfm_rnw_points_rice_av.csv')
#rice price sd
cfm_rnw_points_rice_sd <- terra::extract(rice_sd_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_rice_sd, 'outputs/cfm_rnw_points_rice_sd.csv')
#temperature
cfm_rnw_points_temp <- terra::extract(temp_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_temp, 'outputs/cfm_rnw_points_temp.csv')
#wind
cfm_rnw_points_wind <- terra::extract(wind_stack, cfm_rnw_points)
write_csv(cfm_rnw_points_wind, 'outputs/cfm_rnw_points_wind.csv')

endTime <- Sys.time()
print(endTime - startTime) #1.2 min

#join cfm_rnw_points data tables - nested left join

cfm_rnw_points_data <- left_join(cfm_rnw_points_covariates, cfm_rnw_points_defor, by='ID') %>%
  left_join(., cfm_rnw_points_distance, by='ID') %>%
  left_join(., cfm_rnw_points_drought, by='ID') %>%
  left_join(., cfm_rnw_points_pop, by='ID') %>%
  left_join(., cfm_rnw_points_precip, by='ID') %>%
  left_join(., cfm_rnw_points_rice_av, by='ID') %>%
  left_join(., cfm_rnw_points_rice_sd, by='ID') %>%
  left_join(., cfm_rnw_points_temp, by='ID') %>%
  left_join(., cfm_rnw_points_wind, by='ID')

#remove "_90m.tif" and ".tif" from column names
colnames(cfm_rnw_points_data)<-gsub("_90m.tif","",colnames(cfm_rnw_points_data))
colnames(cfm_rnw_points_data)<-gsub(".tif","",colnames(cfm_rnw_points_data))

#add corrected CFM ID
cfm_rnw_points_df <- as.data.frame(cfm_rnw_points) %>% 
  rename("cfm_id_corrected" = "correctedI")
cfm_rnw_points_data_corrected <- data.frame(cfm_rnw_points_df, cfm_rnw_points_data)

write_csv(cfm_rnw_points_data_corrected, 'outputs/cfm_rnw_points_data_6Jan2023.csv') #update date


# Repeat all of the above for 270m data -----------------------------------

#load data if necessary
covariate_stack <- rast('data/results/covariates/covariate_stack.tif')

# up-sample harmonized covariate rasters from 90m to 270m ----------------

covariate_stack_270m <- aggregate(covariate_stack, fact = 3, fun="mean") #takes 1.2 min
covariate_stack_270m #check output
writeRaster(covariate_stack_270m, "data/results_270m/covariates/covariate_stack.tif", overwrite=FALSE)

#up-sample other raster stacks #takes ~7 min
startTime <- Sys.time()
defor_stack_270m <- aggregate(defor_stack, fact = 3, fun="mean")
distance_stack_270m <- aggregate(distance_stack, fact = 3, fun="mean")
drought_stack_270m <- aggregate(drought_stack, fact = 3, fun="mean")
pop_stack_270m <- aggregate(pop_stack, fact = 3, fun="mean")
precip_stack_270m <- aggregate(precip_stack, fact = 3, fun="mean")
rice_av_stack_270m <- aggregate(rice_av_stack, fact = 3, fun="mean")
rice_sd_stack_270m <- aggregate(rice_sd_stack, fact = 3, fun="mean")
temp_stack_270m <- aggregate(temp_stack, fact = 3, fun="mean")
wind_stack_270m <- aggregate(wind_stack, fact = 3, fun="mean")
endTime <- Sys.time()
print(endTime - startTime) #24 minutes

#save harmonized 270m rasters as TIF files #takes 14 min, only do this once
# 
# startTime <- Sys.time()
# #covariates
# f <- paste0("data/results_270m/covariates/", names(covariate_stack_270m), ".tif")
# r <- writeRaster(covariate_stack_270m, f, overwrite=FALSE)
# #defor #note resulting TIF files had "_90m" in layer names, so I edited them manually
# f <- paste0("data/results_270m/defor/", names(defor_stack_270m))
# r <- writeRaster(defor_stack_270m, f, overwrite=FALSE)
# #distance
# f <- paste0("data/results_270m/distance/", names(distance_stack)) 
# r <- writeRaster(distance_stack, f, overwrite=FALSE) 
# #drought
# f <- paste0("data/results_270m/drought/", names(drought_stack))
# r <- writeRaster(drought_stack, f, overwrite=FALSE)
# #population
# f <- paste0("data/results_270m/population/", names(pop_stack))
# r <- writeRaster(pop_stack, f, overwrite=FALSE)
# #precip
# f <- paste0("data/results_270m/precip/", names(precip_stack))
# r <- writeRaster(precip_stack, f, overwrite=FALSE)
# #rice price average
# f <- paste0("data/results_270m/rice_av/", names(rice_av_stack))
# r <- writeRaster(rice_av_stack, f, overwrite=FALSE)
# #rice price SD
# f <- paste0("data/results_270m/rice_sd/", names(rice_sd_stack))
# r <- writeRaster(rice_sd_stack, f, overwrite=FALSE)
# #temperature
# f <- paste0("data/results_270m/temperature/", names(temp_stack))
# r <- writeRaster(temp_stack, f, overwrite=FALSE)
# #wind
# f <- paste0("data/results_270m/wind/", names(wind_stack))
# r <- writeRaster(wind_stack, f, overwrite=FALSE)
# 
# endTime <- Sys.time()
# print(endTime - startTime) #14 min


# load harmonized 270m data if necessary -----------------------------------
setwd("C:/Users/raenb/Documents/GitHub/madagascar")

#load covariate data
covariate_stack_270m <- rast('data/results_270m/covariates/covariate_stack.tif')

# load annual deforestation data (270m)
filelist_temp <- list.files(path = "data/results_270m/defor", full.names = T) #lists the files
defor_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/defor", full.names = F)
names(defor_stack_270m) <- names_temp

#load distance data
filelist_temp <- list.files(path = "data/results_270m/distance", full.names = T) #lists the files
distance_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/distance", full.names = F)
names(distance_stack_270m) <- names_temp

#load drought data
filelist_temp <- list.files(path = "data/results_270m/drought", full.names = T) #lists the files
drought_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/drought", full.names = F)
names(drought_stack_270m) <- names_temp

#load population data
filelist_temp <- list.files(path = "data/results_270m/population", full.names = T) #lists the files
pop_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/population", full.names = F)
names(pop_stack_270m) <- names_temp

#load precip data
filelist_temp <- list.files(path = "data/results_270m/precip", full.names = T) #lists the files
precip_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/precip", full.names = F)
names(precip_stack_270m) <- names_temp

#load rice_av data
filelist_temp <- list.files(path = "data/results_270m/rice_av", full.names = T) #lists the files
rice_av_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/rice_av", full.names = F)
names(rice_av_stack_270m) <- names_temp

#load rice_sd data
filelist_temp <- list.files(path = "data/results_270m/rice_sd", full.names = T) #lists the files
rice_sd_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/rice_sd", full.names = F)
names(rice_sd_stack_270m) <- names_temp

#load temperature data
filelist_temp <- list.files(path = "data/results_270m/temperature", full.names = T) #lists the files
temp_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/temperature", full.names = F)
names(temp_stack_270m) <- names_temp

#load wind data
filelist_temp <- list.files(path = "data/results_270m/wind", full.names = T) #lists the files
wind_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/wind", full.names = F)
names(wind_stack_270m) <- names_temp

# extract 270m raster values to CFM sample points -----------------------------------------
# **CAREFUL THIS WILL OVERWRITE OBJECTS**

startTime <- Sys.time()

cfm_points_covariates <- terra::extract(covariate_stack_270m, cfm_points, xy=TRUE) #results in NA values for cfmid
#sum(is.na(cfm_points_covariates$cfm_id)) #820 NA values, shouldn't be the case
cfm_points_defor <- terra::extract(defor_stack_270m, cfm_points)
cfm_points_distance <- terra::extract(distance_stack_270m, cfm_points)
cfm_points_drought <- terra::extract(drought_stack_270m, cfm_points)
cfm_points_pop <- terra::extract(pop_stack_270m, cfm_points)
cfm_points_precip <- terra::extract(precip_stack_270m, cfm_points)
cfm_points_rice_av <- terra::extract(rice_av_stack_270m, cfm_points)
cfm_points_rice_sd <- terra::extract(rice_sd_stack_270m, cfm_points)
cfm_points_temp <- terra::extract(temp_stack_270m, cfm_points)
cfm_points_wind <- terra::extract(wind_stack_270m, cfm_points)

endTime <- Sys.time()
print(endTime - startTime) #2 min

#join cfm_points data tables - nested left join

cfm_points_data_270m <- left_join(cfm_points_covariates, cfm_points_defor, by='ID') %>%
  left_join(., cfm_points_distance, by='ID') %>%
  left_join(., cfm_points_drought, by='ID') %>%
  left_join(., cfm_points_pop, by='ID') %>%
  left_join(., cfm_points_precip, by='ID') %>%
  left_join(., cfm_points_rice_av, by='ID') %>%
  left_join(., cfm_points_rice_sd, by='ID') %>%
  left_join(., cfm_points_temp, by='ID') %>%
  left_join(., cfm_points_wind, by='ID')

#remove ".tif" from column names
colnames(cfm_points_data_270m)<-gsub("_90m.tif","",colnames(cfm_points_data_270m))
colnames(cfm_points_data_270m)<-gsub(".tif","",colnames(cfm_points_data_270m))

#add corrected CFM ID
cfm_points_df <- as.data.frame(cfm_points) %>% 
  rename("cfm_id_corrected" = "correctedI")
cfm_points_data_270m_corrected <- data.frame(cfm_points_df, cfm_points_data_270m)

write_csv(cfm_points_data_270m_corrected, 'outputs/cfm_points_data_270m_6Jan2023.csv') #update date

# extract 270m raster values to PA sample points -----------------------------------------
# **CAREFUL THIS WILL OVERWRITE OBJECTS**

startTime <- Sys.time()

pa_points_covariates <- terra::extract(covariate_stack_270m, pa_points, xy=TRUE)
pa_points_defor <- terra::extract(defor_stack_270m, pa_points)
pa_points_distance <- terra::extract(distance_stack_270m, pa_points)
pa_points_drought <- terra::extract(drought_stack_270m, pa_points)
pa_points_pop <- terra::extract(pop_stack_270m, pa_points)
pa_points_precip <- terra::extract(precip_stack_270m, pa_points)
pa_points_rice_av <- terra::extract(rice_av_stack_270m, pa_points)
pa_points_rice_sd <- terra::extract(rice_sd_stack_270m, pa_points)
pa_points_temp <- terra::extract(temp_stack_270m, pa_points)
pa_points_wind <- terra::extract(wind_stack_270m, pa_points)

endTime <- Sys.time()
print(endTime - startTime) #13 seconds

#join pa_points data tables - nested left join

pa_points_data_270m <- left_join(pa_points_covariates, pa_points_defor, by='ID') %>%
  left_join(., pa_points_distance, by='ID') %>%
  left_join(., pa_points_drought, by='ID') %>%
  left_join(., pa_points_pop, by='ID') %>%
  left_join(., pa_points_precip, by='ID') %>%
  left_join(., pa_points_rice_av, by='ID') %>%
  left_join(., pa_points_rice_sd, by='ID') %>%
  left_join(., pa_points_temp, by='ID') %>%
  left_join(., pa_points_wind, by='ID')

#remove "_90m.tif" and ".tif" from column names
colnames(pa_points_data_270m)<-gsub("_90m.tif","",colnames(pa_points_data_270m))
colnames(pa_points_data_270m)<-gsub(".tif","",colnames(pa_points_data_270m))

#add corrected PA ID
pa_points_df <- as.data.frame(pa_points) %>% 
  rename("pa_id_corrected" = "ID")
pa_points_data_270m_corrected <- data.frame(pa_points_df, pa_points_data_270m)

write_csv(pa_points_data_270m_corrected, 'outputs/pa_points_data_270m_6Jan2023.csv') #update date

# extract 270m raster values to RENEWED CFM sample points -----------------------------------------
# **CAREFUL THIS WILL OVERWRITE OBJECTS**

startTime <- Sys.time()

cfm_rnw_points_covariates <- terra::extract(covariate_stack_270m, cfm_rnw_points, xy=TRUE)
cfm_rnw_points_defor <- terra::extract(defor_stack_270m, cfm_rnw_points)
cfm_rnw_points_distance <- terra::extract(distance_stack_270m, cfm_rnw_points)
cfm_rnw_points_drought <- terra::extract(drought_stack_270m, cfm_rnw_points)
cfm_rnw_points_pop <- terra::extract(pop_stack_270m, cfm_rnw_points)
cfm_rnw_points_precip <- terra::extract(precip_stack_270m, cfm_rnw_points)
cfm_rnw_points_rice_av <- terra::extract(rice_av_stack_270m, cfm_rnw_points)
cfm_rnw_points_rice_sd <- terra::extract(rice_sd_stack_270m, cfm_rnw_points)
cfm_rnw_points_temp <- terra::extract(temp_stack_270m, cfm_rnw_points)
cfm_rnw_points_wind <- terra::extract(wind_stack_270m, cfm_rnw_points)

endTime <- Sys.time()
print(endTime - startTime) #2 seconds

#join cfm_rnw_points data tables - nested left join

cfm_rnw_points_data_270m <- left_join(cfm_rnw_points_covariates, cfm_rnw_points_defor, by='ID') %>%
  left_join(., cfm_rnw_points_distance, by='ID') %>%
  left_join(., cfm_rnw_points_drought, by='ID') %>%
  left_join(., cfm_rnw_points_pop, by='ID') %>%
  left_join(., cfm_rnw_points_precip, by='ID') %>%
  left_join(., cfm_rnw_points_rice_av, by='ID') %>%
  left_join(., cfm_rnw_points_rice_sd, by='ID') %>%
  left_join(., cfm_rnw_points_temp, by='ID') %>%
  left_join(., cfm_rnw_points_wind, by='ID')

#remove "_90m.tif" and ".tif" from column names
colnames(cfm_rnw_points_data_270m)<-gsub("_90m.tif","",colnames(cfm_rnw_points_data_270m))
colnames(cfm_rnw_points_data_270m)<-gsub(".tif","",colnames(cfm_rnw_points_data_270m))

#add corrected CFM ID
cfm_rnw_points_df <- as.data.frame(cfm_rnw_points) %>% 
  rename("cfm_id_corrected" = "correctedI")
cfm_rnw_points_data_270m_corrected <- data.frame(cfm_rnw_points_df, cfm_rnw_points_data_270m)

write_csv(cfm_rnw_points_data_270m_corrected, 'outputs/cfm_rnw_points_data_270m_6Jan2023.csv') #update date



## ADD OUTCOME VARIABLE: FOREST COVER to 270m data ----------------------------
library(terra)
library(tidyverse)

#load sample points
cfm_points <- vect('data/sample_points/cfm_for05_pts_id.shp')
cfm_rnw_points <- vect('data/sample_points/cfm_rnw_pts_id.shp')
pa_points <- vect('data/sample_points/pa_for05_pts_id.shp')

#load csv files
cfm_points_data_270m <- read_csv('outputs/cfm_points_data_270m_6Jan2023.csv') #update date
pa_points_data_270m <- read_csv('outputs/pa_points_data_270m_6Jan2023.csv') #update date
cfm_rnw_points_data_270m <- read_csv('outputs/cfm_rnw_points_data_270m_6Jan2023.csv') #update date

# # load annual forest cover data ***Note this is 90m, need 270m**
# filelist_temp <- list.files(path = "data/forest_cover_hansen_90m", pattern = "*.tif$", full.names = T) #lists the files
# forest_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
# names_temp <- list.files(path = "data/forest_cover_hansen_90m", pattern = "*.tif$", full.names = F)
# names(forest_stack) <- names_temp
# 
# #aggregate 90m forest cover to 270m
# startTime <- Sys.time()
# forest_stack_270m <- aggregate(forest_stack, fact = 3, fun="mean")
# endTime <- Sys.time()
# print(endTime - startTime)
# 
# #save resulting file to TIF #note resulting TIF files had "_90m" in layer names, so I edited them manually
# startTime <- Sys.time()
# f <- paste0("data/results_270m/forest/", names(forest_stack_270m))
# r <- writeRaster(forest_stack_270m, f, overwrite=FALSE)
# endTime <- Sys.time()
# print(endTime - startTime)

# load annual forest cover data 270m
filelist_temp <- list.files(path = "data/results_270m/forest", pattern = "*.tif$", full.names = T) #lists the files
forest_stack_270m <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/results_270m/forest", pattern = "*.tif$", full.names = F)
names(forest_stack_270m) <- names_temp

# extract 270m forest cover to cfm, pa, and cfm_rnw points
cfm_points_forest_270m <- terra::extract(forest_stack_270m, cfm_points, xy=T) %>%
  rename_with(~ gsub('.tif', '', .x)) #remove .tif from forest columns
pa_points_forest_270m <- terra::extract(forest_stack_270m, pa_points, xy=T) %>%
  rename_with(~ gsub('.tif', '', .x))
cfm_rnw_points_forest_270m <- terra::extract(forest_stack_270m, cfm_rnw_points, xy=T) %>%
  rename_with(~ gsub('.tif', '', .x))

#join 270m forest data to other data generated above
cfm_points_data_270m <- left_join(cfm_points_data_270m, cfm_points_forest_270m, by='ID')
pa_points_data_270m <- left_join(pa_points_data_270m, pa_points_forest_270m, by='ID')
cfm_rnw_points_data_270m <- left_join(cfm_rnw_points_data_270m, cfm_rnw_points_forest_270m, by='ID')

#spot check
names(cfm_points_data_270m_join)
cfm_points_data_270m_subset <- cfm_points_data_270m_join %>%
  select(x.x, x.y, y.x, y.y, defor2001,forest2001,defor2002,forest2002,defor2003,forest2003,defor2004,forest2004,defor2005,forest2005,defor2006,forest2006,defor2007,forest2007,defor2008,forest2008,defor2009,forest2009,defor2010,forest2010)
view(cfm_points_data_270m_subset) #looks ok

#save to CSV
write_csv(cfm_points_data_270m, 'outputs/cfm_points_data_270m_13Feb2023.csv') #update date
write_csv(pa_points_data_270m, 'outputs/pa_points_data_270m_13Feb2023.csv') #update date
write_csv(cfm_rnw_points_data_270m, 'outputs/cfm_rnw_points_data_270m_13Feb2023.csv') #update date


# ADD OUTCOME VARIABLE: FOREST COVER: Extract raster values to MATCHED 90m data points ----------------------------
library(terra)
library(tidyverse)
#load matched sample points
genetic_matches <- read_csv("outputs/genetic.matches_27Jan2023.csv") #update date, all CFM
genetic_matches_rnw <- read_csv("outputs/genetic.matches.rnw_pop_size_500_12Feb2023.csv") #update date, renewed CFM

#terra package, "vect" documentation
#You can use a data.frame to make a SpatVector of points
#vect(x, geom=c("lon", "lat"), crs="", keepgeom=TRUE)

matched_points <- vect(genetic_matches, geom=c("x","y"), crs="epsg:32738", keepgeom=TRUE) #original crs UTM zone 38S
plot(matched_points) #looks good
matched_points_rnw <- vect(genetic_matches_rnw, geom=c("x","y"), crs="epsg:32738", keepgeom=TRUE) #renewed CFM

# load annual forest cover data
filelist_temp <- list.files(path = "data/forest_cover_hansen_90m", pattern = "*.tif$", full.names = T) #lists the files
forest_stack <- rast(filelist_temp) #creates a single SpatRaster with 20 fields
names_temp <- list.files(path = "data/forest_cover_hansen_90m", pattern = "*.tif$", full.names = F)
names(forest_stack) <- names_temp

#check projections
crs(matched_points, describe=T, proj=F)
crs(matched_points_rnw, describe=T, proj=F) #renewed CFM
crs(forest_stack, describe=T, proj=F)

#extract forest cover to matched points (only takes a few seconds)
matched_points_forest <- terra::extract(forest_stack, matched_points, xy=F, bind=TRUE) #bind=TRUE retains attributes
matched_points_rnw_forest <- terra::extract(forest_stack, matched_points_rnw, xy=F, bind=TRUE) #renewed CFM

#convert to data frame and write to CSV
#all CFM
matched_points_forest_df <- as.data.frame(matched_points_forest) #convert to data frame
matched_points_forest_df <- matched_points_forest_df %>%
  rename_with(~ gsub('_90m.tif', '', .x)) #remove _90m.tif from forest columns
names(matched_points_forest_df)
write_csv(matched_points_forest_df, 'outputs/genetic_matched_points_forest_9Feb2023.csv') #update date
#renewed CFM
matched_points_rnw_forest_df <- as.data.frame(matched_points_rnw_forest) #convert to data frame
matched_points_rnw_forest_df <- matched_points_rnw_forest_df %>%
  rename_with(~ gsub('_90m.tif', '', .x)) #remove _90m.tif from forest columns
names(matched_points_rnw_forest_df)
write_csv(matched_points_rnw_forest_df, 'outputs/genetic_matched_points_rnw_forest_13Feb2023.csv') #update date

#spot check to see if defor and forest cover match
matched_points_subset <- matched_points_forest_df %>%
  select(x,y,defor2001, forest2001, defor2002, forest2002, defor2003, forest2003, defor2004, forest2004, defor2005, forest2005, defor2006, forest2006, defor2007, forest2007, defor2008, forest2008, defor2009, forest2009, defor2010, forest2010)
