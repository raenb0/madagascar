## DO NOT RUN THIS CODE WITHOUT RESTARTING R, RE-USED OBJECT NAMES

library(terra)
library(tidyverse)

setwd("C:/Users/raenb/Documents/GitHub/madagascar")

# load forest density data (30m resolution)

dens90 <- rast("data/density/fordens1990.tif")
dens00 <- rast("data/density/fordens2000.tif")
dens05 <- rast("data/density/fordens2005.tif")
dens10 <- rast("data/density/fordens2010.tif")
dens14 <- rast("data/density/fordens2014.tif")

# calculate summary statistics (sample)

dens90_stats <- summary(dens90)
dens00_stats <- summary(dens00)
dens05_stats <- summary(dens05)
dens10_stats <- summary(dens10)
dens14_stats <- summary(dens14)

dens90_stats
dens00_stats
dens05_stats
dens10_stats
dens14_stats

#dens90_stats[3]
#dens90_stats <- as.data.frame(dens90_stats) #doesn't work

# calculate global mean, sd value (not a sample)

mean_dens90 <- global(dens90, 'mean', na.rm=TRUE)
sd_dens90 <- global(dens90, 'sd', na.rm=TRUE)
#median_dens90 <- global(dens90, 'median') #doesn't work
mean_dens00 <- global(dens00, 'mean', na.rm=TRUE)
sd_dens00 <- global(dens00, 'sd', na.rm=TRUE)
mean_dens05 <- global(dens05, 'mean', na.rm=TRUE)
sd_dens05 <- global(dens05, 'sd', na.rm=TRUE)
mean_dens10 <- global(dens10, 'mean', na.rm=TRUE)
sd_dens10 <- global(dens10, 'sd', na.rm=TRUE)
mean_dens14 <- global(dens14, 'mean', na.rm=TRUE)
sd_dens14 <- global(dens14, 'sd', na.rm=TRUE)

#mean_dens90 <- mean(dens90) #creates a raster
#plot(dens90)
#plot(mean_dens90)

# load forest density in 2005, 2010, 2014 (resolution: 90 x 90)

dens05_90m <- rast("data/density/90m/fordens2005.tif")
dens10_90m <- rast("data/density/90m/fordens2010.tif")
dens14_90m <- rast("data/density/90m/fordens2014.tif")

# forest density in 2005, 2010, 2014, masked to CFM areas (resolution: 90 x 90)

cfm_dens05_90m <- rast("data/density/90m/cfm_dens2005.tif")
cfm_dens10_90m <- rast("data/density/90m/cfm_dens2010.tif")
cfm_dens14_90m <- rast("data/density/90m/cfm_dens2014.tif")

# forest density in 2005, 2010, 2014, masked to PAs (resolution: 90 x 90)

pa_dens05_90m <- rast("data/density/90m/pa_dens2005.tif")
pa_dens10_90m <- rast("data/density/90m/pa_dens2010.tif")
pa_dens14_90m <- rast("data/density/90m/pa_dens2014.tif")

# calculate summary statistics (sample)

dens05_90m_stats <- summary(dens05_90m)
dens10_90m_stats <- summary(dens10_90m)
dens14_90m_stats <- summary(dens14_90m)

cfm_dens05_90m_stats <- summary(cfm_dens05_90m)
cfm_dens10_90m_stats <- summary(cfm_dens10_90m)
cfm_dens14_90m_stats <- summary(cfm_dens14_90m)

pa_dens05_90m_stats <- summary(pa_dens05_90m)
pa_dens10_90m_stats <- summary(pa_dens10_90m)
pa_dens14_90m_stats <- summary(pa_dens14_90m)

# data wrangling

dens05_90m_stats
dens10_90m_stats
dens14_90m_stats

dens05_90m_mean <- parse_number(dens05_90m_stats[4])
dens10_90m_mean <- parse_number(dens10_90m_stats[4])
dens14_90m_mean <- parse_number(dens14_90m_stats[4])

dens05_90m_median <- parse_number(dens05_90m_stats[3])
dens10_90m_median <- parse_number(dens10_90m_stats[3])
dens14_90m_median <- parse_number(dens14_90m_stats[3])

cfm_dens05_90m_stats
cfm_dens10_90m_stats
cfm_dens14_90m_stats

cfm_dens05_90m_mean <- parse_number(cfm_dens05_90m_stats[4])
cfm_dens10_90m_mean <- parse_number(cfm_dens10_90m_stats[4])
cfm_dens14_90m_mean <- parse_number(cfm_dens14_90m_stats[4])

cfm_dens05_90m_median <- parse_number(cfm_dens05_90m_stats[3])
cfm_dens10_90m_median <- parse_number(cfm_dens10_90m_stats[3])
cfm_dens14_90m_median <- parse_number(cfm_dens14_90m_stats[3])

pa_dens05_90m_stats
pa_dens10_90m_stats
pa_dens14_90m_stats

pa_dens05_90m_mean <- parse_number(pa_dens05_90m_stats[4])
pa_dens10_90m_mean <- parse_number(pa_dens10_90m_stats[4])
pa_dens14_90m_mean <- parse_number(pa_dens14_90m_stats[4])

pa_dens05_90m_median <- parse_number(pa_dens05_90m_stats[3])
pa_dens10_90m_median <- parse_number(pa_dens10_90m_stats[3])
pa_dens14_90m_median <- parse_number(pa_dens14_90m_stats[3])

#create vectors
dens_medians <- c(dens05_90m_median, dens10_90m_median, dens14_90m_median) 
cfm_dens_medians <- c(cfm_dens05_90m_median, cfm_dens10_90m_median, cfm_dens14_90m_median)
pa_dens_medians <- c(pa_dens05_90m_median, pa_dens10_90m_median, pa_dens14_90m_median)
                  
dens_means <- c(dens05_90m_mean, dens10_90m_mean, dens14_90m_mean)
cfm_dens_means <- c(cfm_dens05_90m_mean, cfm_dens10_90m_mean, cfm_dens14_90m_mean)
pa_dens_means <- c(pa_dens05_90m_mean, pa_dens10_90m_mean, pa_dens14_90m_mean)

#create data frame
dens_means_medians_df <- data.frame(dens_means, cfm_dens_means, pa_dens_means, dens_medians, cfm_dens_medians, pa_dens_medians, row.names=c("2005", "2010", "2014"))

#write to csv
write.csv(dens_means_medians_df,"outputs/density_means_medians_sample_periodic.csv")


# calculate global mean, sd values (not a sample)

glbl_mean_dens05_90m <- global(dens05_90m, 'mean', na.rm=TRUE)
sd_dens05_90m <- global(dens05_90m, 'sd', na.rm=TRUE)
glbl_mean_dens10_90m <- global(dens10_90m, 'mean', na.rm=TRUE)
sd_dens10_90m <- global(dens10_90m, 'sd', na.rm=TRUE)
glbl_mean_dens14_90m <- global(dens14_90m, 'mean', na.rm=TRUE)
sd_dens14_90m <- global(dens14_90m, 'sd', na.rm=TRUE)

glbl_mean_cfm_dens05_90m <- global(cfm_dens05_90m, 'mean', na.rm=TRUE)
sd_cfm_dens05_90m <- global(cfm_dens05_90m, 'sd', na.rm=TRUE)
glbl_mean_cfm_dens10_90m <- global(cfm_dens10_90m, 'mean', na.rm=TRUE)
sd_cfm_dens10_90m <- global(cfm_dens10_90m, 'sd', na.rm=TRUE)
glbl_mean_cfm_dens14_90m <- global(cfm_dens14_90m, 'mean', na.rm=TRUE)
sd_cfm_dens14_90m <- global(cfm_dens14_90m, 'sd', na.rm=TRUE)

glbl_mean_pa_dens05_90m <- global(pa_dens05_90m, 'mean', na.rm=TRUE)
sd_pa_dens05_90m <- global(pa_dens05_90m, 'sd', na.rm=TRUE)
glbl_mean_pa_dens10_90m <- global(pa_dens10_90m, 'mean', na.rm=TRUE)
sd_pa_dens10_90m <- global(pa_dens10_90m, 'sd', na.rm=TRUE)
glbl_mean_pa_dens14_90m <- global(pa_dens14_90m, 'mean', na.rm=TRUE)
sd_pa_dens14_90m <- global(pa_dens14_90m, 'sd', na.rm=TRUE)

#create vectors
glbl_means_dens <- c(glbl_mean_dens05_90m$mean, glbl_mean_dens10_90m$mean, glbl_mean_dens14_90m$mean) 
glbl_means_cfm_dens <- c(glbl_mean_cfm_dens05_90m$mean, glbl_mean_cfm_dens10_90m$mean, glbl_mean_cfm_dens14_90m$mean)
glbl_means_pa_dens <- c(glbl_mean_pa_dens05_90m$mean, glbl_mean_pa_dens10_90m$mean, glbl_mean_pa_dens14_90m$mean)

sds_dens <- c(sd_dens05_90m$sd, sd_dens10_90m$sd, sd_dens14_90m$sd)
sds_cfm_dens <- c(sd_cfm_dens05_90m$sd, sd_cfm_dens10_90m$sd, sd_cfm_dens14_90m$sd)
sds_pa_dens <- c(sd_pa_dens05_90m$sd, sd_pa_dens10_90m$sd, sd_pa_dens14_90m$sd)

#create data frame
dens_glbl_means_sds_df <- data.frame(glbl_means_dens, sds_dens, glbl_means_cfm_dens, sds_cfm_dens, glbl_means_pa_dens, sds_pa_dens, row.names=c("2005", "2010", "2014"))

#write to csv
write.csv(dens_glbl_means_sds_df,"outputs/density_means_sds_periodic.csv")
