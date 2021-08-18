## DO NOT RUN THIS CODE WITHOUT RESTARTING R, RE-USED OBJECT NAMES

library(terra)
library(tidyverse)

setwd("C:/Users/raenb/Documents/GitHub/madagascar")

# load forest density data (30m resolution) -------------

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

# load forest density in 2005, 2010, 2014 (resolution: 90 x 90) -------------

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


# load distance from forest edge in 2005, 2010, 2014 (resolution: 90 x 90) ------

edge05_90m  <- rast("data/distance_edge_90m/edge_05.tif")
edge10_90m  <- rast("data/distance_edge_90m/edge_10.tif")
edge14_90m  <- rast("data/distance_edge_90m/edge_14.tif")

# distance from forest edge in 2005, 2010, 2014 masked to CFM areas (resolution: 90 x 90)

cfm_edge05_90m  <- rast("data/distance_edge_90m/cfm_edge2005.tif")
cfm_edge10_90m  <- rast("data/distance_edge_90m/cfm_edge2010.tif")
cfm_edge14_90m  <- rast("data/distance_edge_90m/cfm_edge2014.tif")

# distance from forest edge in 2005, 2010, 2014 masked to PAs (resolution: 90 x 90)

pa_edge05_90m  <- rast("data/distance_edge_90m/pa_edge2005.tif")
pa_edge10_90m  <- rast("data/distance_edge_90m/pa_edge2010.tif")
pa_edge14_90m  <- rast("data/distance_edge_90m/pa_edge2014.tif")

# calculate summary statistics (sample)

edge05_90m_stats <- summary(edge05_90m)
edge10_90m_stats <- summary(edge10_90m)
edge14_90m_stats <- summary(edge14_90m)

cfm_edge05_90m_stats <- summary(cfm_edge05_90m)
cfm_edge10_90m_stats <- summary(cfm_edge10_90m)
cfm_edge14_90m_stats <- summary(cfm_edge14_90m)

pa_edge05_90m_stats <- summary(pa_edge05_90m)
pa_edge10_90m_stats <- summary(pa_edge10_90m)
pa_edge14_90m_stats <- summary(pa_edge14_90m)

# data wrangling

edge05_90m_mean <- parse_number(edge05_90m_stats[4])
edge10_90m_mean <- parse_number(edge10_90m_stats[4])
edge14_90m_mean <- parse_number(edge14_90m_stats[4])

edge05_90m_median <- parse_number(edge05_90m_stats[3])
edge10_90m_median <- parse_number(edge10_90m_stats[3])
edge14_90m_median <- parse_number(edge14_90m_stats[3])

cfm_edge05_90m_mean <- parse_number(cfm_edge05_90m_stats[4])
cfm_edge10_90m_mean <- parse_number(cfm_edge10_90m_stats[4])
cfm_edge14_90m_mean <- parse_number(cfm_edge14_90m_stats[4])

cfm_edge05_90m_median <- parse_number(cfm_edge05_90m_stats[3])
cfm_edge10_90m_median <- parse_number(cfm_edge10_90m_stats[3])
cfm_edge14_90m_median <- parse_number(cfm_edge14_90m_stats[3])

pa_edge05_90m_mean <- parse_number(pa_edge05_90m_stats[4])
pa_edge10_90m_mean <- parse_number(pa_edge10_90m_stats[4])
pa_edge14_90m_mean <- parse_number(pa_edge14_90m_stats[4])

pa_edge05_90m_median <- parse_number(pa_edge05_90m_stats[3])
pa_edge10_90m_median <- parse_number(pa_edge10_90m_stats[3])
pa_edge14_90m_median <- parse_number(pa_edge14_90m_stats[3])

#create vectors
edge_medians <- c(edge05_90m_median, edge10_90m_median, edge14_90m_median) 
cfm_edge_medians <- c(cfm_edge05_90m_median, cfm_edge10_90m_median, cfm_edge14_90m_median)
pa_edge_medians <- c(pa_edge05_90m_median, pa_edge10_90m_median, pa_edge14_90m_median)

edge_means <- c(edge05_90m_mean, edge10_90m_mean, edge14_90m_mean)
cfm_edge_means <- c(cfm_edge05_90m_mean, cfm_edge10_90m_mean, cfm_edge14_90m_mean)
pa_edge_means <- c(pa_edge05_90m_mean, pa_edge10_90m_mean, pa_edge14_90m_mean)

#create data frame
edge_means_medians_df <- data.frame(edge_means, cfm_edge_means, pa_edge_means, edge_medians, cfm_edge_medians, pa_edge_medians, row.names=c("2005", "2010", "2014"))

#write to csv
write.csv(edge_means_medians_df,"outputs/distance_edge_means_medians_sample_periodic.csv")

# calculate global mean, sd values (not a sample)

glbl_mean_edge05_90m <- global(edge05_90m, 'mean', na.rm=TRUE)
sd_edge05_90m <- global(edge05_90m, 'sd', na.rm=TRUE)
glbl_mean_edge10_90m <- global(edge10_90m, 'mean', na.rm=TRUE)
sd_edge10_90m <- global(edge10_90m, 'sd', na.rm=TRUE)
glbl_mean_edge14_90m <- global(edge14_90m, 'mean', na.rm=TRUE)
sd_edge14_90m <- global(edge14_90m, 'sd', na.rm=TRUE)

glbl_mean_cfm_edge05_90m <- global(cfm_edge05_90m, 'mean', na.rm=TRUE)
sd_cfm_edge05_90m <- global(cfm_edge05_90m, 'sd', na.rm=TRUE)
glbl_mean_cfm_edge10_90m <- global(cfm_edge10_90m, 'mean', na.rm=TRUE)
sd_cfm_edge10_90m <- global(cfm_edge10_90m, 'sd', na.rm=TRUE)
glbl_mean_cfm_edge14_90m <- global(cfm_edge14_90m, 'mean', na.rm=TRUE)
sd_cfm_edge14_90m <- global(cfm_edge14_90m, 'sd', na.rm=TRUE)

glbl_mean_pa_edge05_90m <- global(pa_edge05_90m, 'mean', na.rm=TRUE)
sd_pa_edge05_90m <- global(pa_edge05_90m, 'sd', na.rm=TRUE)
glbl_mean_pa_edge10_90m <- global(pa_edge10_90m, 'mean', na.rm=TRUE)
sd_pa_edge10_90m <- global(pa_edge10_90m, 'sd', na.rm=TRUE)
glbl_mean_pa_edge14_90m <- global(pa_edge14_90m, 'mean', na.rm=TRUE)
sd_pa_edge14_90m <- global(pa_edge14_90m, 'sd', na.rm=TRUE)

#create vectors
glbl_means_edge <- c(glbl_mean_edge05_90m$mean, glbl_mean_edge10_90m$mean, glbl_mean_edge14_90m$mean) 
glbl_means_cfm_edge <- c(glbl_mean_cfm_edge05_90m$mean, glbl_mean_cfm_edge10_90m$mean, glbl_mean_cfm_edge14_90m$mean)
glbl_means_pa_edge <- c(glbl_mean_pa_edge05_90m$mean, glbl_mean_pa_edge10_90m$mean, glbl_mean_pa_edge14_90m$mean)

sds_edge <- c(sd_edge05_90m$sd, sd_edge10_90m$sd, sd_edge14_90m$sd)
sds_cfm_edge <- c(sd_cfm_edge05_90m$sd, sd_cfm_edge10_90m$sd, sd_cfm_edge14_90m$sd)
sds_pa_edge <- c(sd_pa_edge05_90m$sd, sd_pa_edge10_90m$sd, sd_pa_edge14_90m$sd)

#create data frame
edge_glbl_means_sds_df <- data.frame(glbl_means_edge, sds_edge, glbl_means_cfm_edge, sds_cfm_edge, glbl_means_pa_edge, sds_pa_edge, row.names=c("2005", "2010", "2014"))

#write to csv
write.csv(edge_glbl_means_sds_df,"outputs/distance_edge_means_sds_periodic.csv")

