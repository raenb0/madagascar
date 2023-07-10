#PARKING LOT
#Calculating annual deforestation based on Hansen data and Vieilledent 2000 forest cover
# October 26 2022

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


#slow way: each year one by one

#for deforestation in 2001
#all values >=0 and <= 0 become 0, >=1 and <= 1 become 1, values >= 1 and <= 21 become 0
m <- c(0,0,0, #0 to 0 becomes 0
       1,1,1, #1 to 1 becomes 1
       1,21,0) #1 to 21 becomes 0
rclmat <- matrix(m, ncol=3, byrow=TRUE) #reclassification matrix
defor_2001 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
plot(defor_2001) #check to make sure it looks OK
writeRaster(defor_2001,"data/defor_hansen/defor_2001.tif", overwrite=TRUE)

#for deforestation 2001-2002
m <- c(0,0,0,
       1,2,1,
       2,21,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2002 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
plot(defor_2002)
writeRaster(defor_2002,"data/defor_hansen/defor_2002.tif", overwrite=TRUE)

#deforestation 2001-2003
m <- c(0,0,0,
       1,3,1,
       3,21,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2003 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2003,"data/defor_hansen/defor_2003.tif", overwrite=TRUE)

#deforestation 2001-2004
m <- c(0,0,0,
       1,4,1,
       4,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2004 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2004,"data/defor_hansen/defor_2004.tif", overwrite=TRUE)

#deforestation 2001-2005
m <- c(0,0,0,
       1,5,1,
       5,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2005 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2005,"data/defor_hansen/defor_2005.tif", overwrite=TRUE)

#deforestation 2001-2006
m <- c(0,0,0,
       1,6,1,
       6,21,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2006 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2006,"data/defor_hansen/defor_2006.tif", overwrite=TRUE)

#deforestation 2001-2007
m <- c(0,0,0,
       1,7,1,
       7,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2007 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2007,"data/defor_hansen/defor_2007.tif", overwrite=TRUE)

#deforestation 2001-2008
m <- c(0,0,0,
       1,8,1,
       8,21,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2008 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2008,"data/defor_hansen/defor_2008.tif", overwrite=TRUE)

#deforestation 2001-2009
m <- c(0,0,0,
       1,9,1,
       9,21,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2009 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2009,"data/defor_hansen/defor_2009.tif", overwrite=TRUE)

#deforestation 2001-2010
m <- c(0,0,0,
       1,10,1,
       10,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2010 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2010,"data/defor_hansen/defor_2010.tif", overwrite=TRUE)

#deforestation 2001-2011
m <- c(0,0,0,
       1,11,1,
       11,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2011 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2011,"data/defor_hansen/defor_2011.tif", overwrite=TRUE)

#deforestation 2001-2012
m <- c(0,0,0,
       1,12,1,
       12,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2012 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2012,"data/defor_hansen/defor_2012.tif", overwrite=TRUE)

#deforestation 2001-2013
m <- c(0,0,0,
       1,13,1,
       13,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2013 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2013,"data/defor_hansen/defor_2013.tif", overwrite=TRUE)

#deforestation 2001-2014
m <- c(0,0,0,
       1,14,1,
       14,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2014 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2014,"data/defor_hansen/defor_2014.tif", overwrite=TRUE)

#deforestation 2001-2015
m <- c(0,0,0,
       1,15,1,
       15,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2015 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2015,"data/defor_hansen/defor_2015.tif", overwrite=TRUE)

#deforestation 2001-2016
m <- c(0,0,0,
       1,16,1,
       16,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2016 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2016,"data/defor_hansen/defor_2016.tif", overwrite=TRUE)

#deforestation 2001-2017
m <- c(0,0,0,
       1,17,1,
       17,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2017 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2017,"data/defor_hansen/defor_2017.tif", overwrite=TRUE)

#deforestation 2001-2018
m <- c(0,0,0,
       1,18,1,
       18,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2018 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2018,"data/defor_hansen/defor_2018.tif", overwrite=TRUE)

#deforestation 2001-2019
m <- c(0,0,0,
       1,19,1,
       19,21,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2019 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2019,"data/defor_hansen/defor_2019.tif", overwrite=TRUE)

#deforestation 2001-2020
m <- c(0,0,0,
       1,20,1,
       20,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2020 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2020,"data/defor_hansen/defor_2020.tif", overwrite=TRUE)

#deforestation 2001-2021
m <- c(0,0,0,
       1,21,1,
       21,21,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
defor_2021 <- classify(defor_2000_2021, rclmat, include.lowest=TRUE)
writeRaster(defor_2021,"data/defor_hansen/defor_2021.tif", overwrite=TRUE)



# REORGANIZE MAHALANOBIS MATCHED DATA FOR TWO-PERIOD ANALYSIS -----------------------
library(tidyverse)
library(dplyr)

#load tabular data if needed

matched <- read_csv('outputs/g.matches_8Jan2023.csv')  #load data, update date
names(matched)

#add new columns for deforestation

matched_2pr <- matched %>%
  mutate(defor.1 = defor2009-defor2005) %>% #this captures the NEW deforestation between 2005 and 2009
  mutate(defor.2 = defor2014-defor2009) #this captures the NEW deforestation between 2009 and 2014

#add distance to forest edge (Vieilledent data) at start of each period
matched_2pr <- matched_2pr %>%
  mutate(edge.1 = edge_05) %>%
  mutate(edge.2 = edge_10)

#add control variables (mean during each time period) #manually added avg MDG exchange rates

matched_2pr <- matched_2pr %>%
  rowwise() %>% mutate(distance.1 = mean(c(distance_2005:distance_2009))) %>%
  rowwise() %>% mutate(distance.2 = mean(c(distance_2010:distance_2014))) %>%
  rowwise() %>% mutate(pop.1 = mean(c(pop2005:pop2009))) %>%
  rowwise() %>% mutate(pop.2 = mean(c(pop2010:pop2014))) %>%
  rowwise() %>% mutate(riceav.1 = mean(c(rice_av_2005:rice_av_2009))*1936.756167) %>% #1936.756167 avg exch rate 2005-2009
  rowwise() %>% mutate(riceav.2 = mean(c(rice_av_2010:rice_av_2014))*2186.352) %>% #2186.352 avg exch rate 2010-2014
  rowwise() %>% mutate(ricesd.1 = mean(c(rice_sd_2005:rice_sd_2009))*1936.756167) %>%
  rowwise() %>% mutate(ricesd.2 = mean(c(rice_sd_2010:rice_sd_2014))*2186.352) %>%
  rowwise() %>% mutate(drght.1 = mean(c(drght2005:drght2009))) %>%
  rowwise() %>% mutate(drght.2 = mean(c(drght2010:drght2014))) %>%
  rowwise() %>% mutate(maxprecip.1 = mean(c(precip2005:precip2009))) %>%
  rowwise() %>% mutate(maxprecip.2 = mean(c(precip2010:precip2014))) %>%
  rowwise() %>% mutate(temp.1 = mean(c(temp2005:temp2009))) %>%
  rowwise() %>% mutate(temp.2 = mean(c(temp2010:temp2014))) %>%
  rowwise() %>% mutate(wind.1 = mean(c(wind2005:wind2009))) %>%
  rowwise() %>% mutate(wind.2 = mean(c(wind2010:wind2014)))

#Select desired variables, reorder columns

names(matched_2pr)

matched.subs <- matched_2pr %>%
  dplyr::select(subclass, x, y, CFM, PA, cfm_id_corrected, pa_id_corrected, dist_cart, dist_road, dist_urb, dist_vil, edge_05, edge_10, edge_14, elev, rain, rice, slope, veg_type, q1_materials, v7_security, 
                defor.1:wind.2) #grab all new columns at the end

#subset data to split up PA and CFM data

#for PA data:
PA_data <- matched.subs %>%
  filter(CFM==0)

#for CFM data:

CFM_data <- matched.subs %>%
  filter(CFM==1)

#add unique ID columns to each subset:
PA_data$ID <- seq.int(nrow(PA_data))
CFM_data$ID <- seq.int(nrow(CFM_data))

#add new variable PA_CFM
PA_data$PA_CFM <- "PA"
CFM_data$PA_CFM <- "CFM"

#create character strings for new unique IDs
PA_data$UID <- do.call(paste0, PA_data[c("PA_CFM", "ID")])
CFM_data$UID <- do.call(paste0, CFM_data[c("PA_CFM", "ID")])

#check to make sure they look ok
View(PA_data)
View(CFM_data)

#join the tables back together
matched_bind <- rbind(PA_data, CFM_data)
View(matched_bind)

#write to CSV
write_csv(matched_bind,'outputs/matched_bind_2pr_90m_8Jan2023.csv') #update date

names(matched_bind)

#reorganize
matched_longer <- matched_bind %>%
  pivot_longer(
    cols = defor.1:wind.2,
    names_to = c("timevariant", "period"),
    names_pattern = "([A-Za-z]+).(\\d+)", #([A-Za-z]+) indicates any letters, (\\d+) indicates numbers (year)
    values_to = "timevariant_values"
  )

#View
View(matched_longer)

#now pivot wider to get time variant variables as columns instead of rows
matched_wider <- matched_longer %>%
  pivot_wider(
    names_from = "timevariant",
    values_from = "timevariant_values"
  )

#View
View(matched_wider) #WORKED!

#add a new alphanumeric variable: "cluster" with the format cfm00pa00
names(matched_wider)
matched_wider$cluster <- do.call(paste0, c("cfm", matched_wider["cfm_id_corrected"], "pa", matched_wider["pa_id_corrected"]))
matched_wider$cluster <- as.factor(matched_wider$cluster) #convert "cluster" to a factor
#view(matched_wider)

#write to CSV
write_csv(matched_wider,'outputs/matched_wider_2pr_90m_8Jan2023.csv') #update date

## SPECIFY THE TWO-PERIOD MODEL, DEFORESTATION OUTCOME, CLUSTERED SE -------------------
#replace UID with cluster? ask Ranaivo/Chris

library(fixest)

did_m1_2pr <- feols(defor ~ CFM*period + edge + pop + riceav + ricesd + drght + maxprecip + temp + wind | UID + period, data = matched_wider) #note I used "edge" here (distance from forest edge in the beginning of each period), could I use "distance" (average distance from forest edge across each period?) maybe not

summary(did_m1_2pr, cluster = "cluster")


### SPECIFY THE ANNUAL DiD MODEL -------------------
#outcome variable: DEFORESTATION, so positive coefficients = positive effect on deforestation (undesired outcome)

library(tidyverse)
library(dplyr)
library(tidyr)
library(fixest)

#load data if necessary
matched_yr_wider_exchange <- read_csv('outputs/matched_yr_wider_90m_exchange_8Jan2023.csv') #update date
matched_yr_wider_exchange$year <- as.factor(matched_yr_wider_exchange$year) #convert "year" to a factor

did_m1_yr_robust <- feols(defor ~ CFM + year + CFM:year + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind | UID + year, data = matched_yr_wider_exchange) #included distance
summary(did_m1_yr_robust, cluster = "cluster")


# REORGANIZE RENEWED CFM, MAHALANOBIS MATCHED DATA FOR TWO-PERIOD ANALYSIS -----------------------

library(tidyverse)
library(dplyr)

#load tabular data if needed

matched_rnw <- read_csv('outputs/g.matches_rnw_8Jan2023.csv')  #update dates
names(matched_rnw)

#add new columns for deforestation

matched_rnw_2pr <- matched_rnw %>%
  mutate(defor.1 = defor2009-defor2005) %>% #this captures the NEW deforestation between 2005 and 2009
  mutate(defor.2 = defor2014-defor2009) #this captures the NEW deforestation between 2009 and 2014

#add distance to forest edge (Vieilledent data) at start of each period
matched_rnw_2pr <- matched_rnw_2pr %>%
  mutate(edge.1 = edge_05) %>%
  mutate(edge.2 = edge_10)

#add control variables (mean during each time period) #manually added avg MDG exchange rates

matched_rnw_2pr <- matched_rnw_2pr %>%
  rowwise() %>% mutate(distance.1 = mean(c(distance_2005:distance_2009))) %>%
  rowwise() %>% mutate(distance.2 = mean(c(distance_2010:distance_2014))) %>%
  rowwise() %>% mutate(pop.1 = mean(c(pop2005:pop2009))) %>%
  rowwise() %>% mutate(pop.2 = mean(c(pop2010:pop2014))) %>%
  rowwise() %>% mutate(riceav.1 = mean(c(rice_av_2005:rice_av_2009))*1936.756167) %>% #1936.756167 avg exch rate 2005-2009
  rowwise() %>% mutate(riceav.2 = mean(c(rice_av_2010:rice_av_2014))*2186.352) %>% #2186.352 avg exch rate 2010-2014
  rowwise() %>% mutate(ricesd.1 = mean(c(rice_sd_2005:rice_sd_2009))*1936.756167) %>%
  rowwise() %>% mutate(ricesd.2 = mean(c(rice_sd_2010:rice_sd_2014))*2186.352) %>%
  rowwise() %>% mutate(drght.1 = mean(c(drght2005:drght2009))) %>%
  rowwise() %>% mutate(drght.2 = mean(c(drght2010:drght2014))) %>%
  rowwise() %>% mutate(maxprecip.1 = mean(c(precip2005:precip2009))) %>%
  rowwise() %>% mutate(maxprecip.2 = mean(c(precip2010:precip2014))) %>%
  rowwise() %>% mutate(temp.1 = mean(c(temp2005:temp2009))) %>%
  rowwise() %>% mutate(temp.2 = mean(c(temp2010:temp2014))) %>%
  rowwise() %>% mutate(wind.1 = mean(c(wind2005:wind2009))) %>%
  rowwise() %>% mutate(wind.2 = mean(c(wind2010:wind2014)))

#Select desired variables, reorder columns

names(matched_rnw_2pr)

matched.rnw.subs <- matched_rnw_2pr %>%
  dplyr::select(subclass, x, y, CFM, PA, cfm_id_corrected, pa_id_corrected, dist_cart, dist_road, dist_urb, dist_vil, edge_05, edge_10, edge_14, elev, rain, rice, slope, veg_type, q1_materials, v7_security,   
                defor.1:wind.2) #grab all new columns at the end

#subset data to split up PA and CFM data

#for PA data:
PA_data_rnw <- matched.rnw.subs %>%
  filter(CFM==0)

#for CFM data:

CFM_data_rnw <- matched.rnw.subs %>%
  filter(CFM==1)

#add unique ID columns to each subset:
PA_data_rnw$ID <- seq.int(nrow(PA_data_rnw))
CFM_data_rnw$ID <- seq.int(nrow(CFM_data_rnw))

#add new variable PA_CFM
PA_data_rnw$PA_CFM <- "PA"
CFM_data_rnw$PA_CFM <- "CFM"

#create character strings for new unique IDs
PA_data_rnw$UID <- do.call(paste0, PA_data_rnw[c("PA_CFM", "ID")])
CFM_data_rnw$UID <- do.call(paste0, CFM_data_rnw[c("PA_CFM", "ID")])

#check to make sure they look ok
View(PA_data_rnw)
View(CFM_data_rnw)

#join the tables back together
matched_rnw_bind <- rbind(PA_data_rnw, CFM_data_rnw)
View(matched_rnw_bind)

names(matched_rnw_bind)

#write to CSV 
write_csv(matched_rnw_bind,'outputs/matched_rnw_bind_2pr_8Jan2023.csv') #update date

#reorganize
matched_rnw_longer <- matched_rnw_bind %>%
  pivot_longer(
    cols = defor.1:wind.2,
    names_to = c("timevariant", "period"),
    names_pattern = "([A-Za-z]+).(\\d+)", #([A-Za-z]+) indicates any letters, (\\d+) indicates numbers (year)
    values_to = "timevariant_values"
  )

#View
View(matched_rnw_longer)

#now pivot wider to get time variant variables as columns instead of rows
matched_rnw_wider <- matched_rnw_longer %>%
  pivot_wider(
    names_from = "timevariant",
    values_from = "timevariant_values"
  )

#View
View(matched_rnw_wider) #WORKED!  ***distance values look way too high in some cases

#add a new alphanumeric variable: "cluster" with the format cfm00pa00
names(matched_rnw_wider)
matched_rnw_wider$cluster <- do.call(paste0, c("cfm", matched_rnw_wider["cfm_id_corrected"], "pa", matched_rnw_wider["pa_id_corrected"]))
matched_rnw_wider$cluster <- as.factor(matched_rnw_wider$cluster) #convert "cluster" to a factor

#write to CSV
write_csv(matched_rnw_wider,'outputs/matched_rnw_wider_2pr_8Jan2023.csv') #update date

## SPECIFY THE TWO-PERIOD MODEL, DEFORESTATION OUTCOME, CLUSTERED SE -------------------
#replace UID with cluster? ask Ranaivo/Chris

library(fixest)

did_m1_rnw_2pr <- feols(defor ~ CFM*period + edge + pop + riceav + ricesd + drght + maxprecip + temp + wind | UID + period,
                        data = matched_rnw_wider)

summary(did_m1_rnw_2pr, cluster = "cluster")

#### SPECIFY THE ANNUAL DiD MODEL, RENEWED CFM DATA -------------------

library(tidyverse)
library(dplyr)
library(fixest)

#load data if needed
matched_yr_rnw_wider_exchange <- read_csv('outputs/matched_yr_rnw_wider_exchange_8Jan2023.csv') #update date!

names(matched_yr_rnw_wider_exchange)

matched_yr_rnw_wider_exchange$year <- as.factor(matched_yr_rnw_wider_exchange$year) #convert "year" to a factor

did_m1_yr_rnw_robust <- feols(defor ~ CFM*year + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind | UID + year, data = matched_yr_rnw_wider_exchange) #included distance

summary(did_m1_yr_rnw_robust, cluster = "cluster")

_____________
## commented out DiD models for now, not using them for the paper
# ### DiD model 1, outcome variable: ANNUAL DEFOR -------------------
# did_m1 <- feols(defor_annual ~ CFM*year
#                 + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
#                 | UID + year, #should I include year as an FE? ****
#                 data = matched_yr_wider,
#                 cluster = "cluster")
# 
# summary(did_m1)
# 
# ### DiD model 2, interaction term for distance from urban center, outcome variable: ANNUAL DEFOR -------------------
# #Note from Chris: rice prices increase as you get farther away from Tamatave, so try including an interaction term for distance from urban center (Note from Ranaivo: not exactly, rice prices in remote villages can also be quite low)
# 
# did_m2 <- feols(defor_annual ~ CFM*year
#                 + CFM*year*disturb #interaction term for distance from urban center
#                 + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
#                 | UID + year + vegtype,
#                 data = matched_yr_wider,
#                 cluster = "cluster")
# 
# summary(did_m2)
# 
# ### DiD model 3, interaction term for development, outcome variable: ANNUAL DEFOR -------------------
# did_m3 <- feols(defor_annual ~ CFM*year
#                 + CFM*year*development #interaction term for development
#                 + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
#                 | UID + year + vegtype,
#                 data = matched_yr_wider,
#                 cluster = "cluster")
# 
# summary(did_m3)
# 
# ### DiD model 4, interaction term for security, outcome variable: ANNUAL DEFOR -------------------
# did_m4 <- feols(defor_annual ~ CFM*year
#                 + CFM*year*security #interaction term for security
#                 + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
#                 | UID + year + vegtype,
#                 data = matched_yr_wider,
#                 cluster = "cluster")
# 
# summary(did_m4)


# ## Calculate (marginal effects) using ggeffects (error)
# library(ggeffects)
# summary(event_study_m1)
# ggpredict(event_study_m1, "CFM") #Error in `vectbl_as_col_location2()`: ! Can't extract column with `terms[2]`. ✖ Subscript `terms[2]` must be a location, not a character `NA`.

### Event study model 1, Marginal effects (doesn't work)
# adapted from https://grantmcdermott.com/interaction-effects/
# example: 
# original model:
# summary(lm(mpg ~ factor(am) * wt, mtcars))
# marginal effects: 
#library(fixest)
#feols(mpg ~ am:wt | am, mtcars2)

# Cluster standard errors with help from chatGPT
#library(lmtest)
#library(tictoc)
#library(beepr)
#
# tic()
# # Cluster standard errors with help from chatGPT
# # Create cluster matrix
# cluster_variable <- matched_yr_wider$cluster
# cluster_matrix <- model.matrix(~ cluster_variable - 1)
# 
# # Cluster standard errors
# clustered_se <- cluster.vcov(event_study_m1_plm, cluster_matrix) #Generated an error:
# # Error in vector("list", count) : vector size cannot be NA
# # In addition: Warning message:
# #   In combn(1:cluster_dims, i, simplify = FALSE) :
# #   NAs introduced by coercion to integer range
# 
# # Print coefficient estimates and clustered standard errors
# print(clustered_se)
# toc()
# beep()

# #try to cluster SE - takes forever, terminated process
# library(clubSandwich)
# startTime <- Sys.time()
# V_CR2 <- vcovCR(event_study_m1_plm, cluster=matched_yr_wider$cluster, type="CR2") # clustering SE
# endTime <- Sys.time()
# print(endTime - startTime)

# #try a different way
# library(lmtest)
# startTime <- Sys.time()
# coeftest(event_study_m1_plm, vcov=vcovHC(event_study_m1_plm,type="HC0",cluster="group")) #doesn't work because we need to cluster at the site level, different from the fixed effects (individual)
# endTime <- Sys.time()
# print(endTime - startTime)



### Event study model 1a, without rice prices and SD  ------------------------
event_study_m1a <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                         + distance + pop + drght + precip + temp + wind
                         | UID,
                         data = matched_yr_wider,
                         cluster = "cluster")
summary(event_study_m1a)
#tidy table and save
event_study_m1a_table <- tidy(event_study_m1a) %>%
  mutate(signif = stars.pval(p.value))
write_csv(event_study_m1a_table, "outputs/event_study_m1a_genetic_defor_annual_4Mar2023.csv") #update date


# #Plot DiD model (m1), ANNUAL DEFOR
# summary(did_m1)
# Fig.data.did1 <- tidy(did_m1) 
# Fig.data.did1 <- Fig.data.did1[ ,1:2]
# Fig.data.did1 <-  data.frame(cbind(Fig.data.did1, confint(did_m1)))
# names(Fig.data.did1) <- c("Variable", "Coeff", "Low.CI", "High.CI")
# which(Fig.data.did1$Variable=="CFM:year2006") #9
# which(Fig.data.did1$Variable=="CFM:year2020") #23
# Fig.data.did1 <- Fig.data.did1[9:23, ]  # select only the variables I want to plot
# Fig.data.did1 <- Fig.data.did1 %>%
#   mutate(CFM_year = c(2006:2020))
# Fig.data.did1$CFM_year <- as.factor(Fig.data.did1$CFM_year) #convert CFM_year to a factor
# #write to CSV
# write_csv(Fig.data.did1, "outputs/Fig.data.did1_defor_annual_27Feb2023.csv") #update date
# #plot
# ggplot(Fig.data.did1, mapping = aes(CFM_year,Coeff))+
#   geom_hline(yintercept=0, color = "grey", size=1)+
#   geom_point()+
#   geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
#   labs(x = "Year", y = "Coefficient of interaction of CFM and year")+
#   ggtitle("DiD: Coefficients of interaction of CFM and year on annual deforestation")+
#   theme_minimal()

# # Plot event study (m1) but include both MNP and CFM trends
# summary(event_study_m1)
# Fig.data.m1 <- tidy(event_study_m1)
# Fig.data.m1 <- Fig.data.m1[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
# #  Add CIs in the data frame
# Fig.data.m1 <-  data.frame(cbind(Fig.data.m1, confint(event_study_m1, level = 0.95), confint(event_study_m1, level = 0.90)))
# names(Fig.data.m1) <- c("Variable", "Coeff", "conf.low_95", "conf.high_95", "conf.low_90", "conf.high_90")    # Re-name the columns
# #export to CSV to plot in Excel for now
# write_csv(Fig.data.m1, "outputs/Fig.data.m1_genetic_defor_annual_1Mar2023.csv") #update date


## Trying to plot marginal effects many different ways ----------

event_study_m1_marginal <- feols(defor_annual ~ CFM:timestep + CFM:yrs_post_crisis #changed * to :
                                 + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                                 | UID,
                                 data = matched_yr_wider,
                                 cluster = "cluster")
summary(event_study_m1_marginal) #generates an output but not quite right

# try plm package for event study model 1
library(plm)
event_study_m1_plm <- plm(defor_annual ~ CFM*year + yrs_post_crisis + CFM*yrs_post_crisis
                          + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind,
                          data = matched_yr_wider,
                          effect="individual",
                          model = "within",
                          index = "UID")
summary(event_study_m1_plm) #coefficients are right but SE are not clustered


# ## Plot coefficients (marginal effects) using ggeffects (returned error message) ------------
# library(ggeffects)
# summary(event_study_m1_plm)
# ggeffect(event_study_m1_plm, "CFM") #Error message: Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : contrasts can be applied only to factors with 2 or more levels

#trying ggeffects package on original feols model
summary(event_study_m1)
library(ggeffects)
library(effects)

ggpredict(event_study_m1, terms="CFM") #Error

# try interplot #not compatible with fixest -------------
# https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
library(interplot)
plot_avg <- interplot(event_study_m1, var1 = "CFM", var2 = "yrs_post_crisis", predPro=F)+
  ggtitle("Average Conditional Effects")
#Error in (function (classes, fdef, mtable)  : unable to find an inherited method for function ‘sim’ for signature ‘"fixest"’


#### Try marginaleffects package -------------------
# https://vincentarelbundock.github.io/marginaleffects/index.html
library(marginaleffects)

marginal_means(event_study_m1, variables=c("CFM", "CFM:yrs_post_crisis"), conf_level=0.9)

plot_predictions(event_study_m1, condition = c("yrs_post_crisis", "CFM"), conf_level=0.9) #getting closer

#### Try using fixest i() interaction syntax -----------
event_study_m1_i <- feols(defor_annual ~ i(CFM, yrs_post_crisis) + i(CFM, year)
                          + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                          | UID,
                          cluster = ~cluster,
                          data = matched_yr_wider)
summary(event_study_m1_i)

iplot(event_study_m1_i,
      xlab="Years post crisis",
      main="Event study: Years post crisis")

fixest::coefplot(event_study_m1_i, 
                 keep = c("yrs_post_crisis"),
                 main = "Main title",
                 value.lab = "Estimate and __ci__ Conf. Int.",
                 ylab = NULL,
                 xlab = "Years post crisis")

#tidy the coefficients table and save to csv
library(broom)
library(gtools)
event_study_m1_i_table <- tidy(event_study_m1_i) %>%
  mutate(signif = stars.pval(p.value))
write_csv(event_study_m1_i_table, "outputs/event_study_m1_i_30May2023.csv") #update date


#### add a year-CFM fixed effect term -------------------
#and then just plot that series and the year fixed effects estimates to get the pre-trends, try "year" instead of "timestep"
#including year:CFM fixed effect generates an error
#or, convert "year" to a factor and just run the same event study model
event_study_m1_pre_trends <- feols(defor_annual ~ CFM*year + CFM*yrs_post_crisis
                                   + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                                   | UID + year:CFM, #generates an error
                                   data = matched_yr_wider,
                                   cluster = "cluster")
summary(event_study_m1_pre_trends)

#tidy the table
library(broom)
library(gtools)
event_study_m1_pre_trends_table <- tidy(event_study_m1_pre_trends) %>%
  mutate(signif = stars.pval(p.value))
write_csv(event_study_m1_pre_trends_table, "outputs/event_study_m1_pre_trends_2May2023.csv") #update date
