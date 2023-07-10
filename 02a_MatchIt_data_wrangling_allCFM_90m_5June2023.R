# Madagascar impact evaluation
# Matching and data wrangling, all CFM data, 90m resolution
# June 5 2023

library(tidyverse)
library(MatchIt)
library(cobalt) #for plotting match balance

# Load data ---------------------------------------------------------------

setwd("C:/Users/raenb/Documents/GitHub/madagascar") # set working directory

# read in 90m data

cfm_90m_data <- read_csv("outputs/cfm_points_data_6Jan2023.csv") #update date
#cfm_rnw_90m_data <- read_csv("outputs/cfm_rnw_points_data_6Jan2023.csv") #update date
pa_90m_data <- read_csv("outputs/pa_points_data_6Jan2023.csv") #update date

#check that all three data frames have same number of variables (171)

# read in 270m data

#cfm_90m_data <- read_csv("outputs/cfm_points_data_270m_7Jan2023.csv") #update date
#cfm_rnw_90m_data <- read_csv("outputs/cfm_rnw_points_data_270m_7Jan2023.csv") #update date
#pa_90m_data <- read_csv("outputs/pa_points_data_270m_7Jan2023.csv") #update date


# Data wrangling --------------------------------------------------------

#PA ID = 0 is a real PA, so replace with 99
unique(cfm_90m_data$pa_id) #doesn't contain any 0 values, so no issue

# Filter out sample points that are in overlapping PA and CFM areas

unique(cfm_90m_data$pa_id) #get unique values of PA IDs, includes NA
cfm_90m_data <- cfm_90m_data %>% 
  mutate(pa_id = replace(as.numeric(pa_id), which(is.na(pa_id)), 0)) #replace NA values with 0
unique(cfm_90m_data$pa_id) #check if worked - no NA values, worked

cfm_90m_filter <- cfm_90m_data %>%
  filter(cfm_90m_data$pa_id<=0) #remove rows where PA ID is not zero
unique(cfm_90m_filter$pa_id) #check if worked - only 0 values returned, worked

#repeat for CFM renewed data

unique(cfm_rnw_90m_data$pa_id) #get unique values of PA IDs, only includes NA values so next steps unnecessary
cfm_rnw_90m_data <- cfm_rnw_90m_data %>%
  mutate(pa_id = replace(as.numeric(pa_id), which(is.na(pa_id)), 0)) #replace NA values with 0
unique(cfm_rnw_90m_data$pa_id) #check if worked - no NA values, worked

cfm_rnw_90m_filter <- cfm_rnw_90m_data %>%
  filter(cfm_rnw_90m_data$pa_id<=0) #remove rows where PA ID is not zero
unique(cfm_rnw_90m_filter$pa_id) #check if worked - only 0 values returned, worked

#repeat for PA data

#PA ID = 0 is a real PA, so replace with 99
unique(pa_90m_data$pa_id_corrected) #contains 0s, replace with 99
pa_90m_data <- pa_90m_data %>%
  mutate(pa_id_corrected = replace(as.numeric(pa_id_corrected), pa_id_corrected==0, 99))
unique(pa_90m_data$pa_id_corrected) 

unique(pa_90m_data$cfm_id) #get unique values of CFM IDs - includes NA, we want NA to be 0
pa_90m_data <- pa_90m_data %>% 
  mutate(cfm_id = replace(as.numeric(cfm_id), which(is.na(cfm_id)), 0))
unique(pa_90m_data$cfm_id) 

pa_90m_filter <- pa_90m_data %>%
  filter(cfm_id<=0) #remove rows where PA ID is not NA
unique(pa_90m_filter$cfm_id) #check if worked - only 0 values returned, worked


# add columns for CFM (0 or 1) and PA (0 or 1)

cfm_90m_filter$CFM <- 1
cfm_90m_filter$PA <- 0

cfm_rnw_90m_filter$CFM <- 1
cfm_rnw_90m_filter$PA <- 0

pa_90m_filter$CFM <- 0
pa_90m_filter$PA <- 1

# join tables: CFM and PA

names(cfm_90m_filter)
names(cfm_rnw_90m_filter)
names(pa_90m_filter)  #check if column variables are identical, looks good

cfm_pa_data_90m <- full_join(cfm_90m_filter, pa_90m_filter) #full_join CFM and PA data includes all rows in both
cfm_rnw_pa_data_90m <- full_join(cfm_rnw_90m_filter, pa_90m_filter)  #join renewed CFM and PA data

#write_csv(cfm_pa_data_90m,'outputs/cfm_pa_data_90m_with_na_8Jan2023.csv') #version with NA values

#write_csv(cfm_rnw_pa_data_90m,'outputs/cfm_rnw_pa_data_90m_with_na_8Jan2023.csv') #renewed CFM version with NA values

#replace NA values with 0 in distance from forest edge
#https://sparkbyexamples.com/r-programming/replace-na-values-with-zero-in-r-dataframe/
# Example 12 - Replace NA on multiple columns by Index
# my_dataframe <- my_dataframe %>% 
# mutate_at(c(1,3), ~replace_na(.,0))

#drop un-corrected cfm_id and pa_id columns
cfm_pa_data_90m <- cfm_pa_data_90m %>%
  dplyr::select(!c(cfm_id, pa_id))

cfm_rnw_pa_data_90m <- cfm_rnw_pa_data_90m %>%
  dplyr::select(!c(cfm_id, pa_id))

#replace NA values with 0 in all columns related to: "cfm_id_corrected", "edge", "distance", "pa_id_corrected" 
cfm_pa_data_90m_na_replace <- cfm_pa_data_90m %>%
  mutate_at(c(2,8:10,41:57,172), ~replace_na(.,0)) #double check column numbers!!

cfm_rnw_pa_data_90m_na_replace <- cfm_rnw_pa_data_90m %>%
  mutate_at(c(2,8:10,41:57,172), ~replace_na(.,0)) #double check column numbers!!

#remove sample points with NA values, otherwise matching won't work
cfm_pa_data_90m_no_na <- drop_na(cfm_pa_data_90m_na_replace) #lose about 300 observations
cfm_rnw_pa_data_90m_no_na <- drop_na(cfm_rnw_pa_data_90m_na_replace) #lose about 300 observations

#write to CSV
write_csv(cfm_pa_data_90m_no_na,'outputs/cfm_pa_data_90m_no_na_8Jan2023.csv') #update date, all CFM sample points
write_csv(cfm_rnw_pa_data_90m_no_na,'outputs/cfm_rnw_pa_data_90m_no_na_8Jan2023.csv') #update date, renewed CFM


# Matching using MatchIt Package ------------------------------------------
#https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf

#load data if necessary
cfm_pa_data_90m_no_na <- read_csv('outputs/cfm_pa_data_90m_no_na_8Jan2023.csv') #update date
cfm_rnw_pa_data_90m_no_na <- read_csv('outputs/cfm_rnw_pa_data_90m_no_na_8Jan2023.csv') #update date

#check mean urban distance before matching
names(cfm_pa_data_90m_no_na)
cfm_pa_urbandist_summary <- cfm_pa_data_90m_no_na %>%
  group_by(CFM) %>%
  summarize (mean_urban_dist = mean(dist_urb), stdev_urban_dist = sd(dist_urb)) #CFM: 63,237.50 (63.2 km) PA: 59,437.37 (59.4 km)

##  1:1 NN Propensity Score matching with replacement and exact matching on veg_type --------

m.out1 <- matchit(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + edge_05 + elev + pop2005 + rain + rice + slope + veg_type,
                  data = cfm_pa_data_90m_no_na, replace = TRUE, exact = ~veg_type)
m.out1
summary(m.out1)
saveRDS(m.out1, file = "outputs/m.out1.rds") #save as an RDS file so you can reload it if needed

#generate matched dataset
propensity.score.matches <- get_matches(m.out1, data = cfm_pa_data_90m_no_na,
                                        distance = "mahalanobis")
#write to CSV
write_csv(propensity.score.matches,'outputs/propensity.score.matches_27Jan2023.csv') #update date, all CFM sample points


## 1:1 NN Mahalanobis distance matching w/ replacement and exact matching on veg_type --------
# **used this for analysis

m.out2 <- matchit(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + edge_05 + elev + pop2005 + rain + rice + slope + veg_type,
                  data = cfm_pa_data_90m_no_na,
                  distance = "mahalanobis", replace = TRUE,
                  exact = ~ veg_type)

m.out2
summary(m.out2, un = TRUE)
saveRDS(m.out2, file = "outputs/m.out2.rds") #save as an RDS file so you can reload it if needed

#generate matched dataset
mahalanobis.matches <- get_matches(m.out2, data = cfm_pa_data_90m_no_na,
                                   distance = "mahalanobis")
#write to CSV
write_csv(mahalanobis.matches,'outputs/mahalanobis.matches_26Feb2023.csv') #update date, all CFM sample points


## 1:1 genetic matching with replacement and exact matching on veg_type  ---------------
# (NOTE TAKES 19 hours to >2 days)

library(rgenoud)
library(snow)

library(parallel)
detectCores() #my computer has 16 cores

cl <- c("localhost","localhost","localhost","localhost","localhost","localhost","localhost","localhost",
        "localhost","localhost","localhost","localhost","localhost","localhost","localhost","localhost") #to parallelize

startTime <- Sys.time()
m.out3 <- matchit(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + edge_05 + elev + pop2005 + rain + rice + slope + veg_type,
                  data = cfm_pa_data_90m_no_na,
                  method = "genetic", replace = TRUE,
                  exact = ~ veg_type,
                  pop.size = 500,
                  cluster = cl, balance = F)
endTime <- Sys.time()
print(endTime - startTime)  # 2.3 days without parallel processing, 19.4 hours with 16 cores

m.out3
summary(m.out3, un=TRUE)
saveRDS(m.out3, file = "outputs/m.out3.rds") #save as an RDS file so you can reload it if needed

#generate matched dataset
genetic.matches <- get_matches(m.out3, data = cfm_pa_data_90m_no_na, distance = "mahalanobis")

#write to CSV
write_csv(genetic.matches,'outputs/genetic.matches_26Feb2023.csv') #update date, all CFM sample points

#load matched object (genetic matching)
m.out3 <- readRDS("outputs/m.out3.rds")
m.out3
summary(m.out3)

## plot match balance --------------------------
#https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html

library(cobalt)

v <- data.frame(old = c("dist_cart", "dist_road", "dist_urb", "dist_vil", 
                        "edge_05", "elev", "pop2005", "rain", "rice", "slope", "veg_type"),
                new = c("Distance from cart track", "Distance from road", "Distance to urban center", "Distance to village", "Forest edge distance 2005", "Elevation", "Population 2005", "Average annual rainfall", "Suitability for rice", "Slope", "Vegetation type"))

# Note on thresholds: "Stuart, Lee, and Leacy (2013) found that a threshold of .1 was more effective at assessing imbalance that would lead to biased effect estimation." https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html

# Plot match balance: 1:1 NN PS matching with replacement and exact matching on veg_type
love.plot(m.out1, 
          binary = "std", 
          drop.distance = TRUE, #to avoid confusing "distance" measure
          thresholds = c(m = .1),
          sample.names = c("Unmatched", "Matched"),
          var.names = v,
          title = "Covariate Balance, All CFM, 90m data, propensity score matching",
          shapes = c("circle", "triangle"),
          colors = c("coral2", "cyan3"))

# 1:1 NN Mahalanobis distance matching w/ replacement and exact matching on veg_type
love.plot(m.out2, 
          binary = "std",
          drop.distance = TRUE, 
          thresholds = c(m = .1),
          sample.names = c("Unmatched", "Matched"),
          var.names = v,
          title = "Covariate Balance, All CFM, 90m data, Mahalanobis distance matching",
          shapes = c("circle", "triangle"),
          colors = c("coral2", "cyan3"))

# 1:1 genetic matching with replacement and exact matching on veg_type
love.plot(m.out3, 
          binary = "std", 
          drop.distance = TRUE, #to avoid confusing "distance" measure
          thresholds = c(m = .1),
          sample.names = c("Unmatched", "Matched"),
          var.names = v,
          title = "Covariate Balance, All CFM, 90m data, genetic matching",
          shapes = c("circle", "triangle"),
          colors = c("coral2", "cyan3"))


# Check for pseudoreplication in matched data -------------------------
#following example in https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
#matched <- read_csv('outputs/g.matches_8Jan2023.csv')  #load data, update date #Mahalanobis matched data
matched <- read_csv('outputs/genetic.matches_26Feb2023.csv') #Genetic matched data
matched_pa <- matched %>% filter(pa_id_corrected != 0) #selects only PA rows
matched_pa <- matched_pa %>% dplyr::select(!c(id, subclass, ID)) #removing columns with unique values
matched_pa_unique <- unique(matched_pa) # 4,392 unique PA comparison points, which means we have pseudoreplication


# Reorganize Matched Data for Event Study analysis -----------------------
library(tidyverse)

names(genetic.matches)

#load tabular data if needed
#matched_yr <- read_csv('outputs/genetic_matched_points_forest_9Feb2023.csv') #genetic matched data with forest cover added in QGIS
matched_yr <- read_csv('outputs/genetic.matches_26Feb2023.csv') #genetic matched data without forest cover
names(matched_yr)

#subset data to split up PA and CFM data

#for PA data:
PA_data_yr <- matched_yr %>%
  filter(CFM==0)

#for CFM data:

CFM_data_yr <- matched_yr %>%
  filter(CFM==1)

#add unique ID columns to each subset:
PA_data_yr$ID <- seq.int(nrow(PA_data_yr))
CFM_data_yr$ID <- seq.int(nrow(CFM_data_yr))

#add new variable PA_CFM
PA_data_yr$PA_CFM <- "PA"
CFM_data_yr$PA_CFM <- "CFM"

#create character strings for new unique IDs
PA_data_yr$UID <- do.call(paste0, PA_data_yr[c("PA_CFM", "ID")])
CFM_data_yr$UID <- do.call(paste0, CFM_data_yr[c("PA_CFM", "ID")])

#write to CSV (we'll need this later for Rosenbaum bounds sensitivity analysis)
write_csv(PA_data_yr,'outputs/PA_data_yr_90m_genetic_27Feb2023.csv') #update date, genetic matched data
write_csv(CFM_data_yr,'outputs/CFM_data_yr_90m_genetic_27Feb2023.csv') #update date, genetic matched data

#join the tables back together
matched_yr_bind <- rbind(PA_data_yr, CFM_data_yr)

#drop defor and forest years that we're not including (2001-2004, 2021) and distance2000 #updated
names(matched_yr_bind)
matched_yr_bind <- matched_yr_bind %>% dplyr::select(-c(defor2001, defor2002, defor2003, defor2004, defor2021, distance_2000))
#, forest2001:forest2004, forest2021)) #no longer including forest cover as an outcome variable
names(matched_yr_bind)

#rename rice price data to match other time variant variable names
matched_yr_bind <- matched_yr_bind %>%
  rename_with(~ gsub('_', '', .x)) #removes underscores from rice_av_ and rice_sd_
names(matched_yr_bind)

#reorganize columns to group all time variant variables together
matched_yr_bind <- matched_yr_bind %>%
  select(id:y,CFM:mahalanobis,PACFM,UID,defor2005:defor2020,distance2005:wind2020) #forest2005:forest2020,
names(matched_yr_bind)

# reorganize based on https://dcl-wrangle.stanford.edu/pivot-advanced.html
#this works but the time variant variables are all in a single column, "timevariant"

matched_yr_longer <- matched_yr_bind %>%
  pivot_longer(
    cols = defor2005:wind2020,
    names_to = c("timevariant", "year"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "timevariant_values"
  )

#now pivot wider to get time variant variables as columns instead of rows
matched_yr_wider <- matched_yr_longer %>%
  pivot_wider(
    names_from = "timevariant",
    values_from = "timevariant_values"
  )

#write to CSV
write_csv(matched_yr_wider,'outputs/matched_yr_wider_90m_genetic_27Feb2023.csv') #update date!


### Convert rice prices to Malagasy Ariary instead of USD --------------------------

#load matched data if needed
matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_genetic_27Feb2023.csv') #update date!

names(matched_yr_wider)

MDG_exchange <- read_csv("data/MDG_exchange_rates.csv")

matched_yr_wider <- matched_yr_wider %>% mutate(year = as.double(year)) #convert year to numeric format

matched_yr_wider <- left_join(matched_yr_wider, MDG_exchange, by = c("year" = "Year")) %>%
  mutate(riceav_mdg = riceav*Exchange_rate) %>%
  mutate(ricesd_mdg = ricesd*Exchange_rate)

### Add new variables for event study design: "timestep" and "years since crisis" --------------------

#"timestep": takes value 0 = first year in dataset (2005), 1 = second year, etc. through 2017
#"years post crisis" = takes values 0 for all years 2005-2009, then 2010 = 1, 2011 = 2, 2012 = 3, etc?

matched_yr_wider <- matched_yr_wider %>%
  mutate(timestep = year - 2005) #year-2005 = 0 for 2005, 1 for 2006, etc.

matched_yr_wider <- matched_yr_wider %>%
  mutate(yrs_post_crisis = case_when(year<2010 ~ 0,
                                     year==2010 ~ 1,
                                     year==2011 ~ 2,
                                     year==2012 ~ 3,
                                     year==2013 ~ 4,
                                     year==2014 ~ 5,
                                     year==2015 ~ 6,
                                     year==2016 ~ 7,
                                     year==2017 ~ 8,
                                     year==2018 ~ 9,
                                     year==2019 ~ 10,
                                     year==2020 ~ 11))

### Add a new alphanumeric variable: "cluster" with the format cfm00pa00 ---------------------
names(matched_yr_wider)
matched_yr_wider$cluster <- do.call(paste0, c("cfm", matched_yr_wider["cfmidcorrected"], "pa", matched_yr_wider["paidcorrected"]))
matched_yr_wider$cluster <- as.factor(matched_yr_wider$cluster) #convert "cluster" to a factor
matched_yr_wider$year <- as.factor(matched_yr_wider$year) #convert "year" to a factor
#view(matched_yr_wider)

### Add a new variable "Renewed" for renewed CFM --------------------------
renewed <- read_csv("data/CFM_pre05_renewed.csv") #load CFM data
names(renewed) #note ID is the correct CFM id
renewed %>% 
  group_by(Renewal) %>%
  tally() #count how many CFM are renewed

renewed_simple <- renewed %>%
  select(ID, RENEWED) #create a simplified version of the table

matched_yr_wider <- left_join(matched_yr_wider, renewed_simple, by = c("cfmidcorrected"="ID")) #join

unique(matched_yr_wider$RENEWED) #includes NA
matched_yr_wider <- matched_yr_wider %>% 
  mutate(RENEWED = replace(as.numeric(RENEWED), which(is.na(RENEWED)), 0))  #replace NA values with 0
unique(matched_yr_wider$RENEWED)

#write to CSV
write_csv(matched_yr_wider,'outputs/matched_yr_wider_90m_genetic_27Feb2023.csv') #update date


## Calculate ANNUAL defor (not CUMULATIVE defor) in each year -------------------
matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_genetic_27Feb2023.csv') #update date
names(matched_yr_wider)

#test on a subset
matched_subset <- matched_yr_wider %>%
  select(UID,year, defor)

matched_defor_annual <- matched_subset %>%
  mutate(defor_lag = case_when(year==2005 ~ defor,
                               year > 2005 ~ lag(defor)))

matched_defor_annual <- matched_defor_annual %>%
  mutate(defor_annual = case_when(year==2005 ~ defor,
                                  year > 2005 ~ defor-defor_lag)) #worked!

#apply to full dataset
matched_yr_wider <- matched_yr_wider %>%
  mutate(defor_lag = case_when(year==2005 ~ defor,
                               year > 2005 ~ lag(defor)))

matched_yr_wider <- matched_yr_wider %>%
  mutate(defor_annual = case_when(year==2005 ~ defor,
                                  year > 2005 ~ defor-defor_lag))

#write to CSV
write_csv(matched_yr_wider,'outputs/matched_yr_wider_90m_genetic_defor_annual_27Feb2023.csv') #update date

## Include variables for development, security ----------------------

names(matched_yr_wider)

#histogram of each
ggplot(matched_yr_wider, aes(x=q1materials)) + geom_histogram(aes(color=PACFM, fill=PACFM), position = "dodge")
ggplot(matched_yr_wider, aes(x=v7security)) + geom_histogram(aes(color=PACFM, fill=PACFM), position = "dodge")

#check if variables are correlated - answer is not really
#library(corrr)
#cor(matched_yr_wider$q1materials, matched_yr_wider$v7security, method = "spearman", use = "pairwise.complete.obs") #0.14

# Modify q1materials and v7security to be binary (0/1) instead of categorical (0-4 or 0-5)

unique(matched_yr_wider$q1materials) #takes values 0 through 5
#q1materials: values 0 to 5 represent quintiles
q1materials_count <- matched_yr_wider %>% count(q1materials) #create a table with counts
#calculate median value for CFM and MNP
matched_yr_wider %>%
  group_by(PACFM) %>%
  summarize(median_q1materials = median(q1materials)) #median is 2 for both CFM and MNP

matched_yr_wider <- matched_yr_wider %>%
  mutate(development = ifelse(q1materials > 2, "1","0")) #used median value as threshold

unique(matched_yr_wider$v7security) #takes values 0 through 4
#calculate median value for CFM and MNP
matched_yr_wider %>%
  group_by(PACFM) %>%
  summarize(median_v7security = median(v7security)) #median is 2 CFM and 3 for MNP
#v7security: Values from 0 to 4 represent percentages
matched_yr_wider <- matched_yr_wider %>%
  mutate(security = ifelse(v7security > 2, "1","0")) #used median value of CFM as threshold

names(matched_yr_wider) #includes development, security
write_csv(matched_yr_wider,'outputs/matched_yr_wider_90m_genetic_defor_annual_development_security_4Mar2023.csv') #update date

#check mean urban distance for CFM, MNP pixels after matching
matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_genetic_defor_annual_development_security_4Mar2023.csv') #update date

matched_cfm_pa_urbandist_summary <- matched_yr_wider %>%
  group_by(CFM) %>%
  summarize(mean_urban_dist = mean(disturb), stdev_urban_dist = sd(disturb))
