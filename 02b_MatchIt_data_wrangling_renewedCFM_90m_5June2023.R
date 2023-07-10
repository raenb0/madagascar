# Madagascar impact evaluation
# Matching and data wrangling, Renewed CFM data, 90m resolution
# June 5 2023


# REPEAT WITH RENEWED CFM DATA --------------------------------------------
library(tidyverse)
library(MatchIt)

# Repeat matching (MatchIt Package) for Renewed CFM data ------------
#load data
cfm_rnw_pa_data_90m_no_na <- read_csv('outputs/cfm_rnw_pa_data_90m_no_na_8Jan2023.csv') #update date

##  1:1 NN Propensity Score matching with replacement and exact matching on veg_type --------

m.out1.rnw <- matchit(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + edge_05 + elev + pop2005 + rain + rice + slope + veg_type,
                      data = cfm_rnw_pa_data_90m_no_na, replace = TRUE, exact = ~veg_type)
m.out1.rnw
summary(m.out1.rnw)
saveRDS(m.out1.rnw, file = "outputs/m.out1.rnw.rds") #save as an RDS file so you can reload it if needed

#generate matched dataset
propensity.score.matches.rnw <- get_matches(m.out1.rnw, data = cfm_rnw_pa_data_90m_no_na,
                                            distance = "mahalanobis")
#write to CSV
write_csv(propensity.score.matches.rnw,'outputs/propensity.score.matches.rnw_3Feb2023.csv') #update date


## 1:1 NN Mahalanobis distance matching w/ replacement and exact matching on veg_type --------
# **used this for analysis

m.out2.rnw <- matchit(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + edge_05 + elev + pop2005 + rain + rice + slope + veg_type,
                      data = cfm_rnw_pa_data_90m_no_na,
                      distance = "mahalanobis", replace = TRUE,
                      exact = ~ veg_type)

m.out2.rnw
summary(m.out2.rnw, un = TRUE)
saveRDS(m.out2.rnw, file = "outputs/m.out2.rnw.rds") #save it as an RDS file

#generate matched dataset
mahalanobis.matches.rnw <- get_matches(m.out2.rnw, data = cfm_rnw_pa_data_90m_no_na,
                                       distance = "mahalanobis")
#write to CSV
write_csv(mahalanobis.matches.rnw,'outputs/mahalanobis.matches.rnw_3Feb2023.csv') #update date, all CFM sample points


## 1:1 genetic matching with replacement and exact matching on veg_type  ---------------
# (NOTE TAKES 19 HOURS)
library(MatchIt)
library(rgenoud)
library(snow)

library(parallel)
detectCores() #my computer has 16 cores

cl <- c("localhost","localhost","localhost","localhost","localhost","localhost","localhost","localhost") #to parallelize

startTime <- Sys.time() #to record time required to run this
m.out4.rnw <- matchit(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + edge_05 + elev + pop2005 + rain + rice + slope + veg_type,
                      data = cfm_rnw_pa_data_90m_no_na,
                      method = "genetic", replace = TRUE,
                      exact = ~ veg_type,
                      pop.size = 500, #documentation says "use much larger pop.size" than 10 but doesn't say how large?
                      cluster=cl, balance=F) 
endTime <- Sys.time()
print(endTime - startTime)  #19 hours with 8 cores

m.out4.rnw
summary(m.out4.rnw, un=TRUE)
saveRDS(m.out4.rnw, file = "outputs/m.out4.rnw.rds") #save it as an RDS file

#generate matched dataset
genetic.matches.rnw <- get_matches(m.out4.rnw, data = cfm_rnw_pa_data_90m_no_na, distance = "mahalanobis")

#write to CSV
write_csv(genetic.matches.rnw,'outputs/genetic.matches.rnw_pop_size_500_12Feb2023.csv') #update date, all CFM sample points


# check for pseudoreplication in matched data (renewed CFM only) -------------------------
matched_rnw <- read_csv('outputs/genetic.matches.rnw_pop_size_500_12Feb2023.csv')  #load data, update date
matched_pa <- matched_rnw %>% filter(pa_id_corrected != 0) #selects only PA rows
matched_pa <- matched_pa %>% dplyr::select(!c(id, subclass, ID)) #removing columns with unique values
matched_pa_unique <- unique(matched_pa) # 3,155 unique PA comparison points, which means we have pseudoreplication


# plot match balance, renewed CFM data ------------------------
library(cobalt)

v <- data.frame(old = c("dist_cart", "dist_road", "dist_urb", "dist_vil",
                        "edge_05", "elev", "pop2005", "rain", "rice", "slope", "veg_type"),
                new = c("Distance from cart track", "Distance from road", "Distance to urban center",
                        "Distance to village", "Forest edge distance 2005", "Elevation", "Population 2005",
                        "Average annual rainfall", "Suitability for rice", "Slope", "Vegetation type"))

# Plot match balance: 1:1 NN Propensity Score matching with replacement and exact matching on veg_type
#m.out1.rnw <- readRDS("outputs/m.out1.rnw.rds") #load RDS file if needed
love.plot(m.out1.rnw,
          binary = "std",
          drop.distance = TRUE, #to avoid confusing "distance" measure
          thresholds = c(m = .1),
          sample.names = c("Unmatched", "Matched"),
          var.names = v,
          title = "Covariate Balance, Renewed CFM, 90m data, propensity score matching",
          shapes = c("circle", "triangle"),
          colors = c("coral2", "cyan3"))

## Plot match balance: 1:1 NN Mahalanobis distance matching w/ replacement and exact matching on veg_type
love.plot(m.out3.rnw,
          binary = "std",
          drop.distance = TRUE,
          thresholds = c(m = .1),
          sample.names = c("Unmatched", "Matched"),
          var.names = v,
          title = "Covariate Balance, Renewed CFM, 90m data, Mahalanobis distance matching",
          shapes = c("circle", "triangle"),
          colors = c("coral2", "cyan3"))

# Plot match balance: 1:1 genetic matching with replacement and exact matching on veg_type
#m.out4.rnw <- readRDS("outputs/m.out4.rnw.rds") #load RDS file if needed
love.plot(m.out4.rnw,
          binary = "std",
          drop.distance = TRUE,
          thresholds = c(m = .1),
          sample.names = c("Unmatched", "Matched"),
          var.names = v,
          title = "Covariate Balance, Renewed CFM, 90m data, genetic matching",
          shapes = c("circle", "triangle"),
          colors = c("coral2", "cyan3"))


## Reorganize matched data for event study analysis, renewed CFM data -----------------------

library(tidyverse)

#load tabular data if needed
matched_yr_rnw <- read_csv('outputs/genetic_matched_points_rnw_forest_13Feb2023.csv')  #update dates

names(matched_yr_rnw)

#subset data to split up PA and CFM data

#for PA data:
PA_data_yr_rnw <- matched_yr_rnw %>%
  filter(CFM==0)

#for CFM data:

CFM_data_yr_rnw <- matched_yr_rnw %>%
  filter(CFM==1)

#add unique ID columns to each subset:
PA_data_yr_rnw$ID <- seq.int(nrow(PA_data_yr_rnw))
CFM_data_yr_rnw$ID <- seq.int(nrow(CFM_data_yr_rnw))

#add new variable PA_CFM
PA_data_yr_rnw$PA_CFM <- "PA"
CFM_data_yr_rnw$PA_CFM <- "CFM"

#create character strings for new unique IDs
PA_data_yr_rnw$UID <- do.call(paste0, PA_data_yr_rnw[c("PA_CFM", "ID")])
CFM_data_yr_rnw$UID <- do.call(paste0, CFM_data_yr_rnw[c("PA_CFM", "ID")])

#write to CSV (we'll need this later for Rosenbaum bounds sensitivity analysis)
write_csv(PA_data_yr_rnw,'outputs/PA_data_yr_rnw_90m_13Feb2023.csv') #update date
write_csv(CFM_data_yr_rnw,'outputs/CFM_data_yr_rnw_90m_13Feb2023.csv') #update date

#join the tables back together
matched_yr_rnw_bind <- rbind(PA_data_yr_rnw, CFM_data_yr_rnw)

#drop defor years that we're not including (2001-2004, 2021) and distance2000
names(matched_yr_rnw_bind)
matched_yr_rnw_bind <- matched_yr_rnw_bind %>% dplyr::select(-c(defor2001, defor2002, defor2003, defor2004, defor2021, distance_2000, forest2001:forest2004, forest2021))
names(matched_yr_rnw_bind)

#rename rice price data to match other time variant variable names
matched_yr_rnw_bind <- matched_yr_rnw_bind %>%
  rename_with(~ gsub('_', '', .x)) #removes underscores from rice_av_ and rice_sd_
names(matched_yr_rnw_bind)

#reorganize columns to group all time variant variables together
matched_yr_rnw_bind <- matched_yr_rnw_bind %>%
  select(id:y,CFM:mahalanobis,PACFM,UID,defor2005:defor2020,forest2005:forest2020,distance2005:wind2020)
names(matched_yr_rnw_bind)

# reorganize based on https://dcl-wrangle.stanford.edu/pivot-advanced.html
#this works but the time variant variables are all in a single column, "timevariant"

matched_yr_rnw_longer <- matched_yr_rnw_bind %>%
  pivot_longer(
    cols = defor2005:wind2020,
    names_to = c("timevariant", "year"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "timevariant_values"
  )

#now pivot wider to get time variant variables as columns instead of rows
matched_yr_rnw_wider <- matched_yr_rnw_longer %>%
  pivot_wider(
    names_from = "timevariant",
    values_from = "timevariant_values"
  )

#View
View(matched_yr_rnw_wider)

#write to CSV
write_csv(matched_yr_rnw_wider,'outputs/matched_yr_rnw_wider_genetic_forest_13Feb2023.csv') #update date!


### convert rice prices to Malagasy Ariary instead of USD ------------------------

#load data if needed
matched_yr_rnw_wider <- read_csv('outputs/matched_yr_rnw_wider_genetic_forest_13Feb2023.csv') #update date!

MDG_exchange <- read_csv("data/MDG_exchange_rates.csv")

matched_yr_rnw_wider <- matched_yr_rnw_wider %>% mutate(year = as.double(year)) #convert year to numeric format

matched_yr_rnw_wider <- left_join(matched_yr_rnw_wider, MDG_exchange, by = c("year" = "Year")) %>%
  mutate(riceav_mdg = riceav*Exchange_rate) %>%
  mutate(ricesd_mdg = ricesd*Exchange_rate)

# ### Modify q1materials and v7security to be binary (0/1) instead of categorical (0-4 or 0-5) ---------------------
# 
# unique(matched_yr_rnw_wider$q1materials) #takes values 1 through 5 #note no 0 values in this dataset
# #q1materials: values 0 to 5 represent quintiles
# matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
#   mutate(development = ifelse(q1materials > 1, "1","0")) #tried different thresholds, >3/>2/>1
# 
# unique(matched_yr_rnw_wider$v7security) #takes values 0 through 4
# #v7security: Values from 0 to 4 represent percentages
# matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
#   mutate(security = ifelse(v7security > 1, "1","0")) #tried different thresholds, >2/>1, significant if >1

### add new variables for event study design: "timestep" and "years since crisis" ----------------------------
#"timestep": takes value 0 = first year in dataset (2005), 1 = second year, etc. through 2017
#"years post crisis" = takes values 0 for all years 2005-2009, then 2010 = 1, 2011 = 2, 2012 = 3, etc?

matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
  mutate(timestep = year - 2005) #year-2005 = 0 for 2005, 1 for 2006, etc.

matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
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

### add a new alphanumeric variable: "cluster" with the format cfm00pa00 ----------------------------------
names(matched_yr_rnw_wider)
matched_yr_rnw_wider$cluster <- do.call(paste0, c("cfm", matched_yr_rnw_wider["cfmidcorrected"], "pa", matched_yr_rnw_wider["paidcorrected"]))
matched_yr_rnw_wider$cluster <- as.factor(matched_yr_rnw_wider$cluster) #convert "cluster" to a factor
matched_yr_rnw_wider$year <- as.factor(matched_yr_rnw_wider$year) #convert "year" to a factor
#view(matched_yr_rnw_wider)

#write to CSV
write_csv(matched_yr_rnw_wider,'outputs/matched_yr_rnw_wider_genetic_forest_13Feb2023.csv') #update date


# Calculate ANNUAL defor (not CUMULATIVE defor) in each year, renewed CFM -------------------
matched_yr_rnw_wider <- read_csv('outputs/matched_yr_rnw_wider_genetic_forest_13Feb2023.csv') #update date
names(matched_yr_rnw_wider)

#test on a subset
matched_subset <- matched_yr_rnw_wider %>%
  select(UID,year, defor)

matched_defor_annual <- matched_subset %>%
  mutate(defor_lag = case_when(year==2005 ~ defor,
                               year > 2005 ~ lag(defor)))

matched_defor_annual <- matched_defor_annual %>%
  mutate(defor_annual = case_when(year==2005 ~ defor,
                                  year > 2005 ~ defor-defor_lag)) #worked!

#apply to full dataset
matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
  mutate(defor_lag = case_when(year==2005 ~ defor,
                               year > 2005 ~ lag(defor)))

matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
  mutate(defor_annual = case_when(year==2005 ~ defor,
                                  year > 2005 ~ defor-defor_lag))

# Modify q1materials and v7security to be binary (0/1) instead of categorical (0-4 or 0-5) ----------

unique(matched_yr_rnw_wider$q1materials) #takes values 1 through 5
#q1materials: values 0 to 5 represent quintiles
q1materials_count <- matched_yr_rnw_wider %>% count(q1materials) #create a table with counts, left skew

matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
  mutate(development = ifelse(q1materials > 3, "1","0")) #tried different thresholds, >4,>3/>2/>1

unique(matched_yr_rnw_wider$v7security) #takes values 0 through 4
#v7security: Values from 0 to 4 represent percentages
matched_yr_rnw_wider <- matched_yr_rnw_wider %>%
  mutate(security = ifelse(v7security > 2, "1","0")) #tried different thresholds, >3/>2/>1

#write to CSV
write_csv(matched_yr_rnw_wider,'outputs/matched_yr_rnw_wider_90m_genetic_defor_annual_development_security_2Mar2023.csv') #update date

# Plot deforestation in matched CFM, PA pixels -----------------
matched_yr_rnw_wider <- read_csv('outputs/matched_yr_rnw_wider_90m_genetic_defor_annual_development_security_2Mar2023.csv') #update date

defor_rnw_plot1 <- matched_yr_rnw_wider %>%
  group_by(year, PACFM) %>%
  summarise(mean_defor = mean(defor_annual), sd_defor = sd(defor_annual)) %>%
  ggplot(mapping = aes(x=year, y=mean_defor, colour=PACFM, fill=PACFM)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=mean_defor-sd_defor, ymax=mean_defor+sd_defor), alpha=0.3) +
  #  scale_colour_manual(values=c("#1E88E5", "#FFC107"))  +
  labs(
    x = "Year",
    y = "Mean deforestation (percent)",
    title = "Mean annual deforestation in matched CFM and MNP samples, renewed CFM") +
  theme_minimal()

defor_rnw_plot1

defor_rnw_plot2 <- matched_yr_rnw_wider %>%
  group_by(year, PACFM) %>%
  summarise(mean_defor = mean(defor_annual), sd_defor = sd(defor_annual)) %>%
  ggplot(mapping = aes(x=year, y=mean_defor, colour=PACFM, fill=PACFM)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean_defor-sd_defor, ymax=mean_defor+sd_defor),position = "dodge", width = 0.2) +
  scale_colour_manual(values=c("#1E88E5", "#FFC107"))  +
  labs(
    x = "Year",
    y = "Mean deforestation (percent)",
    title = "Mean annual deforestation in matched CFM and MNP samples, renewed CFM") +
  theme_minimal()

defor_rnw_plot2