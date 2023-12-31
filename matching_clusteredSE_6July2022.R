library(tidyverse)
library(dplyr)
library(Matching)
library(tidyr)


# Load data ---------------------------------------------------------------

setwd("C:/Users/raenb/Documents/GitHub/madagascar") # set working directory

# read in 90m data

cfm_90m_data <- read_csv("data/sample_points/cfm05_90m_data.csv") #note all columns read in as double, but some are categorical e.g. veg type can be 1, 2, 3**
cfm_rnw_90m_data <- read_csv("data/sample_points/cfm05_rnw_90m_data.csv") #subset of CFM that were renewed
pa_90m_data <- read_csv("data/sample_points/pa05_90m_data.csv")

#change data types for categorical columns - is this necessary?
cfm_90m_data <- cfm_90m_data %>% mutate(cfmid = as.integer(cfmid),
                         paid = as.integer(paid),
                         rice = as.integer(rice),
                         vegtype = as.integer(vegtype))

cfm_rnw_90m_data <- cfm_rnw_90m_data %>% mutate(cfmid = as.integer(cfmid),
                                        paid = as.integer(paid),
                                        rice = as.integer(rice),
                                        vegtype = as.integer(vegtype))

pa_90m_data <- pa_90m_data %>% mutate(cfmid = as.integer(cfmid),
                                        paid = as.integer(paid),
                                        rice = as.integer(rice),
                                        vegtype = as.integer(vegtype))

# read in 300m data

#cfm_300m_data <- read_csv("data/sample_points/cfm_for00_data_300m.csv") 
#pa_300m_data <- read_csv("data/sample_points/pa_for00_data_300m.csv")


# working with 90m data to start


# Data wrangling --------------------------------------------------------

# Rename variables

names(cfm_90m_data) #get variable names

cfm_90m_data <- rename(cfm_90m_data,
                       cfm_id = cfmid,
                       dist_cart = distcart,
                       dist_road = distroad,
                       dist_urb = disturb,
                       dist_vil = distvil,
                       # elev = elevmsk,
                       # for2000 = for200090m,
                       # for2001 = for200190m,
                       # for2002 = for200290m,
                       # for2003 = for200390m,
                       # for2004 = for200490m,
                       # for2005 = for200590m,
                       # for2006 = for200690m,
                       # for2007 = for200790m,
                       # for2008 = for200890m,
                       # for2009 = for200990m,
                       # for2010 = for201090m,
                       # for2011 = for201190m,
                       # for2012 = for201290m,
                       # for2013 = for201390m,
                       # for2014 = for201490m,
                       # for2015 = for201590m,
                       # for2016 = for201690m,
                       # for2017 = for201790m,
                       pa_id = paid,
                       # precip = precyr,
                       # pop05 = mdgpd2005,
                       # pop06 = mdgpd2006,
                       # pop07 = mdgpd2007,
                       # pop08 = mdgpd2008,
                       # pop09 = mdgpd2009,
                       # pop10 = mdgpd2010,
                       # pop11 = mdgpd2011,
                       # pop12 = mdgpd2012,
                       # pop13 = mdgpd2013,
                       # pop14 = mdgpd2014,
                       # pop15 = mdgpd2015,
                       # pop16 = mdgpd2016,
                       # pop17 = mdgpd2017,
                       # rice = ricethr,
                       veg_type = vegtype) 


names(cfm_90m_data) #check if renaming worked

# repeat for PA data

names(pa_90m_data) #get variable names

pa_90m_data <- rename(pa_90m_data,
                      cfm_id = cfmid,
                      dist_cart = distcart,
                      dist_road = distroad,
                      dist_urb = disturb,
                      dist_vil = distvil,
                      # elev = elevmsk,
                      # for2000 = for200090m,
                      # for2001 = for200190m,
                      # for2002 = for200290m,
                      # for2003 = for200390m,
                      # for2004 = for200490m,
                      # for2005 = for200590m,
                      # for2006 = for200690m,
                      # for2007 = for200790m,
                      # for2008 = for200890m,
                      # for2009 = for200990m,
                      # for2010 = for201090m,
                      # for2011 = for201190m,
                      # for2012 = for201290m,
                      # for2013 = for201390m,
                      # for2014 = for201490m,
                      # for2015 = for201590m,
                      # for2016 = for201690m,
                      # for2017 = for201790m,
                      pa_id = paid,
                      # precip = precyr,
                      # pop05 = mdgpd2005,
                      # pop06 = mdgpd2006,
                      # pop07 = mdgpd2007,
                      # pop08 = mdgpd2008,
                      # pop09 = mdgpd2009,
                      # pop10 = mdgpd2010,
                      # pop11 = mdgpd2011,
                      # pop12 = mdgpd2012,
                      # pop13 = mdgpd2013,
                      # pop14 = mdgpd2014,
                      # pop15 = mdgpd2015,
                      # pop16 = mdgpd2016,
                      # pop17 = mdgpd2017,
                      # rice = ricethr,
                      veg_type = vegtype)

names(pa_90m_data) #check if renaming worked


#add new variable to CFM data: "renewed" (0 or 1) to indicate subset of CFM areas that were renewed ("implemented")

#load 'renewed' data
cfm_pre05_renewed <- read_csv('data/CFM_pre05_renewed.csv') #attribute table from CFM pre-2005 shp

renewed <- cfm_pre05_renewed %>%
  dplyr::select(ID, RENEWED)

cfm_90m_data <- left_join(cfm_90m_data, renewed, by = c("cfm_id" = "ID"))

pa_90m_data$RENEWED <- 0 #number of columns in PA and CFM datasets must match


# Filter out sample points that are in overlapping PA and CFM areas

unique(cfm_90m_data$pa_id) #get unique values of PA IDs, includes NA
cfm_90m_data <- cfm_90m_data %>% 
  mutate(pa_id = replace(as.numeric(pa_id), which(is.na(pa_id)), 0))
unique(cfm_90m_data$pa_id) #check if worked - no NA values, worked

cfm_90m_filter <- cfm_90m_data %>%
  filter(cfm_90m_data$pa_id<=0) #remove rows where PA ID is not zero
unique(cfm_90m_filter$pa_id) #check if worked - only 0 values returned, worked

#repeat for PA data

unique(pa_90m_data$cfm_id) #get unique values of CFM IDs - includes NA, we want NA to be 0
pa_90m_data <- pa_90m_data %>% 
  mutate(cfm_id = replace(as.numeric(cfm_id), which(is.na(cfm_id)), 0))
unique(pa_90m_data$cfm_id) # WORKED!!
         
pa_90m_filter <- pa_90m_data %>%
  filter(cfm_id<=0) #remove rows where PA ID is not NA
unique(pa_90m_filter$cfm_id) #check if worked - only 0 values returned, worked


# add columns for CFM (0 or 1) and PA (0 or 1)

cfm_90m_filter$CFM <- 1
cfm_90m_filter$PA <- 0

pa_90m_filter$CFM <- 0
pa_90m_filter$PA <- 1

#create version of CFM data with only renewed (implemented) CFM sites
cfm_90m_renewed <- cfm_90m_filter %>%
  filter(cfm_90m_filter$RENEWED==1) #include only rows where RENEWED == 1 #includes only 6,112 observations
unique(cfm_90m_renewed$RENEWED) #check if worked - only 1 values returned, worked

# join tables

names(cfm_90m_filter) #check if column variables are identical, looks good
names(pa_90m_filter)

cfm_pa_data_90m <- full_join(cfm_90m_filter, pa_90m_filter) #full_join includes all rows in x or y

cfm_pa_data_90m_no_na <- drop_na(cfm_pa_data_90m) #remove sample points with NA values #NOTE lose around 10,000 observations

#write to CSV
write_csv(cfm_pa_data_90m_no_na,'outputs/cfm_pa_data_90m_no_na_7July2022.csv') #update date, this includes all CFM sample points

#write_csv(cfm_pa_data_90m,'outputs/cfm_pa_data_90m_17Mar2022.csv') #version with NA values


# Define treatment

Treat <- cfm_pa_data_90m_no_na$CFM

# Define covariates

names(cfm_pa_data_90m_no_na)

cov.names <- c("dist_cart","dist_road","dist_urb","dist_vil","DVSP","edge05","elev","pop05","precip","rice","slope","veg_type") # Names of covariates used to match **NOTE alphabetical order, included population 2005 and distance to forest edge 2005

# Extract the covariates

covs <- cfm_pa_data_90m_no_na[cov.names]

Ex <- c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", 
        "FALSE", "FALSE", "FALSE", "TRUE") # Logical vector to allow EXACT matching to be done for the "veg" variable (i.e., matching operates within each type of vegetation)


### MAHALANOBIS DISTANCE MATCHING ### -------------------------------

## SPECIFYING THE MATCHING

# We do not need to specify the arguments Z and V in the Match function because we are not going to use the estimate from Matching
# Z is needed if we want to do bias adjusment to linearly correct for the remaining imbalance within Matching. Usually Z is the same as X
# Because BiasAdjust = FALSE, no bias adjustment will be performed (so no need to define Z)
# V deals with the homoscedasticity assumption in linear model (t-test is a lineal model). V define the variables for which homoscedasticity-robust variances will be calculated
# What we want is the ATT (Average effect of Treatment on the Treated). So, we leave the argument estimand to its default (i.e., "ATT")
# Weight = 2 specifies Mahalanobis distance matching

startTime <- Sys.time() #to record time required to run this
m1 <-  Match(Tr=Treat, X=covs, M = 1, BiasAdjust=FALSE,  exact=Ex, replace=TRUE, ties=TRUE, Weight=2) # run matching
endTime <- Sys.time()
print(endTime - startTime)


# CHECK COVARIATE BALANCE -------------------------------------------------
#Ranaivo said to replace "covs" with the list of covariates we actually want to use to match on

names(covs)

startTime <- Sys.time() #to record time required to run this
mb1 <- MatchBalance(Treat ~ dist_cart + dist_road + dist_urb + dist_vil + DVSP + edge05 + elev + pop05 + precip + rice + slope + veg_type,
                    match.out = m1, nboots = 500, data=cfm_pa_data_90m_no_na)
endTime <- Sys.time()
print(endTime - startTime) #only takes ~30 seconds



## MATCHED DATASET TO BE USED FOR THE DIFFERENCE IN DIFFERENCE (DID) ANALYSIS 

matched <- rbind(cfm_pa_data_90m_no_na[m1$index.treated,],cfm_pa_data_90m_no_na[m1$index.control,]) # this is the matched dataset
#wght <- c(m1$weights,m1$weights) # weights of the observations in the matched dataset, to be used for post matching analysis (e.g., DID regression)

#write to CSV *update date!

write_csv(matched, 'outputs/mahalanobis_matched_6July2022.csv') #update date
#write.csv(wght,'outputs/mahalanobis_wght_17Mar2022.csv') #note this outputs a table with only values of 1***


## Check match balance using f tests

##Chris said: The matching effectiveness can be tested using a joint balance test (an F- or chi-squared test of the linear probability OLS regression using CFM as a binary dependent variable, regressed on all of the observables you have at baseline (2005) and look at the F-test that the whole set of covariates explains statistically significant variation in CFM status (vs. PA status).

#BEFORE MATCHING:

#load data if needed
cfm_pa_data_90m_no_na <- read_csv('outputs/cfm_pa_data_90m_no_na_6July2022.csv') #update date
matched <- read_csv('outputs/mahalanobis_matched_6July2022.csv')  #update date

Treat <- cfm_pa_data_90m_no_na$CFM

mb2 <- lm(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + DVSP + edge05 + elev + pop05 + precip + rice + slope + veg_type, data = cfm_pa_data_90m_no_na)

summary(mb2) #F-statistic: 500.7 on 12 and 37247 DF,  p-value: < 2.2e-16

#AFTER MATCHING: 

mb3 <- lm(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + DVSP + edge05 + elev + pop05 + precip + rice + slope + veg_type, data = matched)

summary(mb3) #F-statistic: 13.83 on 12 and 14575 DF,  p-value: < 2.2e-16


### GENETIC MATCHING ###  (TAKES >63 HRS TO RUN)-------------------------
# library(rgenoud)
# 
# ## SPECIFYING THE MATCHING
#
# startTime_gen <- Sys.time() #to record time required
# gen1 <- GenMatch(Tr=Treat, X=covs, pop.size= 500, exact= Ex, replace=TRUE, ties= TRUE) #took >63 hrs to run
#endTime_gen <- Sys.time()
#print(endTime_gen - startTime_gen)
#
# mgen1 <- Match(Tr=Treat, X=covs, M = 1, BiasAdjust=FALSE, exact=Ex, replace=TRUE, ties=TRUE, Weight.matrix= gen1)
# 
# ## CHECK COVARIATE BALANCE
# mb2 <- MatchBalance(Treat ~ dist_cart + dist_road + dist_urb + dist_vil + DVSP + edge05 + elev + pop05 + precip + rice + slope + veg_type,
#                     match.out = mgen1, nboots = 500, data=cfm_pa_data_90m_no_na) 
# 
# ## MATCHED DATASET TO BE USED FOR THE DIFFERENCE IN DIFFERENCE (DID) ANALYSIS 
# 
# gen.matched <- rbind(cfm_pa_data_90m_no_na[mgen1$index.treated,],cfm_pa_data_90m_no_na[mgen1$index.control,]) # this is the matched dataset
# gen.wght <- c(mgen1$weights,mgen1$weights)# weights of the observations in the matched dataset, to be used for post matching analysis (e.g., DID regression)
# 
# #WRITE TO CSV
# 
# write.csv(gen.matched,'outputs/genetic_matched.csv')
# write.csv(gen.wght,'outputs/genetic_wght.csv')



# ## REORGANIZE MAHALANOBIS MATCHED DATA FOR TWO-PERIOD ANALYSIS -----------------------

library(tidyverse)
library(dplyr)

#load tabular data if needed

matched <- read_csv('outputs/mahalanobis_matched_6July2022.csv')  #update dates
names(matched)

#add new columns for deforestation
#this will return a 0 if there was no deforestation between 2005 and 2010,
#and 0-1 if there was deforestation from 2005 and 2010
#(reverse order to avoid negative numbers)

matched$defor.1 <- matched$for2005-matched$for2010
matched$defor.2 <- matched$for2010-matched$for2014

#add population variables

matched$pop.1 <- matched$pop05
matched$pop.2 <- matched$pop10

#add rice price variables #do you want a single year, or avg over the previous 5 year period??**ask Chris

matched$riceavg.1 <- matched$riceavg2005
matched$riceavg.2 <- matched$riceavg2010

#add rice volatility variables

matched$ricesd.1 <- matched$ricesd2005
matched$ricesd.2 <- matched$ricesd2010

#add drought variables

matched$drought.1 <- matched$drought2005
matched$drought.2 <- matched$drought2010

#add maxPrecip variables

matched$maxPrecip.1 <- matched$maxPrecip2005
matched$maxPrecip.2 <- matched$maxPrecip2010

#add maxTemp variables

matched$maxTemp.1 <- matched$maxTemp2005
matched$maxTemp.2 <- matched$maxTemp2010

#add wind variables

matched$wind.1 <- matched$wind2005
matched$wind.2 <- matched$wind2010


#add distance to forest edge variables

matched$edge.1 <- matched$edge05
matched$edge.2 <- matched$edge10

#add change in forest density (fragmentation) variables **New addition July 12
#this will return a 0 if there was no change in density % between 2005 and 2010,
#and 0-100 if there was change in density % from 2005 and 2010
# e.g. POSITIVE values indicate DECREASE in density

matched$dens.1 <- matched$fordens2005-matched$fordens2010
matched$dens.2 <- matched$fordens2010-matched$fordens2014

#Select desired variables, reorder columns

names(matched)

matched.subs <- matched %>%
  dplyr::select(CFM, PA, cfm_id, pa_id, dist_cart, dist_road, dist_urb, dist_vil, DVSP, elev, rice, precip, slope, veg_type,  defor.1:dens.2) #grab all new columns at the end

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

# #add new variable to CFM data: "renewed" (0 or 1) to indicate subset of CFM areas that were renewed ("implemented")
# #NOTE this step needs to be performed prior to matching
# 
# cfm_pre05_renewed <- read_csv('data/CFM_pre05_renewed.csv') #attribute table from CFM pre-2005 shp
# renewed <- cfm_pre05_renewed %>%
#   dplyr::select(ID, RENEWED)
# CFM_data <- left_join(CFM_data, renewed, by = c("cfm_id" = "ID"))
# 
# PA_data$RENEWED <- 0 #number of columns must match

#join the tables back together
matched_bind <- rbind(PA_data, CFM_data)
View(matched_bind)

names(matched_bind)

#reorganize  
matched_longer <- matched_bind %>%
  pivot_longer(
    cols = defor.1:dens.2,
    names_to = c("timevariant", "period"),
    names_pattern = "([A-Za-z]+).(\\d+)",
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

#write to CSV
write_csv(matched_wider,'outputs/matched_wider_6July2022.csv') #update date



### SPECIFY THE TWO-PERIOD MODEL, DEFORESTATION OUTCOME -------------------
#outcome variable: DEFORESTATION (so POSITIVE coefficients = MORE deforestation, I think)

library(plm)
library(clubSandwich)

#load data if needed
matched_wider <- read_csv('outputs/matched_wider_6July2022.csv') #update date

names(matched_wider) #edge was strongly correlated with other variables, threw an error, so I removed it

did_m1 <- plm(defor ~ CFM*period + pop + riceavg + ricesd + drought + maxPrecip + maxTemp + wind, 
              data = matched_wider, 
              effect="twoways", 
              model = "within", 
              index = c("UID", "period"))

summary(did_m1) 

#Error when I included "edge" variable
#This results from linearly dependent columns, i.e. strongly correlated variables. Examine the pairwise covariance (or correlation) of your variables to investigate if there are any variables that can potentially be removed. You're looking for covariances (or correlations) >> 0. Alternatively, you can probably automate this variable selection by using a forward stepwise regression.

#Also the model automatically dropped ricesd "because of singularities"


#### CLUSTERED SE -------------------
#code adapted from Ranaivo

#load data if needed
matched_wider <- read_csv('outputs/matched_wider_6July2022.csv') #update date

#add a new alphanumeric variable: "cluster" with the format cfm00pa00
names(matched_wider)
matched_wider$cluster <- do.call(paste0, c("cfm", matched_wider["cfm_id"], "pa", matched_wider["pa_id"]))
view(matched_wider)

#write to CSV
write_csv(matched_wider,'outputs/matched_wider_cluster_6July2022.csv') #update date

memory.limit(size=5000000)

#clustering SE (takes a long time, requires too much memory:
startTime <- Sys.time() #to record time required to run this (40 minutes)
V_CR2 <- vcovCR(did_m1, cluster=matched_wider$cluster, type="CR2") # clustering SE. "clusterID": CFM site or PA identification code in the data #didn't work: Error:  cannot allocate vector of size 1.6 Gb
endTime <- Sys.time()
print(endTime - startTime) #took 40 minutes

#p-values
startTime <- Sys.time()
coef_test(did_m1, vcov=V_CR2, test="Satterthwaite") # p-values, also took a long time to run
endTime <- Sys.time()
print(endTime - startTime)
      
#95% CI
startTime <- Sys.time()
conf_int(did_m1, vcov=V_CR2, test="Satterthwaite") # 95% CI. I personally prefer presenting CI because of all the controversies surrounding p-values
endTime <- Sys.time()
print(endTime - startTime)


# ### SPECIFY THE TWO-PERIOD MODEL, FOREST DENSITY OUTCOME -------------------
# #outcome variable: FOREST DENSITY (so POSITIVE coefficients = MORE intact forest, I think)
# 
# #load data if needed
# matched_wider <- read_csv('outputs/matched_wider_6July2022.csv')
# 
# library(plm)
# 
# did_m2 <- plm(dens ~ CFM*time + pop + edge, data = matched_wider, effect="twoways", model = "within", index = c("UID", "time"))
# 
# summary(did_m2)





## REORGANIZE MAHALANOBIS MATCHED DATA FOR ANNUAL ANALYSIS -----------------------##updated May 2022

library(tidyverse)
library(dplyr)

#load tabular data if needed
matched <- read_csv('outputs/mahalanobis_matched_6May2022.csv')  #update dates
#wght <- read_csv('outputs/mahalanobis_wght_17Mar2022.csv') #unnecessary

#add weights to matched dataset ##unnecessary

w_matched_yr <- data.frame(matched)#, wght)

names(w_matched_yr)

#rename population variables

w_matched_yr$pop2005 <- w_matched_yr$pop05
w_matched_yr$pop2006 <- w_matched_yr$pop06
w_matched_yr$pop2007 <- w_matched_yr$pop07
w_matched_yr$pop2008 <- w_matched_yr$pop08
w_matched_yr$pop2009 <- w_matched_yr$pop09
w_matched_yr$pop2010 <- w_matched_yr$pop10
w_matched_yr$pop2011 <- w_matched_yr$pop11
w_matched_yr$pop2012 <- w_matched_yr$pop12
w_matched_yr$pop2013 <- w_matched_yr$pop13
w_matched_yr$pop2014 <- w_matched_yr$pop14
w_matched_yr$pop2015 <- w_matched_yr$pop15
w_matched_yr$pop2016 <- w_matched_yr$pop16
w_matched_yr$pop2017 <- w_matched_yr$pop17

#add new columns for deforestation
#this will return a 0 if there was no deforestation between years, 
#and a positive number 0.0-1.0 if there was deforestation between years
#(I know the order seems wrong, but this way we avoid negative numbers)

w_matched_yr$defor2005 <- w_matched_yr$for2004-w_matched_yr$for2005
w_matched_yr$defor2006 <- w_matched_yr$for2005-w_matched_yr$for2006
w_matched_yr$defor2007 <- w_matched_yr$for2006-w_matched_yr$for2007
w_matched_yr$defor2008 <- w_matched_yr$for2007-w_matched_yr$for2008
w_matched_yr$defor2009 <- w_matched_yr$for2008-w_matched_yr$for2009
w_matched_yr$defor2010 <- w_matched_yr$for2009-w_matched_yr$for2010
w_matched_yr$defor2011 <- w_matched_yr$for2010-w_matched_yr$for2011
w_matched_yr$defor2012 <- w_matched_yr$for2011-w_matched_yr$for2012
w_matched_yr$defor2013 <- w_matched_yr$for2012-w_matched_yr$for2013
w_matched_yr$defor2014 <- w_matched_yr$for2013-w_matched_yr$for2014
w_matched_yr$defor2015 <- w_matched_yr$for2014-w_matched_yr$for2015
w_matched_yr$defor2016 <- w_matched_yr$for2015-w_matched_yr$for2016
w_matched_yr$defor2017 <- w_matched_yr$for2016-w_matched_yr$for2017

names(w_matched_yr)

#Select desired variables, reorder columns, including riceavg2011 and ricesd2016 which are out of order

w_matched_yr_subs <- w_matched_yr %>%
  dplyr::select(CFM, PA, cfm_id, pa_id, dist_cart, dist_road, dist_urb, dist_vil, DVSP, edge05, edge10, edge14,fordens2005, fordens2010, fordens2014, elev, rice, precip, slope, veg_type, for2005:for2017, defor2005:defor2017, distance2005:distance2017, pop2005:pop2017, riceavg2005:riceavg2010, riceavg2011, riceavg2012:riceavg2017, ricesd2005:ricesd2015, ricesd2016, ricesd2017, drought2005:drought2017, maxPrecip2005:maxPrecip2017, maxTemp2005:maxTemp2017, wind2005:wind2017) #note edge_10 and edge_14 were renamed edge10 and edge14, added distance variables, added climate variables, excluded 2018-2020 for now

names(w_matched_yr_subs)

#subset data to split up PA and CFM data

#for PA data:
PA_data_yr <- w_matched_yr_subs %>%
  filter(CFM==0)

#for CFM data:

CFM_data_yr <- w_matched_yr_subs %>%
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

#check to make sure they look ok
#View(PA_data_yr)
#View(CFM_data_yr)

#join the tables back together
w_matched_yr_bind <- rbind(PA_data_yr, CFM_data_yr)
View(w_matched_yr_bind)





# reorganize based on https://dcl-wrangle.stanford.edu/pivot-advanced.html
#this works but the time variant variables are all in a single column, "timevariant"
w_matched_yr_longer <- w_matched_yr_bind %>%
  pivot_longer(
    cols = for2005:wind2017, #added new variables here
    names_to = c("timevariant", "year"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "timevariant_values"
  )

#View
View(w_matched_yr_longer)

#now pivot wider to get time variant variables as columns instead of rows
w_matched_yr_wider <- w_matched_yr_longer %>%
  pivot_wider(
    names_from = "timevariant",
    values_from = "timevariant_values"
  )

#View
View(w_matched_yr_wider) #WORKED!

#rename "for" to "forest" because this causes problems later
w_matched_yr_wider <- rename(w_matched_yr_wider,
                       forest = "for")

#write to CSV
write_csv(w_matched_yr_wider,'outputs/w_matched_yr_wider_6May2022.csv') #update date!




### SPECIFY THE ANNUAL MODEL -------------------
#outcome variable: FOREST COVER, so POSITIVE coefficients indicate MORE forest cover (LESS deforestation), I think

library(tidyverse)
library(dplyr)
library(tidyr)
library(plm)

#load data if needed
w_matched_yr_wider <- read_csv('outputs/w_matched_yr_wider_3May2022.csv') #update date!

names(w_matched_yr_wider)

#add variable "time" which takes values 1-13 for years 2005-2017 #doesn't work
#w_matched_yr_wider$time <- w_matched_yr_wider$year - 2004 
#Error in w_matched_yr_wider$year - 2004 : 
#non-numeric argument to binary operator

#View(w_matched_yr_wider)

did_m1_yr <- plm(defor ~ CFM*year + distance + pop + riceavg + ricesd + drought + maxPrecip + maxTemp + wind, data = w_matched_yr_wider, effect="twoways", model = "within", index = c("UID", "year")) #new output variable defor instead of forest, new control variable "distance" from forest edge

#did_m1_yr <- plm(forest ~ CFM*year + pop + riceavg + ricesd, data = w_matched_yr_wider, effect="twoways", model = "within", index = c("UID", "year")) #original version looked at forest cover, not defor

summary(did_m1_yr)


#try package "broom" to output regression summary table
library(broom)
did_m1_yr_summary <- broom::tidy(did_m1_yr)

#write to CSV
write_csv(did_m1_yr_summary,'outputs/did_m1_yr_summary_6May2022.csv') #update date!




