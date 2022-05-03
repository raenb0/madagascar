library(tidyverse)
library(dplyr)
library(Matching)
library(tidyr)


# Load data ---------------------------------------------------------------

setwd("C:/Users/raenb/Documents/GitHub/madagascar") # set working directory

# read in 90m data

cfm_90m_data <- read_csv("data/sample_points/cfm05_90m_data.csv") #note all columns read in as double, but some are categorical**
pa_90m_data <- read_csv("data/sample_points/pa05_90m_data.csv")

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
                       edge_05 = edge05,
                       elev = elevmsk,
                       for2000 = for200090m,
                       for2001 = for200190m,
                       for2002 = for200290m,
                       for2003 = for200390m,
                       for2004 = for200490m,
                       for2005 = for200590m,
                       for2006 = for200690m,
                       for2007 = for200790m,
                       for2008 = for200890m,
                       for2009 = for200990m,
                       for2010 = for201090m,
                       for2011 = for201190m,
                       for2012 = for201290m,
                       for2013 = for201390m,
                       for2014 = for201490m,
                       for2015 = for201590m,
                       for2016 = for201690m,
                       for2017 = for201790m,
                       pa_id = paid,
                       precip = precyr,
                       pop05 = mdgpd2005,
                       pop06 = mdgpd2006,
                       pop07 = mdgpd2007,
                       pop08 = mdgpd2008,
                       pop09 = mdgpd2009,
                       pop10 = mdgpd2010,
                       pop11 = mdgpd2011,
                       pop12 = mdgpd2012,
                       pop13 = mdgpd2013,
                       pop14 = mdgpd2014,
                       pop15 = mdgpd2015,
                       pop16 = mdgpd2016,
                       pop17 = mdgpd2017,
                       rice = ricethr,
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
                      edge_05 = edge05,
                      elev = elevmsk,
                      for2000 = for200090m,
                      for2001 = for200190m,
                      for2002 = for200290m,
                      for2003 = for200390m,
                      for2004 = for200490m,
                      for2005 = for200590m,
                      for2006 = for200690m,
                      for2007 = for200790m,
                      for2008 = for200890m,
                      for2009 = for200990m,
                      for2010 = for201090m,
                      for2011 = for201190m,
                      for2012 = for201290m,
                      for2013 = for201390m,
                      for2014 = for201490m,
                      for2015 = for201590m,
                      for2016 = for201690m,
                      for2017 = for201790m,
                      pa_id = paid,
                      precip = precyr,
                      pop05 = mdgpd2005,
                      pop06 = mdgpd2006,
                      pop07 = mdgpd2007,
                      pop08 = mdgpd2008,
                      pop09 = mdgpd2009,
                      pop10 = mdgpd2010,
                      pop11 = mdgpd2011,
                      pop12 = mdgpd2012,
                      pop13 = mdgpd2013,
                      pop14 = mdgpd2014,
                      pop15 = mdgpd2015,
                      pop16 = mdgpd2016,
                      pop17 = mdgpd2017,
                      rice = ricethr,
                      veg_type = vegtype)

names(pa_90m_data) #check if renaming worked


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

# join tables

names(cfm_90m_filter) #check if column variables are identical, looks good
names(pa_90m_filter)

cfm_pa_data_90m <- full_join(cfm_90m_filter, pa_90m_filter) #full_join includes all rows in x or y

#cfm_pa_data_90m <- cfm_pa_data_90m %>% dplyr::select(-InclProb) #dropped first variable (InclProb) 

cfm_pa_data_90m_no_na <- drop_na(cfm_pa_data_90m) #remove sample points with NA values **SKIPPED THIS STEP, MIGHT CAUSE ISSUES

#write to CSV
write_csv(cfm_pa_data_90m_no_na,'outputs/cfm_pa_data_90m_no_na_17Mar2022.csv') #update date
#write_csv(cfm_pa_data_90m,'outputs/cfm_pa_data_90m_17Mar2022.csv') #version with NA values

# Define treatment
# Ranaivo says: We do not need to define the outcome because we are not going to use the estimate from Matching. Matching can work without the outcome.

Treat <- cfm_pa_data_90m_no_na$CFM

# Define covariates

names(cfm_pa_data_90m_no_na)

cov.names <- c("dist_cart","dist_road","dist_urb","dist_vil","DVSP","edge_05","elev","pop05","rice","precip","slope","veg_type") # Names of covariates used to match **NOTE alphabetical order, included population 2005 and distance to forest edge 2005

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

m1 <-  Match(Tr=Treat, X=covs, M = 1, BiasAdjust=FALSE,  exact=Ex, replace=TRUE, ties=TRUE, Weight=2) # run matching



# CHECK COVARIATE BALANCE -------------------------------------------------
#Ranaivo said to replace "covs" with the list of covariates we actually want to use to match on

names(covs)

mb1 <- MatchBalance(Treat ~ dist_cart + dist_road + dist_urb + dist_vil + DVSP + edge_05 + elev + pop05 + precip + rice + slope + veg_type,
                    match.out = m1, nboots = 500, data=cfm_pa_data_90m_no_na) 

# Ranaivo said: Do not worry about the p-values of the balance statistics. We have large sample. So, p-values are likely to be significant
# Further, there is the issue of multiple hypotheses testing. The same dataset is used to test the different covariates. So, the probability of finding significant difference is increased


##Chris said: The matching effectiveness can be tested using a joint balance test (an F- or chi-squared test of the linear
#probability OLS regression using CFM as a binary dependent variable, regressed on all of the observables you have at baseline
#(2005) and look at the F-test that the whole set of covariates explains statistically significant variation in CFM status 
#(vs. PA status).

#BEFORE MATCHING:

#load data if needed
cfm_pa_data_90m_no_na <- read_csv('outputs/cfm_pa_data_90m_no_na_17Mar2022.csv') #update date
matched <- read_csv('outputs/mahalanobis_matched_17Mar2022.csv')  #update date

Treat <- cfm_pa_data_90m_no_na$CFM

mb2 <- lm(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + DVSP + edge_05 + elev + pop05 + precip + rice + slope + veg_type, data = cfm_pa_data_90m_no_na)
summary(mb2)

#AFTER MATCHING: 

mb3 <- lm(CFM ~ dist_cart + dist_road + dist_urb + dist_vil + DVSP + edge_05 + elev + pop05 + precip + rice + slope + veg_type, data = matched)

summary(mb3)


## MATCHED DATASET TO BE USED FOR THE DIFFERENCE IN DIFFERENCE (DID) ANALYSIS 

matched <- rbind(cfm_pa_data_90m_no_na[m1$index.treated,],cfm_pa_data_90m_no_na[m1$index.control,]) # this is the matched dataset
wght <- c(m1$weights,m1$weights) # weights of the observations in the matched dataset, to be used for post matching analysis (e.g., DID regression)

#write to CSV *update date!

write_csv(matched, 'outputs/mahalanobis_matched_17Mar2022.csv') #update date
write.csv(wght,'outputs/mahalanobis_wght_17Mar2022.csv') #note this outputs a table with only values of 1***



### GENETIC MATCHING ###  (TAKES >63 HRS TO RUN)-------------------------

#library(rgenoud)

## SPECIFYING THE MATCHING
#gen1 <- GenMatch(Tr=Treat, X=covs, pop.size= 500, exact= Ex, replace=TRUE, ties= TRUE) #took >63 hrs to run
#mgen1 <- Match(Tr=Treat, X=covs, M = 1, BiasAdjust=FALSE, exact=Ex, replace=TRUE, ties=TRUE, Weight.matrix= gen1)

## CHECK COVARIATE BALANCE
#mb2 <- MatchBalance(Treat ~ elev + dist_road + dist_cart + dist_urb + dist_vil + precip + slope + pop00 + DVSP + veg,
#                    match.out = mgen1, nboots = 500, data=cfm_pa_data_90m_no_na) 

## MATCHED DATASET TO BE USED FOR THE DIFFERENCE IN DIFFERENCE (DID) ANALYSIS 

#gen.matched <- rbind(cfm_pa_data_90m_no_na[mgen1$index.treated,],cfm_pa_data_90m_no_na[mgen1$index.control,]) # this is the matched dataset
#gen.wght <- c(mgen1$weights,mgen1$weights)# weights of the observations in the matched dataset, to be used for post matching analysis (e.g., DID regression)

#WRITE TO CSV (Ranaivo says not to bother)

#write.csv(gen.matched,'outputs/genetic_matched.csv')
#write.csv(gen.wght,'outputs/genetic_wght.csv')



# ## REORGANIZE MAHALANOBIS MATCHED DATA FOR TWO-PERIOD ANALYSIS -----------------------
# 
# library(tidyverse)
# library(dplyr)
# 
# #load tabular data if needed
# 
# matched <- read_csv('outputs/mahalanobis_matched_17Mar2022.csv')  #update dates
# wght <- read_csv('outputs/mahalanobis_wght_17Mar2022.csv')
# 
# #add weights to matched dataset
# 
# w.matched <- data.frame(matched, wght)
# 
# #add new columns for deforestation
# #this will return a 0 if there was no deforestation between 2005 and 2010, 
# #and 0-1 if there was deforestation from 2005 and 2010 
# #(I know the order seems wrong, but this way we avoid negative numbers)
# 
# w.matched$defor.1 <- w.matched$for2005-w.matched$for2010
# w.matched$defor.2 <- w.matched$for2010-w.matched$for2014
# 
# #add population variables 
# #Ranaivo says: It is better to use the pop at the beginning of the periods 
# #(i.e., in 2005 for period 1 and 2010 for period 2) than the difference
# #(we only need 2 time periods)
# 
# w.matched$pop.1 <- w.matched$pop05
# w.matched$pop.2 <- w.matched$pop10
# 
# #add distance to forest edge variables
# 
# w.matched$edge.1 <- w.matched$edge_05
# w.matched$edge.2 <- w.matched$edge_10
# 
# #add change in forest density (fragmentation) variables **New addition July 12
# #this will return a 0 if there was no change in density % between 2005 and 2010, 
# #and 0-100 if there was change in density % from 2005 and 2010 
# # e.g. POSITIVE values indicate DECREASE in density
# 
# w.matched$dens.1 <- w.matched$fordens05-w.matched$fordens10
# w.matched$dens.2 <- w.matched$fordens10-w.matched$fordens14
# 
# #Select desired variables, reorder columns
# 
# names(w.matched)
# 
# w.matched.subs <- w.matched %>%
#   dplyr::select(CFM, PA, cfm_id, pa_id, dist_cart, dist_road, dist_urb, dist_vil, DVSP, elev, rice, precip, slope, veg_type,  pop.1, pop.2, defor.1, defor.2, edge.1, edge.2, dens.1, dens.2)
# 
# #subset data to split up PA and CFM data
# 
# #for PA data:
# PA_data <- w.matched.subs %>%
#   filter(CFM==0)
# 
# #for CFM data:
# 
# CFM_data <- w.matched.subs %>%
#   filter(CFM==1)
# 
# #add unique ID columns to each subset:
# PA_data$ID <- seq.int(nrow(PA_data))
# CFM_data$ID <- seq.int(nrow(CFM_data))
# 
# #add new variable PA_CFM
# PA_data$PA_CFM <- "PA"
# CFM_data$PA_CFM <- "CFM"
# 
# #create character strings for new unique IDs
# PA_data$UID <- do.call(paste0, PA_data[c("PA_CFM", "ID")])
# CFM_data$UID <- do.call(paste0, CFM_data[c("PA_CFM", "ID")])
# 
# #check to make sure they look ok
# View(PA_data)
# View(CFM_data)
# 
# #add new variable to CFM data: "renewed" (0 or 1) to indicate subset of CFM areas that were renewed ("implemented") ***new addition August 18 2021
# 
# cfm_pre05_renewed <- read_csv('data/CFM_pre05_renewed.csv') #attribute table from CFM pre-2005 shp
# renewed <- cfm_pre05_renewed %>%
#   dplyr::select(ID, RENEWED)
# CFM_data <- left_join(CFM_data, renewed, by = c("cfm_id" = "ID"))
# 
# PA_data$RENEWED <- 0 #number of columns must match
# 
# #join the tables back together
# w.matched_bind <- rbind(PA_data, CFM_data)
# View(w.matched_bind)
# 
# #reorganize
# w.matched.reorg <- reshape(w.matched_bind,
#                             idvar = "UID",
#                             direction = "long",
#                             varying = c("pop.1", "pop.2", "defor.1", "defor.2", "edge.1", "edge.2", "dens.1", "dens.2")
#                             )
# 
# #View
# View(w.matched.reorg)
# 
# #write to CSV
# write_csv(w.matched.reorg,'outputs/w.matched.reorg_18Aug2021.csv') #update date



# ### SPECIFY THE TWO-PERIOD MODEL, DEFORESTATION OUTCOME -------------------
# #outcome variable: DEFORESTATION (so POSITIVE coefficients = MORE deforestation, I think)
# 
# #load data if needed
# w.matched.reorg <- read_csv('outputs/w.matched.reorg_27Aug2021.csv')  
# 
# library(plm)
# library(clubSandwich)
# 
# did_m1 <- plm(defor ~ CFM*time + pop + edge, data = w.matched.reorg, effect="twoways", model = "within", index = c("UID", "time"))
# 
# summary(did_m1)
# 
# #### CLUSTERED SE -------------------
# #code adapted from Ranaivo
# 
# #add a new alphanumeric variable: "cluster" with the format cfm00pa00
# names(w.matched.reorg)
# w.matched.reorg$cluster <- do.call(paste0, c("cfm", w.matched.reorg["cfm_id"], "pa", w.matched.reorg["pa_id"]))
# 
# memory.limit(size=5000000)
# 
# V_CR2 <- vcovCR(did_m1, cluster=w.matched.reorg$cluster, type="CR2") # clustering SE. "clusterID": CFM site or PA identification code in the data #didn't work: Error: cannot allocate vector of size 3.7 Gb
# 
# coef_test(did_m1, vcov=V_CR2, test="Satterthwaite") # p-values #also took a long time to run
# conf_int(did_m1, vcov=V_CR2, test="Satterthwaite") # 95% CI. I personally prefer presenting CI because of all the controversies surrounding p-values
# 
# 
# ### SPECIFY THE TWO-PERIOD MODEL, FOREST DENSITY OUTCOME -------------------
# #outcome variable: FOREST DENSITY (so POSITIVE coefficients = MORE intact forest, I think)
# 
# #load data if needed
# w.matched.reorg <- read_csv('outputs/w.matched.reorg_27Aug2021.csv')  
# 
# library(plm)
# 
# did_m2 <- plm(dens ~ CFM*time + pop + edge, data = w.matched.reorg, effect="twoways", model = "within", index = c("UID", "time"))
# 
# summary(did_m2)





## REORGANIZE MAHALANOBIS MATCHED DATA FOR ANNUAL ANALYSIS -----------------------##updated March 17 2022

library(tidyverse)
library(dplyr)

#load tabular data if needed
matched <- read_csv('outputs/mahalanobis_matched_17Mar2022.csv')  #update dates
#wght <- read_csv('outputs/mahalanobis_wght_17Mar2022.csv') #unnecessary

#add weights to matched dataset ##unnecessary

w_matched_yr <- data.frame(matched)#, wght)

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

#Select desired variables, reorder columns

names(w_matched_yr)

w_matched_yr_subs <- w_matched_yr %>%
  dplyr::select(CFM, PA, cfm_id, pa_id, dist_cart, dist_road, dist_urb, dist_vil, DVSP, edge_05, edge10, edge14,fordens2005, fordens2010, fordens2014, elev, rice, precip, slope, veg_type, for2005:for2017, pop2005:pop2017, riceavg2005:riceavg2017, ricesd2005:ricesd2017) #note edge_10 and edge_14 were renamed edge10 and edge14

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
    cols = for2005:ricesd2017,
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
write_csv(w_matched_yr_wider,'outputs/w_matched_yr_wider_17Mar2022.csv') #update date




### SPECIFY THE ANNUAL MODEL -------------------
#outcome variable: FOREST COVER, so POSITIVE coefficients indicate MORE forest cover (LESS deforestation), I think

library(tidyverse)
library(dplyr)
library(tidyr)
library(plm)

#load data if needed
w_matched_yr_wider <- read_csv('outputs/w_matched_yr_wider_17Mar2022.csv')

names(w_matched_yr_wider)

#add variable "time" which takes values 1-13 for years 2005-2017
w_matched_yr_wider$time <- w_matched_yr_wider$year - 2004

View(w_matched_yr_wider)

did_m1_yr <- plm(forest ~ CFM*year + pop + riceavg + ricesd, data = w_matched_yr_wider, effect="twoways", model = "within", index = c("UID", "year")) #do I need to include CFM + time individually as well as the interaction term?

summary(did_m1_yr)

did_m1_yr_coeffs <- as.data.frame(did_m1_yr$coefficients)
did_m1_yr_coeffs$variables <- row.names(did_m1_yr_coeffs)




