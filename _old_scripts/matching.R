library(tidyverse)
library(dplyr)
library(Matching)
library(tidyr)

setwd("C:/Users/raenb/Documents/GitHub/madagascar") # set working directory

#read in 30m data

cfm_30m_data <- read_csv("data/sample_points/cfm_for00_data_30m.csv") # read CFM 30m data
pa_30m_data <- read_csv("data/sample_points/pa_for00_data_30m.csv") #, col_types = cols(vegtypemsk = col_factor())

# read in 90m data

cfm_90m_data <- read_csv("data/sample_points/cfm_for00_data_90m.csv")
pa_90m_data <- read_csv("data/sample_points/pa_for00_data_90m.csv")

# read in 300m data

cfm_300m_data <- read_csv("data/sample_points/cfm_for00_data_300m.csv") 
pa_300m_data <- read_csv("data/sample_points/pa_for00_data_300m.csv")

# working with 90m data to start

# remove CFM data point if PA ID has a value (those are sample points in areas that overlap both PAs and CFM areas)
# and vice versa

names(cfm_90m_data) #get variable names
unique(cfm_90m_data$paidmsk) #get unique values of PA IDs - includes both 0 and NA, we want NA to be 0
cfm_90m_data$paidmsk %>% replace_na(0) #replace NA values with 0
unique(cfm_90m_data$paidmsk) #check if worked - no NA values anymore

cfm_90m_filter <- cfm_90m_data %>%
  filter(cfm_90m_data$paidmsk<=0) #remove rows where PA ID is not zero
unique(cfm_90m_filter$paidmsk) #check if worked - only 0 values returned, worked

#repeat for PA data
names(pa_90m_data) #get variable names
unique(pa_90m_data$cfmrstrid) #get unique values of CFM IDs - includes NA, we want NA to be 0
#pa_90m_data$cfmrstrid %>% replace_na(0) #replace NA values with 0, doesn't work
#unique(pa_90m_data$cfmrstrid) #check if worked - didn't work
pa_90m_data <- pa_90m_data %>% 
  mutate(cfmrstrid = replace(as.numeric(cfmrstrid), which(is.na(cfmrstrid)), 0))
unique(pa_90m_data$cfmrstrid) # WORKED!!
         
pa_90m_filter <- pa_90m_data %>%
  filter(cfmrstrid<=0) #remove rows where PA ID is not NA
unique(pa_90m_filter$cfmrstrid) #check if worked - only 0 values returned, worked

# add columns for CFM (0 or 1) and PA (0 or 1)

cfm_90m_filter$CFM <- 1
cfm_90m_filter$PA <- 0

pa_90m_filter$CFM <- 0
pa_90m_filter$PA <- 1

# join tables

names(cfm_90m_filter) #check if column variables are identical, looks good
names(pa_90m_filter)

cfm_pa_data_90m <- full_join(cfm_90m_filter, pa_90m_filter) #full_join includes all rows in x or y

cfm_pa_data_90m <- cfm_pa_data_90m %>% dplyr::select(-InclProb) #dropped first variable (InclProb) 

cfm_pa_data_90m_no_na <- drop_na(cfm_pa_data_90m)

# We do not need to define the outcome because we are not going to use the estimate from Matching. Matching can work without the outcome.

Treat <- cfm_pa_data_90m_no_na$CFM # Define treatment

names(cfm_pa_data_90m_no_na)

cfm_pa_data_90m_no_na <- rename(cfm_pa_data_90m_no_na, dist_cart = distcartutm, dist_road = distroadutm, dist_urb = disturbutm, dist_vil = distvilutm, DVSP = durationrst, elev = elevationut, pop00 = lspop2000UT, rice = paddythrutm, precip = precyrutm, slope = slopeutm, veg = vegtypemsk) #rename the covariates

cov.names <- c("dist_cart","dist_road","dist_urb","dist_vil","DVSP","elev","pop00","rice","precip","slope","veg") # Names of covariates used to match **NOTE alphabetical order, included population 2000


covs <- cfm_pa_data_90m_no_na[cov.names] # Extract the covariates

Ex <- c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", 
        "FALSE", "FALSE", "TRUE") # Logical vector to allow EXACT matching to be done for the "veg" variable (i.e., matching operates within each type of vegetation)


### MAHALANOBIS DISTANCE MATCHING ###

## SPECIFYING THE MATCHING

# We do not need to specify the arguments Z and V in the Match function because we are not going to use the estimate from Matching
# Z is needed if we want to do bias adjusment to linearly correct for the remaining imbalance within Matching. Usually Z is the same as X
# Because BiasAdjust = FALSE, no biase adjustment will be performed (so no need to define Z)
# V deals with the homoscedasticity assumption in linear model (t-test is a lineal model). V define the variables for which homoscedasticity-robust variances will be calculated
# What we want is the ATT (Average effect of Treatment on the Treated). So, we leave the argument estimand to its defauld (i.e., "ATT")
# Weight = 2 specifies Mahalanobis distance matching

m1 <-  Match(Tr=Treat, X=covs, M = 1, BiasAdjust=FALSE,  exact=Ex,replace=TRUE, ties=TRUE, Weight=2) # run matching

## CHECK COVARIATE BALANCE

names(covs)

mb1 <- MatchBalance(Treat ~ elev + dist_road + dist_cart + dist_urb + dist_vil + precip + slope + pop00 + DVSP + veg,
                    match.out = m1, nboots = 500, data=cfm_pa_data_90m_no_na) 

#Update: Ranaivo said to replace "covs" with the list of covariates we actually want to use to match on
# Ranaivo: Do not worry about the p-values of the balance statistics. We have large sample. So, p-values are likely to be significant
# Further, there is the issue of multiple hypotheses testing. The same dataset is used to test the different covariates. So, the probability of finding significant difference is increased

## MATCHED DATASET TO BE USED FOR THE DIFFERENCE IN DIFFERENCE (DID) ANALYSIS 

matched <- rbind(cfm_pa_data_90m_no_na[m1$index.treated,],cfm_pa_data_90m_no_na[m1$index.control,]) # this is the matched dataset
wght <- c(m1$weights,m1$weights) # weights of the observations in the matched dataset, to be used for post matching analysis (e.g., DID regression)

#write to CSV (Ranaivo says not to bother)
write_csv(matched, 'outputs/mahalanobis_matched.csv')
write.csv(wght,'outputs/mahalanobis_wght.csv')



### GENETIC MATCHING ###  (TAKES >63 HRS TO RUN)

#library(rgenoud)

## SPECIFYING THE MATCHING
#gen1 <- GenMatch(Tr=Treat, X=covs, pop.size= 500, exact= Ex, replace=TRUE, ties= TRUE) #took >63 hrs to run
#mgen1 <- Match(Tr=Treat, X=covs, M = 1, BiasAdjust=FALSE, exact=Ex, replace=TRUE, ties=TRUE, Weight.matrix= gen1)

## CHECK COVARIATE BALANCE
#mb2 <- MatchBalance(Treat ~ elev + dist_road + dist_cart + dist_urb + dist_vil + precip + slope + pop00 + DVSP + veg,
                    match.out = mgen1, nboots = 500, data=cfm_pa_data_90m_no_na) 

## MATCHED DATASET TO BE USED FOR THE DIFFERENCE IN DIFFERENCE (DID) ANALYSIS 

#gen.matched <- rbind(cfm_pa_data_90m_no_na[mgen1$index.treated,],cfm_pa_data_90m_no_na[mgen1$index.control,]) # this is the matched dataset
#gen.wght <- c(mgen1$weights,mgen1$weights)# weights of the observations in the matched dataset, to be used for post matching analysis (e.g., DID regression)

#WRITE TO CSV (Ranaivo says not to bother)

#write.csv(gen.matched,'outputs/genetic_matched.csv')
#write.csv(gen.wght,'outputs/genetic_wght.csv')



## REORGANIZE MAHALANOBIS MATCHED DATA FOR ANALYSIS

#add weights to matched dataset

w.matched <- data.frame(matched, wght)

#add new columns for deforestation
#this will return a 0 if there was no deforestation between 2005 and 2010, 
#and a 1 if there was deforestation from 2005 and 2010 
#(I know the order seems wrong, but this way we avoid negative numbers)

w.matched$defor.1 <- w.matched$for05-w.matched$for10
w.matched$defor.2 <- w.matched$for10-w.matched$for14

#rename population variables 
#Ranaivo says: It is better to use the pop at the beginning of the periods 
#(i.e., in 2005 for period 1 and 2010 for period 2) than the difference
#(we only need 2 time periods, but I renamed all three for clarity)

names(w.matched)[names(w.matched) == "pop05"] <- "pop.1"
names(w.matched)[names(w.matched) == "pop10"] <- "pop.2"
names(w.matched)[names(w.matched) == "pop13"] <- "pop.3"

#Remove unnecessary variables, i.e., for05, for10, for14, pop.3

w.matched.subs = subset(w.matched, select = -c(for05, for10, for14, pop.3) )

#subset data to split up PA and CFM data
#for PA data:
PA_data <- subset(w.matched.subs, CFM==0, select = FID:defor.2)

#for CFM data:
CFM_data <- subset(w.matched.subs, CFM==1, select = FID:defor.2)

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
w.matched_bind <- rbind(PA_data, CFM_data)
View(w.matched_bind)

#reorganize - something went wrong because pop is always 0 in time 2
#w.matched.reorg <- reshape(w.matched_bind,
#                           idvar = "UID",
#                           direction="long", 
#                           varying=c(pop=c(11,12), defor=c(16,17)),
#                           v.names = c("pop", "defor"),
#                           times = c("1", "2")
#                           )

#trying Ranaivo's simpler version (I added idvar="UID") - seems to work
w.matched.reorg <- reshape(w.matched_bind,
                            idvar = "UID",
                            direction = "long",
                            varying = c(12, 13, 17, 18)
                            )

#write to CSV
write.csv(w.matched.reorg,'w.matched.reorg.csv')

#View
View(w.matched.reorg)

##TRY OUT PGLM - didn't work
#library(pglm)

#pglm(defor ~ time + time*CFM + pop,
#     data = w.matched.reorg,
#     effect = "twoways", #specify that we control for time period
#     model = "within", #change within same unit over time
#     family = binomial('logit'), #is this right? 
#     index = c("UID", "time"),
#     start = NULL
#     )

#(Trying to get pglm to work)
#tried family = binomial('logit') and got below error

# Error in maxRoutine(fn = logLik, grad = grad, hess = hess, start = start,  : 
#argument "start" is missing, with no default

#tried family = ordinal('logit') and got this error

#Error in pglm(defor ~ time + time * CFM + pop, data = w.matched.reorg,  : 
#the response must have at least 3 different values

#tried family = binomial('probit') and got the same error as the first time

#Error in maxRoutine(fn = logLik, grad = grad, hess = hess, start = start,  : 
#                            argument "start" is missing, with no default

#tried  family = 'logit'

#Error in get(family, mode = "function") : 
#object 'logit' of mode 'function' was not found


#TRY OUT CONDITIONAL LOGIT MODEL (STEPHEN PARRY's SUGGESTION)
# logit <- glm(defor ~ time + time*CFM + pop,
#              data = w.matched.reorg,
#              family = binomial)

summary(logit)

##Try again
library(survival)

clogit <- clogit(defor ~ time + time*CFM + pop,
       data = w.matched.reorg,
       method="exact")

summary(clogit)
