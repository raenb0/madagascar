# Madagascar impact evaluation
# Event study, parallel trends test, all CFM, 90m
# Rachel Neugarten
# 12 Feb 2024


# Check parallel trends in the pre-crisis period --------------------------

library(tidyverse)
library(fixest)

matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_region_12Feb2024.csv') #update date, version with region ID added
names(matched_yr_wider)

matched_yr_pre_crisis <- matched_yr_wider %>% filter(year < 2010) #note year is not a factor in this version
matched_yr_pre_crisis$year <- as.factor(matched_yr_pre_crisis$year) #convert "year" to a factor

# parallel trends check: defor_annual
did_m1_yr_pre_crisis <- feols(defor_annual ~ CFM*year + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                              | UID, #don't need vegtype because redundant with UID, UID and year make it two-ways, remove year fixed effect to plot year, CFM:year separately
                              data = matched_yr_pre_crisis,
                              cluster = "cluster") #only pre-crisis years 2005-2009
summary(did_m1_yr_pre_crisis) #coefficient on CFM:year not significant (achieved parallel trends)***

covariances_m1_pre_crisis <- vcov(did_m1_yr_pre_crisis) #save variance-covariance matrix

#tidy coefficients table and save to csv
library(broom)
library(gtools)

parallel_trends_m1_table <- tidy(did_m1_yr_pre_crisis) %>%
  mutate(signif = stars.pval(p.value))
write_csv(parallel_trends_m1_table, "outputs/parallel_trends_m1_table_31May2023.csv") #update date

## calculate coefficients, SE, confidence intervals -------------
# code from Sumanta Basu
# vcov: variance-covariance matrix
# vars: a vector with the indices of regression predictors to add
# returns the standard error of the sum of betas

calc_se <- function(varcov, vars){
  return(sqrt(sum(varcov[vars, vars])))
}

#calc_se(varcov = covariances_m1_pre_crisis, vars = c(1,13)) #test: calculate SE for year2006, CFM:year2006

did_m1_coefficients <- did_m1_yr_pre_crisis$coefficients #store coefficient estimates

## varcov: variance-covariance matrix
# vars: a vector with the indices of regression predictors to add
# coef: a vector with regression coefficients
# ci_level: either 0.95 or 0.90
# returns the CI of the sum of betas

#function to calculate the SE and confidence intervals
calc_ci <- function(coef, varcov, vars, ci_level = 0.95){
  se = sqrt(sum(varcov[vars, vars]))
  multiplier = ifelse(ci_level == 0.95, 1.96, 1.645)
  ci_lo = sum(coef[vars]) - multiplier*se
  ci_hi = sum(coef[vars]) + multiplier*se
  return(c(ci_lo, ci_hi))
}

#initialize an array
ci_table_pre_crisis <- array(0, c(4, 2))

#apply the function to event_study_m1$coefficients and  covariances_m1
for (j in 1:4){  #this refers to coefficients 1:4 (year2006 to year2009)
  ci_table_pre_crisis[j,] = calc_ci(coef = did_m1_yr_pre_crisis$coefficients, 
                           varcov = covariances_m1_pre_crisis,
                           vars = c(j, j+12), ci_level = 0.90) #change vars entry 
}

round(ci_table_pre_crisis,5)

## DiD (annual) without a variable for yrs_post_crisis -----------
# We initially used a DiD but moved to an event study design as it better controls for differences in pre-crisis trends, see below
matched_yr_wider$year <- as.factor(matched_yr_wider$year) #convert "year" to a factor

did_m1_yr <- feols(defor_annual ~ CFM*year + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                              | UID, #remove year fixed effect to plot year, CFM:year separately
                              data = matched_yr_wider,
                              cluster = "cluster") 
summary(did_m1_yr)


#calculate the correct SE to plot DiD MNP and CFM coefficients
covariances_did_m1 <- vcov(did_m1_yr) #save variance-covariance table

calc_se <- function(varcov, vars){
  return(sqrt(sum(varcov[vars, vars])))
}

#function to calculate the SE and confidence intervals
calc_ci <- function(coef, varcov, vars, ci_level = 0.95){
  se = sqrt(sum(varcov[vars, vars]))
  multiplier = ifelse(ci_level == 0.95, 1.96, 1.645)
  ci_lo = sum(coef[vars]) - multiplier*se
  ci_hi = sum(coef[vars]) + multiplier*se
  return(c(ci_lo, ci_hi))
}

did_m1_yr_table <-tidy(did_m1_yr) %>%
  mutate(signif = stars.pval(p.value))
write_csv(did_m1_yr_table, "outputs/DiD_m1_yr_coefficients_6June2023.csv") #update date

# Event Study models ------------------------------------------------------
# Event study model from Chris Barrett: 
# Deforestation_it = a_0 + beta_1*CFM_i + tau_1 * timestep_t  +  tau_2* timestep_t * CFM_i  + GAMMA*YEARS-POST-CRISIS + DELTA*CFM_i* YEARS-POST-CRISIS +  PSI*CONTROLS_it
# coefficient on (interaction between timestep_t and CFM) controls for parallel trends before crisis period
# delta (effect of interaction between CFM and years post-crisis) is the coefficient of interest

library(tidyverse)
library(fixest)

#load data if necessary

#matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_genetic_defor_annual_development_security_4Mar2023.csv') #update date

matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_region_12Feb2024.csv') #update date, version with region_ID added

names(matched_yr_wider)

#matched_yr_wider$year <- as.factor(matched_yr_wider$year) #convert "year" to a factor
matched_yr_wider$yrs_post_crisis <- as.factor(matched_yr_wider$yrs_post_crisis) #convert "yrs_post_crisis" to a factor

# change CFM to a factor
matched_yr_wider$CFM <- as.factor(matched_yr_wider$CFM)

### Event study model 1 (base model)  ------------------------
event_study_m1 <- feols(defor_annual ~ CFM*year + CFM*yrs_post_crisis #year (numeric) instead of timestep
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster") #original version
                        #cluster = c("cluster","region_ID"))#cluster by region
summary(event_study_m1)

# test whether the sum of coefficients are jointly significantly greater than 0
event_study_m1_coefficients_sum <- sum(event_study_m1$coefficients) #0.2262255 greater than 0, yes

#### Generate variance-covariance table to calculate correct SE ----------------
covariances_m1 <- vcov(event_study_m1)

#convert to data frame for saving
covariances_excel <- covariances_m1 %>%
  data.frame() %>%
  rownames_to_column()

write_csv(covariances_excel, "outputs/covariances_event_study_m1_30May2023.csv") #update date


#### Calculate SE from variance-covariance matrix  ---------------------
# vcov: variance-covariance matrix
# vars: a vector with the indices of regression predictors to add
# returns the standard error of the sum of betas

calc_se <- function(varcov, vars){
  return(sqrt(sum(varcov[vars, vars])))
}

# # example
# 
# vcov1 <- matrix(c(1, 0.5, 0.5, 2), nrow = 2)
# vcov1
# 
# vars <- c(1, 2)
# vars
# 
# # se(beta1 + beta2) 
# calc_se(varcov = vcov1, vars = vars)

#test with covariance matrix
# calc_se(varcov = covariances_m1, vars = c(1,2)) #looks good

## varcov: variance-covariance matrix
# vars: a vector with the indices of regression predictors to add
# coef: a vector with regression coefficients
# ci_level: either 0.95 or 0.90
# returns the CI of the sum of betas

#function to calculate the SE and confidence intervals
calc_ci <- function(coef, varcov, vars, ci_level = 0.95){
  se = sqrt(sum(varcov[vars, vars]))
  multiplier = ifelse(ci_level == 0.95, 1.96, 1.645)
  ci_lo = sum(coef[vars]) - multiplier*se
  ci_hi = sum(coef[vars]) + multiplier*se
  return(c(ci_lo, ci_hi))
}

# rewrite function to use variable names instead of index
# varname: a vector of variable names, e.g. c("year", "CFM", "yrs_post_crisis")
calc_ci_vname <- function(coef, varcov, varname, ci_level = 0.95){
  vars = which(rownames(varcov) %in% varname) 
  print(vars)
  se = sqrt(sum(varcov[vars, vars]))
  multiplier = ifelse(ci_level == 0.95, 1.96, 1.645)
  ci_lo = sum(coef[vars]) - multiplier*se
  ci_hi = sum(coef[vars]) + multiplier*se
  return(c(ci_lo, ci_hi))
}

#initialize an array
ci_table <- array(0, c(11, 2))

#apply the function to event_study_m1$coefficients and  covariances_m1
for (j in 2:12){  #this refers to coefficients 2:12 (year and yrs_post_crisis1:11)
  ci_table[j-1,] = calc_ci(coef = event_study_m1$coefficients, 
                         varcov = covariances_m1,
                         vars = c(1, j), ci_level = 0.90) #change vars entry 
}

round(ci_table,5) #this table gives the correct confidence intervals for yrs_post_crisis (CFM=0)


#initialize a new array
ci_table_name <- array(0, c(11, 2))

for (j in 2:12){  #this refers to coefficients 2:12 (year and yrs_post_crisis1:11)
  print(j)
  vnames_j = c("year", paste0("yrs_post_crisis", j-1))
  print(vnames_j)
  ci_table_name[j-1,] = calc_ci_vname(coef = event_study_m1$coefficients, 
                           varcov = covariances_m1,
                           varname = vnames_j, ci_level = 0.90) #change vars entry 
}

round(ci_table_name,5) #this table gives the correct confidence intervals for yrs_post_crisis (CFM=0)

#initialize a new array
ci_table2 <- array(0, c(11, 2))

for (j in 2:12){
# for (k in 22:32){ #this refers to coefficients 22:32 (CFM:year and CFM:yrs_post_crisis1:11)
  ci_table2[j-1,] = calc_ci(coef = event_study_m1$coefficients, #changed j-1 to k-21***
                           varcov = covariances_m1,
                           vars = c(1, j, 21, 20+j), ci_level = 0.90) #changed c(1,j) to c(21,k)***
# s}
}

round(ci_table2,5) #not yet correct because I need to combine the above coefficients with these coefficients***



### Event study model 2, interaction term: distance to urban center ------------------------
event_study_m2 <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                        + CFM*yrs_post_crisis*disturb #interaction term for distance to urban center
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster")
summary(event_study_m2)

### Event study model 3, with interaction term for development ------------

#relevel development so reference = 1 instead of 0
matched_yr_wider$development <- as.factor(matched_yr_wider$development) #convert to factor
matched_yr_wider <- within(matched_yr_wider, development <- relevel(development, ref = 1))

event_study_m3 <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis 
                        + CFM*yrs_post_crisis*development #interaction term for development
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster")
summary(event_study_m3)

### Event study model 4, with interaction term for security -----------------------

#relevel security so reference = 1 instead of 0
matched_yr_wider$security <- as.factor(matched_yr_wider$security) #convert to factor
matched_yr_wider <- within(matched_yr_wider, security <- relevel(security, ref = 1))

event_study_m4 <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                        + CFM*yrs_post_crisis*security #interaction term for security
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster")
summary(event_study_m4)

### Event study model 5, with interaction term for population density --------------
event_study_m5 <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                        + CFM*yrs_post_crisis*pop #interaction term for population
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster")
summary(event_study_m5)

### Event study model 6, with interaction term for distance from roads --------------
event_study_m6 <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                        + CFM*yrs_post_crisis*distroad #interaction term for distance from roads
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster")
summary(event_study_m6)  #coefficients are mostly negative but not significant

### Event study model 7, with interaction term for distance from villages --------------
event_study_m7 <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                        + CFM*yrs_post_crisis*distvil #interaction term for distance from villages
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster")
summary(event_study_m7) #coefficients are negative but not significant

### Event study model 8, with interaction term for slope --------------
event_study_m8 <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                        + CFM*yrs_post_crisis*slope #interaction term for slope
                        + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                        | UID,
                        data = matched_yr_wider,
                        cluster = "cluster")
summary(event_study_m8) #coefficients are not significant

### tidy coefficient tables and write to CSV files ------------------
library(broom)
library(gtools)

# #DiD model
# did_m1_table <-tidy(did_m1) %>%
#   mutate(signif = stars.pval(p.value))
# write_csv(did_m1_table, "outputs/did_m1_genetic_defor_annual_27Feb2023.csv") #update date


#event study models
event_study_m1_table <- tidy(event_study_m1) %>%
  mutate(signif = stars.pval(p.value))
event_study_m2_table <- tidy(event_study_m2) %>%
  mutate(signif = stars.pval(p.value))
event_study_m3_table <- tidy(event_study_m3) %>%
  mutate(signif = stars.pval(p.value))
event_study_m4_table <- tidy(event_study_m4) %>%
  mutate(signif = stars.pval(p.value))
event_study_m5_table <- tidy(event_study_m5) %>%
  mutate(signif = stars.pval(p.value))

write_csv(event_study_m1_table, "outputs/event_study_m1_genetic_defor_annual_12Feb2024_original.csv") #update date
write_csv(event_study_m2_table, "outputs/event_study_m2_genetic_defor_annual_4Mar2023.csv") #update date
write_csv(event_study_m3_table, "outputs/event_study_m3_genetic_defor_annual_4Mar2023.csv") #update date
write_csv(event_study_m4_table, "outputs/event_study_m4_genetic_defor_annual_4Mar2023.csv") #update date
write_csv(event_study_m5_table, "outputs/event_study_m5_genetic_defor_annual_4Mar2023.csv") #update date

#save as RDS files
saveRDS(event_study_m1, file = "outputs/event_study_m1.rds")
saveRDS(event_study_m2, file = "outputs/event_study_m2.rds")
saveRDS(event_study_m3, file = "outputs/event_study_m3.rds")
saveRDS(event_study_m4, file = "outputs/event_study_m4.rds")
saveRDS(event_study_m5, file = "outputs/event_study_m5.rds")

#summarize multiple models in a single table
library(fixest)
event_study_comparison <- etable(event_study_m1, event_study_m1a, event_study_m2, event_study_m3, event_study_m4,event_study_m5)
write_csv(event_study_comparison, "outputs/event_study_comparison_10Mar2023.csv") #update date

##Plot coefficients---------------------------------------
library(tidyverse)
library(broom)
library(fixest)

#faster way to do this
summary(event_study_m1)
coefplot(event_study_m1, 
         keep = "CFM1:yrs_post_crisis",
         main = "Effect of CFM:yrs_post_crisis on annual deforestation",
         value.lab = "Estimate and __ci__ Conf. Int.",
         ylab = NULL,
         xlab = "Years post crisis")
summary(event_study_m2)
coefplot(event_study_m2, keep = "CFM:yrs_post_crisis(.+):disturb")


#Plot event study model (m1) --------------------
summary(event_study_m1)
Fig.data.1 <- tidy(event_study_m1)     # event_study_m1 is the first Event Study model
Fig.data.1 <- Fig.data.1[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.1 <-  data.frame(cbind(Fig.data.1, confint(event_study_m1)))    #  Add CIs in the data frame
names(Fig.data.1) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
which(Fig.data.1$Variable=="CFM:yrs_post_crisis1") #Identify variables of interest: 22
which(Fig.data.1$Variable=="CFM:yrs_post_crisis11") #32
Fig.data.1 <- Fig.data.1[22:32, ]  # select only the variables I want to plot
Fig.data.1 <- Fig.data.1 %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.1$CFM_year <- as.factor(Fig.data.1$CFM_year) #convert CFM_year to a factor
#write to CSV
write_csv(Fig.data.1, "outputs/Fig.data.m1_genetic_defor_annual_27Feb2023.csv") #update date
#plot
ggplot(Fig.data.1, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of interaction of CFM and years post crisis")+
  ggtitle("Coefficients of interaction of CFM and years post crisis on annual deforestation")+
  theme_minimal()

#Plot event study model, with interaction term for distance from urban center (m2)
summary(event_study_m2)
Fig.data.2 <- tidy(event_study_m2)
Fig.data.2 <- Fig.data.2[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.2 <-  data.frame(cbind(Fig.data.2, confint(event_study_m2)))    #  Add CIs in the data frame
names(Fig.data.2) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
which(Fig.data.2$Variable=="CFM:yrs_post_crisis1:disturb") #44
which(Fig.data.2$Variable=="CFM:yrs_post_crisis11:disturb") #54 
Fig.data.2 <- Fig.data.2[44:54, ]  # select only the variables I want to plot
Fig.data.2 <- Fig.data.2 %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.2$CFM_year <- as.factor(Fig.data.2$CFM_year) #convert CFM_year to a factor
#write to CSV
write_csv(Fig.data.2, "outputs/Fig.data.m2_genetic_defor_annual_4Mar2023.csv") #update date
#plot
ggplot(Fig.data.2, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  #scale_y_continuous(labels = scales::percent)+ #note values are tiny
  labs(x = "Year", y = "Coefficient of interaction of CFM, yrs post crisis, and distance from urban center")+
  ggtitle("Coefficients of interaction of CFM, yrs post crisis, urban distance on annual deforestation")+
  theme_minimal()

#Plot event study model, with interaction term for development (m3)
summary(event_study_m3)
Fig.data.3 <- tidy(event_study_m3)
Fig.data.3 <- Fig.data.3[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.3 <-  data.frame(cbind(Fig.data.3, confint(event_study_m3)))    #  Add CIs in the data frame
names(Fig.data.3) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
which(Fig.data.3$Variable=="CFM:yrs_post_crisis1:development1") #44
which(Fig.data.3$Variable=="CFM:yrs_post_crisis11:development1") #54 
Fig.data.3 <- Fig.data.3[44:54, ]  # select only the variables I want to plot
Fig.data.3 <- Fig.data.3 %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.3$CFM_year <- as.factor(Fig.data.3$CFM_year) #convert CFM_year to a factor
#write to CSV
write_csv(Fig.data.3, "outputs/Fig.data.m3_genetic_defor_annual_27Feb2023.csv") #update date
#plot
ggplot(Fig.data.3, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of interaction of CFM, years post crisis, and development")+
  ggtitle("Coefficients of interaction of CFM, years post crisis, development on annual deforestation")+
  theme_minimal()

#Plot event study model, with interaction term for security (m4)
summary(event_study_m4)
Fig.data.4 <- tidy(event_study_m4)
Fig.data.4 <- Fig.data.4[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.4 <-  data.frame(cbind(Fig.data.4, confint(event_study_m4)))    #  Add CIs in the data frame
names(Fig.data.4) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
which(Fig.data.4$Variable=="CFM:yrs_post_crisis1:security1") #44
which(Fig.data.4$Variable=="CFM:yrs_post_crisis11:security1")  #54
Fig.data.4 <- Fig.data.4[44:54, ]  # select only the variables I want to plot
Fig.data.4 <- Fig.data.4 %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.4$CFM_year <- as.factor(Fig.data.4$CFM_year) #convert CFM_year to a factor
#write to CSV
write_csv(Fig.data.4, "outputs/Fig.data.m4_genetic_defor_annual_27Feb2023.csv") #update date
#plot
ggplot(Fig.data.4, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of interaction of CFM, years post crisis, and security")+
  ggtitle("Coefficients of interaction of CFM, years post crisis, security on annual deforestation")+
  theme_minimal()

#Plot event study model, with interaction term for population density (m5)
summary(event_study_m5)
Fig.data.5 <- tidy(event_study_m5)
Fig.data.5 <- Fig.data.5[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.5 <-  data.frame(cbind(Fig.data.5, confint(event_study_m5)))    #  Add CIs in the data frame
names(Fig.data.5) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
which(Fig.data.5$Variable=="CFM:yrs_post_crisis1:pop") #45
which(Fig.data.5$Variable=="CFM:yrs_post_crisis11:pop")  #55
Fig.data.5 <- Fig.data.5[45:55, ]  # select only the variables I want to plot
Fig.data.5 <- Fig.data.5 %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.5$CFM_year <- as.factor(Fig.data.5$CFM_year) #convert CFM_year to a factor
#write to CSV
write_csv(Fig.data.5, "outputs/Fig.data.m5_genetic_defor_annual_4Mar2023.csv") #update date
#plot
ggplot(Fig.data.5, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of interaction of CFM, years post crisis, and population")+
  ggtitle("Coefficients of interaction of CFM, years post crisis, population on annual deforestation")+
  theme_minimal()
