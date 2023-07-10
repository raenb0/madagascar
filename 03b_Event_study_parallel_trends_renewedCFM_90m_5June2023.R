# Madagascar impact evaluation
# Event study, parallel trends test, renewed CFM, 90m
# Rachel Neugarten
# 5 June 2023


## Check parallel trends in the pre-crisis period, renewed CFM data --------------------------
library(tidyverse)
library(fixest)

matched_yr_rnw_wider <- read_csv('outputs/matched_yr_rnw_wider_90m_genetic_defor_annual_development_security_2Mar2023.csv') #update date
names(matched_yr_rnw_wider)
matched_yr_rnw_pre_crisis <- matched_yr_rnw_wider %>% filter(year < 2010) #note year is not a factor in this version
matched_yr_rnw_pre_crisis$year <- as.factor(matched_yr_rnw_pre_crisis$year) #convert "year" to a factor

#parallel trends check, ANNUAL DEFORESTATION
did_m1_yr_rnw_pre_crisis <- feols(defor_annual ~ CFM*year 
                                  + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                                  | UID + year,
                                  data = matched_yr_rnw_pre_crisis,
                                  cluster = "cluster") #only pre-crisis years 2005-2009
summary(did_m1_yr_rnw_pre_crisis)
#the coefficient of CFM:year is only marginally significant in 2008, so we have established parallel trends***

#tidy coefficients table and save to csv
library(broom)
library(gtools)

parallel_trends_m1_rnw_table <- tidy(did_m1_yr_rnw_pre_crisis) %>%
  mutate(signif = stars.pval(p.value))
write_csv(parallel_trends_m1_rnw_table, "outputs/parallel_trends_m1_rnw_table_defor_annual_2Mar2023.csv") #update date


## Event Study model, Renewed CFM data ---------------------------------------------------
library(tidyverse)
library(fixest)

#load data if necessary
matched_yr_rnw_wider <- read_csv('outputs/matched_yr_rnw_wider_90m_genetic_defor_annual_development_security_2Mar2023.csv') #update date

names(matched_yr_rnw_wider)

matched_yr_rnw_wider$yrs_post_crisis <- as.factor(matched_yr_rnw_wider$yrs_post_crisis) #convert "yrs_post_crisis" to a factor
matched_yr_rnw_wider$year <- as.factor(matched_yr_rnw_wider$year) #convert "year" to a factor

#relevel development so reference = 1 instead of 0
matched_yr_rnw_wider$development <- as.factor(matched_yr_rnw_wider$development) #convert to factor
matched_yr_rnw_wider <- within(matched_yr_rnw_wider, development <- relevel(development, ref = 1))

#relevel security so reference = 1 instead of 0
matched_yr_rnw_wider$security <- as.factor(matched_yr_rnw_wider$security) #convert to factor
matched_yr_rnw_wider <- within(matched_yr_rnw_wider, security <- relevel(security, ref = 1))

# ### DiD model, FE for vegtype, outcome variable: ANNUAL DEFOR -------------------
# did_m1_rnw <- feols(defor_annual ~ CFM*year
#                 + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
#                 | UID + year,
#                 data = matched_yr_rnw_wider,
#                 cluster = "cluster")
# 
# summary(did_m1_rnw)

## Event study model, renewed CFM data, ANNUAL DEFOR ----------------
event_study_m1_rnw <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                            + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
                            | UID,
                            data = matched_yr_rnw_wider,
                            cluster = "cluster")

summary(event_study_m1_rnw)

### Event study model 2, interaction term: distance to urban center, outcome variable: ANNUAL DEFOR ------------------------
event_study_m2_rnw <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                            + CFM*yrs_post_crisis*disturb #interaction term for distance to urban center
                            + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
                            | UID,
                            data = matched_yr_rnw_wider,
                            cluster = "cluster")
summary(event_study_m2_rnw)

### Event study model 3, with interaction term for development, ANNUAL DEFOR -------
event_study_m3_rnw <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis 
                            + CFM*yrs_post_crisis*development #interaction term for development
                            + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind 
                            | UID,
                            data = matched_yr_rnw_wider,
                            cluster = "cluster")
summary(event_study_m3_rnw)

### Event study model 4, with interaction term for security, ANNUAL DEFOR -----------------------
event_study_m4_rnw <- feols(defor_annual ~ CFM*timestep + CFM*yrs_post_crisis
                            + CFM*yrs_post_crisis*security #interaction term for security
                            + distance + pop + riceav_mdg + ricesd_mdg + drght + precip + temp + wind
                            | UID,
                            data = matched_yr_rnw_wider,
                            cluster = "cluster")
summary(event_study_m4_rnw)


### tidy coefficient tables and write to CSV files, renewed CFM --------------
library(broom)
library(gtools)

#did_m1_rnw_table <-tidy(did_m1_rnw) %>%
#  mutate(signif = stars.pval(p.value))
#write_csv(did_m1_rnw_table, "outputs/did_m1_rnw_genetic_defor_annual_22Feb2023.csv") #update date

#event study
event_study_m1_rnw_table <- tidy(event_study_m1_rnw) %>%
  mutate(signif = stars.pval(p.value))
event_study_m2_rnw_table <- tidy(event_study_m2_rnw) %>%
  mutate(signif = stars.pval(p.value))
event_study_m3_rnw_table <- tidy(event_study_m3_rnw) %>%
  mutate(signif = stars.pval(p.value))
event_study_m4_rnw_table <- tidy(event_study_m4_rnw) %>%
  mutate(signif = stars.pval(p.value))
write_csv(event_study_m1_rnw_table, "outputs/event_study_m1_rnw_genetic_defor_annual_2Mar2023.csv") #update date
write_csv(event_study_m2_rnw_table, "outputs/event_study_m2_rnw_genetic_defor_annual_2Mar2023.csv") #update date
write_csv(event_study_m3_rnw_table, "outputs/event_study_m3_rnw_genetic_defor_annual_2Mar2023.csv") #update date
write_csv(event_study_m4_rnw_table, "outputs/event_study_m4_rnw_genetic_defor_annual_2Mar2023.csv") #update date


## Plot coefficients, renewed CFM ---------------------------------------
library(broom)
library(ggplot2)

#Plot event study model (m1), renewed CFM, ANNUAL DEFOR
summary(event_study_m1_rnw)
Fig.data.1.rnw <- tidy(event_study_m1_rnw)
Fig.data.1.rnw <- Fig.data.1.rnw[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.1.rnw <-  data.frame(cbind(Fig.data.1.rnw, confint(event_study_m1_rnw)))    #  Add CIs in the data frame
names(Fig.data.1.rnw) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
## In the next couple of lines I Identified the row numbers of the variables I want to plot (I do not want to plot all the coeffs of all variables.  #Just the variables of interest). The variables of interest are from row # 62 to 159
which(Fig.data.1.rnw$Variable=="CFM:yrs_post_crisis1") #22
which(Fig.data.1.rnw$Variable=="CFM:yrs_post_crisis11") #32
Fig.data.1.rnw <- Fig.data.1.rnw[22:32, ]  # select only the variables I want to plot
Fig.data.1.rnw <- Fig.data.1.rnw %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.1.rnw$CFM_year <- as.factor(Fig.data.1.rnw$CFM_year) #convert CFM_year to a factor
view(Fig.data.1.rnw)
write_csv(Fig.data.1.rnw, "outputs/Fig.data.1.rnw_genetic_defor_annual_13Feb2023.csv") #update date
#plot
ggplot(Fig.data.1.rnw, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of CFM:yrs_post_crisis")+
  ggtitle("Effect of CFM:yrs_post_crisis on annual deforestation, renewed CFM")+
  theme_minimal()

#Plot event study model, interaction term for distance from urban center (m2), renewed CFM, ANNUAL DEFOR
summary(event_study_m2_rnw)
Fig.data.2.rnw <- tidy(event_study_m2_rnw)  
Fig.data.2.rnw <- Fig.data.2.rnw[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.2.rnw <-  data.frame(cbind(Fig.data.2.rnw, confint(event_study_m2_rnw)))    #  Add CIs in the data frame
names(Fig.data.2.rnw) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
## In the next couple of lines I Identified the row numbers of the variables I want to plot (I do not want to plot all the coeffs of all variables.  #Just the variables of interest). The variables of interest are from row # 62 to 159
which(Fig.data.2.rnw$Variable=="CFM:yrs_post_crisis1:disturb") #44
which(Fig.data.2.rnw$Variable=="CFM:yrs_post_crisis11:disturb") #54 
Fig.data.2.rnw <- Fig.data.2.rnw[44:54, ]  # select only the variables I want to plot
Fig.data.2.rnw <- Fig.data.2.rnw %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.2.rnw$CFM_year <- as.factor(Fig.data.2.rnw$CFM_year) #convert CFM_year to a factor
#view(Fig.data.2.rnw)
write_csv(Fig.data.2.rnw, "outputs/Fig.data.2.rnw_genetic_defor_annual_5Mar2023.csv") #update date
#plot
ggplot(Fig.data.2.rnw, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of interaction of CFM, yrs post crisis, and urban distance")+
  ggtitle("Effect of CFM:yrs_post_crisis:urban distance on annual defor, renewed CFM")+
  theme_minimal()

#Plot event study model, interaction term for development (m3), renewed CFM, ANNUAL DEFOR
summary(event_study_m3_rnw)
Fig.data.3.rnw <- tidy(event_study_m3_rnw)  
Fig.data.3.rnw <- Fig.data.3.rnw[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.3.rnw <-  data.frame(cbind(Fig.data.3.rnw, confint(event_study_m3_rnw)))    #  Add CIs in the data frame
names(Fig.data.3.rnw) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
## In the next couple of lines I Identified the row numbers of the variables I want to plot (I do not want to plot all the coeffs of all variables.  #Just the variables of interest). The variables of interest are from row # 62 to 159
which(Fig.data.3.rnw$Variable=="CFM:yrs_post_crisis1:development1") #44
which(Fig.data.3.rnw$Variable=="CFM:yrs_post_crisis11:development1") #54 
Fig.data.3.rnw <- Fig.data.3.rnw[44:54, ]  # select only the variables I want to plot
Fig.data.3.rnw <- Fig.data.3.rnw %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.3.rnw$CFM_year <- as.factor(Fig.data.3.rnw$CFM_year) #convert CFM_year to a factor
view(Fig.data.3.rnw)
write_csv(Fig.data.3.rnw, "outputs/Fig.data.3.rnw_genetic_defor_annual_13Feb2023.csv") #update date
#plot
ggplot(Fig.data.3.rnw, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of interaction of CFM, years post crisis, and development")+
  ggtitle("Effect of CFM:yrs_post_crisis:development on annual deforestation, renewed CFM")+
  theme_minimal()


#Plot event study model, with interaction term for security and FE for vegtype (m4), ANNUAL DEFOR
summary(event_study_m4_rnw)
Fig.data.4.rnw <- tidy(event_study_m4_rnw)
Fig.data.4.rnw <- Fig.data.4.rnw[ ,1:2]   # select the first two columns, which are the variable names and the coeffs
Fig.data.4.rnw <-  data.frame(cbind(Fig.data.4.rnw, confint(event_study_m4_rnw)))    #  Add CIs in the data frame
names(Fig.data.4.rnw) <- c("Variable", "Coeff", "Low.CI", "High.CI")    # Re-name the columns
## In the next couple of lines I Identified the row numbers of the variables I want to plot (I do not want to plot all the coeffs of all variables.  #Just the variables of interest). The variables of interest are from row # 62 to 159
which(Fig.data.4.rnw$Variable=="CFM:yrs_post_crisis1:security1") #44
which(Fig.data.4.rnw$Variable=="CFM:yrs_post_crisis11:security1")  #54
Fig.data.4.rnw <- Fig.data.4.rnw[44:54, ]  # select only the variables I want to plot
Fig.data.4.rnw <- Fig.data.4.rnw %>%
  mutate(CFM_year = c(2010:2020))
Fig.data.4.rnw$CFM_year <- as.factor(Fig.data.4.rnw$CFM_year) #convert CFM_year to a factor
view(Fig.data.4.rnw)
write_csv(Fig.data.4.rnw, "outputs/Fig.data.4.rnw_genetic_defor_annual_13Feb2023.csv") #update date
#plot
ggplot(Fig.data.4.rnw, mapping = aes(CFM_year,Coeff))+
  geom_hline(yintercept=0, color = "grey", size=1)+
  geom_point()+
  geom_errorbar(aes(ymin=Low.CI, ymax=High.CI))+
  labs(x = "Year", y = "Coefficient of interaction of CFM, years post crisis, and security")+
  ggtitle("Effect of CFM:yrs_post_crisis:security on annual deforestation, renewed CFM")+
  theme_minimal()



## Rosenbaum bounds sensitivity analysis, renewed CFM -----------------------------------
# May be unnecessary, see note above
library(rbounds)

#load data if necessary
PA_data_yr_rnw <- read_csv('outputs/PA_data_yr_rnw_90m_13Feb2023.csv') #update date
CFM_data_yr_rnw <- read_csv('outputs/CFM_data_yr_rnw_90m_13Feb2023.csv') #update date

View(PA_data_yr_rnw)
View(CFM_data_yr_rnw)

#create an outcome variable for defor 2009-2014 (crisis period only)
PA_data_yr_rnw <- PA_data_yr_rnw %>%
  mutate(defor_2009_2014 = defor2014 - defor2009)
CFM_data_yr_rnw <- CFM_data_yr_rnw %>%
  mutate(defor_2009_2014 = defor2014 - defor2009)

hlsens(CFM_data_yr_rnw$defor_2009_2014, PA_data_yr_rnw$defor_2009_2014, pr = 0.5, Gamma = 1.1, GammaInc = 0.01)
#highly sensitive to bias (only a Gamma of 1.01 and the estimate bounds 0)

#create an outcome variable for defor 2005-2014 (pre-crisis and crisis period)
PA_data_yr_rnw <- PA_data_yr_rnw %>%
  mutate(defor_2005_2014 = defor2014 - defor2005)
CFM_data_yr_rnw <- CFM_data_yr_rnw %>%
  mutate(defor_2005_2014 = defor2014 - defor2005)

hlsens(CFM_data_yr_rnw$defor_2005_2014, PA_data_yr_rnw$defor_2005_2014, pr = 0.5, Gamma = 1.1, GammaInc = 0.01)
#highly sensitive to bias (only a Gamma of 1.01 and the estimate bounds 0)

#create an outcome variable for defor 2005-2020 (entire study period)
PA_data_yr_rnw <- PA_data_yr_rnw %>%
  mutate(defor_2005_2020 = defor2020 - defor2005)
CFM_data_yr_rnw <- CFM_data_yr_rnw %>%
  mutate(defor_2005_2020 = defor2020 - defor2005)

hlsens(CFM_data_yr_rnw$defor_2005_2020, PA_data_yr_rnw$defor_2005_2020, pr = 0.5, Gamma = 1.1, GammaInc = 0.01)
#highly sensitive to bias (only a Gamma of 1.01 and the estimate bounds 0)