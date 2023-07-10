# Plotting matched and unmatched covariate data, unmatched deforestation data
# 5 November 2022

library(ggplot2)
library(tidyverse)
library(dplyr)

#load data
#unmatched data
cfm_pa_data <- read_csv('outputs/cfm_pa_data_90m_no_na_5Nov2022.csv') #update date
cfm_rnw_pa_data <- read_csv('outputs/cfm_rnw_pa_data_90m_no_na_5Nov2022.csv') #update date

#matched data
matched_data <- read_csv('outputs/matched_yr_wider_exchange_5Nov2022.csv') #update date

#histograms of unmatched data
names(cfm_pa_data)
dist_cart_hist1 <- ggplot(data = cfm_pa_data, aes(x = dist_cart)) + 
  geom_histogram()  + 
  ggtitle("Distance from cart tracks for UNmatched data") + 
  xlab("Distance from cart tracks (m)")
dist_cart_hist1

#histograms of matched data
names(matched_data)

dist_cart_hist2 <- ggplot(data = matched_data, aes(x = distcart)) + 
  geom_histogram()  + 
  ggtitle("Distance from cart tracks for matched data") + 
  xlab("Distance from cart tracks (m)")  + xlim(c(0, 25000))
dist_cart_hist2

dist_road_hist <- ggplot(data = matched_data, aes(x = distroad)) + 
  geom_histogram()
dist_road_hist

# mean annual deforestation (matched data)
matched_data %>%
  group_by(PACFM, year) %>%
  summarize(defor_mean = mean(defor)) %>%
  ggplot(mapping = aes(x = year, y = defor_mean, color=PACFM)) + 
  geom_point() +
  geom_line() +
  ggtitle ("Average annual deforestation in CFM and MNP, matched data") +
  ylab("Avg annual deforestation (%)") +
  xlab("Year")


# Reorganize unmatched data -----------------------------------------------

library(tidyverse)
library(dplyr)

#load tabular data if needed
#unmatched data
cfm_pa_data <- read_csv('outputs/cfm_pa_data_90m_no_na_5Nov2022.csv') #update date

names(cfm_pa_data)

#subset data to split up PA and CFM data

#for PA data:
PA_data <- cfm_pa_data %>%
  filter(CFM==0)

#for CFM data:

CFM_data <- cfm_pa_data %>%
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
unmatched_data_bind <- rbind(PA_data, CFM_data)
View(unmatched_data_bind)

#drop defor years that we're not including (2001-2004, 2021)
names(unmatched_data_bind)
unmatched_data_bind <- unmatched_data_bind %>% select(-c(defor2001, defor2002, defor2003, defor2004, defor2021))
names(unmatched_data_bind)

#rename rice price data to match other time variant variable names
unmatched_data_bind <- unmatched_data_bind %>%
  rename_with(~ gsub('_', '', .x)) #removes underscores from rice_av_ and rice_sd_
names(unmatched_data_bind)

# reorganize based on https://dcl-wrangle.stanford.edu/pivot-advanced.html
#this works but the time variant variables are all in a single column, "timevariant"

unmatched_data_longer <- unmatched_data_bind %>%
  pivot_longer(
    cols = defor2005:wind2020,
    names_to = c("timevariant", "year"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "timevariant_values"
  )

#View
View(unmatched_data_longer)

#now pivot wider to get time variant variables as columns instead of rows
unmatched_data_wider <- unmatched_data_longer %>%
  pivot_wider(
    names_from = "timevariant",
    values_from = "timevariant_values"
  )

#View
View(unmatched_data_wider)

#write to CSV
write_csv(unmatched_data_wider,'outputs/unmatched_data_wider_7Nov2022.csv') #update date!


# Plot unmatched deforestation data ---------------------------------------

# mean annual deforestation (matched data)
unmatched_data_wider %>%
  group_by(year, PACFM) %>%
  summarize(defor_mean = mean(defor)) %>%
  ggplot(mapping = aes(x = year, y = defor_mean, color=PACFM)) + 
  geom_point() +
  geom_line() +
  ggtitle ("Average annual deforestation in CFM and MNP, UNmatched data") +
  ylab("Avg annual deforestation (%)") +
  xlab("Year")
#Message: geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?