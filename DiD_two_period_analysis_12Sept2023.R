# TWO-PERIOD DiD ANALYSIS -----------------------
#updated Sept 12 2023 to re-run two-period DiD analysis

library(tidyverse)
library(dplyr)

#load tabular data if needed

matched <- read_csv('outputs/genetic.matches_26Feb2023.csv')  #load data, update date
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
write_csv(matched_bind,'outputs/matched_bind_2pr_90m_12Sept2023.csv') #update date

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
write_csv(matched_wider,'outputs/matched_wider_2pr_90m_12Sept2023.csv') #update date

## SPECIFY THE TWO-PERIOD MODEL, DEFORESTATION OUTCOME, CLUSTERED SE -------------------
#replace UID with cluster? ask Ranaivo/Chris

library(fixest)

did_m1_2pr <- feols(defor ~ CFM*period + edge + pop + riceav + ricesd + drght + maxprecip + temp + wind | UID, data = matched_wider, cluster = "cluster") #note I used "edge" here (distance from forest edge in the beginning of each period), could I use "distance" (average distance from forest edge across each period?) maybe not
#remove FE for UID, add cluster = "cluster"

summary(did_m1_2pr)

#tidy coefficients table and save to csv
library(broom)
library(gtools)

did_m1_2pr_table <- tidy(did_m1_2pr) %>%
  mutate(signif = stars.pval(p.value))

write_csv(did_m1_2pr_table, "outputs/did_m1_2pr_coefftable_12Sept2023.csv") #update date
