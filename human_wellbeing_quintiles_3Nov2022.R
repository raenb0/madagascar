# convert Madagascar HWB data (Wu Yang, Conservation International) into quintiles
#November 3 2022

library(tidyverse)
library(dplyr)

setwd("C:/Users/raenb/Documents/GitHub/madagascar") # set working directory

#load data

hwb <- read_csv("data/HWB_attribute_table.csv")
names(hwb)

pctiles <- seq(0, 1, 0.2)


#add a new variable, Q1_quintile, representing the quintile values of Q1 (index of basic material for good life)
hwb_quintile <- hwb %>% mutate(Q1_quintile = cut(Q1, 
                          quantile(Q1, 
                                   probs = pctiles), 
                          labels = pctiles[2:length(pctiles)]*5)) #multiplies quintile (0.2-1.0) by 5 to get integer (1-5)

hwb_quintile <- hwb_quintile %>% mutate(Q1_quintile = as.numeric(Q1_quintile))
hwb_quintile <- hwb_quintile %>% dplyr::mutate(Q1_quintile = replace_na(Q1_quintile,0))

#convert v7_mm, indicator of security conditions and risk of theft of property, from percentages  to integer
unique(hwb$v7_mm) #(0, 0.25, 0.50, 0.75, 1.0)

hwb_quintile <- hwb_quintile %>% dplyr::mutate(v7_quintile = v7_mm*4) #multiply by 4 to convert to integer

hwb_subs <- hwb_quintile %>% select(Q1, Q1_quintile, v7_mm, v7_quintile) #check results, look good

write_csv(hwb_quintile, 'outputs/hwb_quintile.csv')

