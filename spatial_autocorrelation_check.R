# check for spatial autocorrelation ---------------------
# 29 Jan 2024

# check distances between observations in unique CFM or MNP
library(tidyverse)

#load data
matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_genetic_defor_annual_development_security_4Mar2023.csv') #update date

names(matched_yr_wider)

matched_selected <- matched_yr_wider %>% 
  select(UID,x,y,cfmidcorrected,paidcorrected) 
matched_distinct <- distinct(matched_selected) #each UID only has a single row
write_csv(matched_distinct, "outputs/matched_distinct.csv")

# # from https://stackoverflow.com/questions/48389027/calculating-the-distance-between-points-in-r
# library(raster)
# m <- pointDistance(matched_distinct[, c("x", "y")], lonlat=TRUE)
# 
# # To get the nearest point to each point, you can do
# mm <- as.matrix(as.dist(m))
# diag(mm) <- NA
# i <- apply(mm, 1, which.min)
# 
# #to get the distances:
# apply(mm, 1, min, na.rm=TRUE)