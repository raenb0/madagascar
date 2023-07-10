# calculate area of CFM polygons
# 5 June 2023

library(terra)

setwd("C:/Users/raenb/Box/Documents/GIS/_Madagascar_Political_Conflict_Analyses/_working_folder")

#calculate area of all CFM
CFM_all <- vect("CFM_all_UTM.shp") #re-projected layer to UTM
CFM_all_df <- as.data.frame(CFM_all) #1,019 count of all CFM sites
area_cfm_ha <- expanse(CFM_all, unit="ha") #calculate area in hectares
total_area_cfm_ha <- sum(area_cfm_ha) #3,117,334 ha area of all CFM
area_cfm_km2 <- expanse(CFM_all, unit="km") #calculate area in sq km
total_area_cfm_km2 <- sum(area_cfm_km2) #31,173.34 sq km, makes sense

#calculate area of CFM established prior to 2005
CFM_pre05 <- vect("CFM_pre05_UTM.shp")
CFM_pre05 <- as.data.frame(CFM_pre05) # 362 count of CFM established prior to 2005
area_cfm_pre05 <- expanse(CFM_pre05, unit="ha")
total_area_cfm_pre05 <- sum(area_cfm_pre05) #1,096,874 ha  area of CFM established before 2005

#calculate area of CFM established prior to 2005 and renewed
CFM_pre05_rnw <- vect("CFM_pr05_rnwd.shp")
area_cfm_pre05_rnw <- expanse(CFM_pre05_rnw, unit="ha")
total_area_cfm_pre05_rnw <- sum(area_cfm_pre05_rnw) #471,007.8 ha  area of CFM established before 2005
