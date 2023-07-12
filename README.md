# Madagascar political crisis effect on community forests

Code associated with paper: "The effect of a political crisis on performance of community- and state-managed forests in Madagascar"

Authors: Rachel A. Neugarten, Ranaivo A. Rasolofoson, Christopher B. Barrett, Ghislain Vieilledent, Amanda D. Rodewald

Spatial data (raster tif files) used for this analysis available via https://zenodo.org/record/8132923

Selected tabular data outputs are contained within sub-folder "outputs_to_share""

Primary code used for analysis includes:

01_extract_raster_values_to_points   - Extracting raster values to sample points

02a_Matchit_data_wrangling_allCFM_90m  - Statistical matching analysis and data formatting (all CFM, 90m resolution)

02b_MatchIt_data_wrangling_renewedCFM - Statistical matching analysis and data formatting (renewed CFM, 90m resolution)

03a_Event_study_parallel_trends_allCFM_90m - Event study and parallel trends analysis (all CFM, 90m resolution)

03b_Event_study_parallel_trends_renewedCFM_90m - Event study and parallel trends analysis (renewed CFM, 90m resolution)

Misc other code includes:

calculate_area_CFM_polygons - calculating area of CFM sites in Madagascar

human_wellbeing_quintiles - identifying quintiles of human well-being (development and security) data

parking_lot - code that was no longer used in analysis

plot_defor_matched_CFM_MNP - plotting deforestation trends in CFM and matched MNP forests

plot_matched_unmatched_covariates_data - plotting covariate data