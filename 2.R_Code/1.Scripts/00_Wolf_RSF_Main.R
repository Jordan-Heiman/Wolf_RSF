#   WILD 562 - Wolf RSF                                                     ####

# Author: Jordan Heiman

# Date:2023-02-20 

# Purpose: This set of scripts is meant to help develop a conceptual framework 
# for modeling wolf habitat use and selection in order to identify habitat for 
# wolves as a function of environmental and 'biotic' factors. This is a 
# compilation of labs 2-6 of University of Montana's Wildlife Habitat Modeling 
# course with Dr. Mark Hebblewhite (WILD 562)

################################################################################
# Setup                                                                     ####
# Line to set working directory/install and load here package
if (!require("here", character.only = TRUE)) {
  install.packages("here", dependencies = TRUE)
  library("here", character.only = TRUE)
}

################################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
#        Load packages via sourcing load package script:
source(here("2.R_Code", "2.Functions", "01_Wolf_RSF_Packages.R"))

#      Functions                                                            ####
source(here("2.R_Code", "2.Functions", "02_Wolf_RSF_Prep_Spatial_Cov.R"))
source(here("2.R_Code", "2.Functions", "03_Wolf_RSF_Home_Range.R"))
source(here("2.R_Code", "2.Functions", "04_Wolf_RSF_GLM_Models.R"))

#      Data                                                                 ####
cov_shp_lst <- lapply(c("elc_habitat.shp"), 
                      function(x) here("1.Data", x))
cov_cont_rast_lst <- list.files(here("1.Data"),
                           pattern = "*.tif$",
                           full.names = TRUE)
cov_cat_rast_lst <- here("1.Data", "landcover")

# This table could be change if different categories need to be merged, etc. Just 
# needs to be a data.frame with the id in one column and the name of the habitat 
# type or group it belongs to
landcov_cats <- data.frame(id = 0:16, 
                           habitat_type = c(NA, "Open Conifer", "Moderate Conifer", 
                                            "Closed Conifer", "Deciduous", 
                                            "Mixed Forest", "Regeneration", 
                                            "Herbaceous", "Shrub", "Water",
                                            "Rock-Ice","Cloud", "Burn-Forest", 
                                            "Burn-Grassland", "Burn-Shrub", 
                                            "Alpine Herb", "Alpine Shrub"))
wolf_shp <- here("1.Data", "wolfyht.shp")
  
################################################################################
#   Lab 2.1 - Prepare spatial covariate rasters                             ####
cov_rasters <- prep_sp_cov(cov_shp = cov_shp_lst,
                           cov_cont_rast = cov_cont_rast_lst,
                           cov_cat_rast = cov_cat_rast_lst,
                           cov_cont_rast_names = c("high_hum", "hum_acc", "elev"),
                           cov_cat_rast_names = c("landcov"),
                           cat_att_tbl = lst(landcov = landcov_cats),
                           sa_ext = c(xmin = 443680.6, 
                                      xmax = 650430.4, 
                                      ymin = 5618416, 
                                      ymax = 5789236),
                           ras_res = 30,
                           pref_crs = "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#   Lab 2.2/2.3 - Select random available points                            ####
all_pts <- home_range_pts(ani_shp = wolf_shp,
                          group_col = "Pack",
                          coord_col = c("EASTING", "NORTHING"),
                          pref_crs = "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")








#   Lab 2.4 - Extract raster values for available points                    ####
all_pts_covs <- lapply(all_pts, 
                       function(y) terra::extract(cov_rasters, 
                                                  st_as_sf(y),
                                                  xy = TRUE))

#   Lab 3 - Univariate models                                               ####
uni_mods <- glm_mods(pts_df_lst = all_pts_covs)





# list of all the coefficients for all the covariates
lapply(uni_mods, function(x) lapply(x, function(y) summary(y)$coefficients[, 1:2]))

#   [Title]                                              ####
