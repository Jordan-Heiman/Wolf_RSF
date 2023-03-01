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
source(here("2.R_Code", "2.Functions", "04_Wolf_RSF_Point_Setup.R"))
source(here("2.R_Code", "2.Functions", "05_Wolf_RSF_GLM_Models.R"))

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
#   Lab 2.1/4 - Prepare spatial covariate rasters                           ####
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

#   Lab 2.4 - Extract raster values for available points, set up pt table   ####
all_pt_covs <- pt_setup(pts = all_pts,
                        rast = cov_rasters,
                        by.group = TRUE)

#   Lab 3/4 - Univariate models                                             ####
#   NOTE: change by.group to create models by pack or groupings             ####
uni_mods <- glm_mods(pt_sets = all_pt_covs)








# Things may be broken past here




#   Lab 5 - Collinearity/correlation testing                                ####
# Using function provided by Hebblewhite and created by Bill Venables; 
# https://stat.ethz.ch/pipermail/r-help/2001-November/016201.html 
cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, use = "complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  Rstar <- ifelse(R[above]<0.05, "***", "NS")
  R[above] <- paste(R[above], Rstar)
  R
}

# This shouldn't have to change even if covariates change because there are
# always 5 columns created by the previous functions and the first column is 
# an ID for the point, and all the tables should have the same columns
all_cov_lst <- names(all_pt_covs[[1]])[2:(length(names(all_pt_covs[[1]]))-5)]

# Need to not include any categorical covariates
cov_cat_names <- c("landcov")
cov_lst <- all_cov_lst[!(all_cov_lst %in% cov_cat_names)]

# Correlation tables, no threshold determined
cor_tbls <- lapply(all_pt_covs, function(x) cor.prob(as.matrix(x[,..cov_lst])))

reformulate(cov_lst, "used")

# could then have a list of covariates that are correlated and somehow make sure 
# that those are not duplicated in any multivariate models would need to set a 
# threshold for how much collinearity is too much







# List of all the coefficients for all the covariates
#lapply(uni_mods, function(x) summary(x)$coefficients[, 1:2])