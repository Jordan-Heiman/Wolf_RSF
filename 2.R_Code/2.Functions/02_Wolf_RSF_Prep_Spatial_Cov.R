### WILD 562 - Wolf RSF
### Jordan Heiman
## Date: 2023-02-20

## Function purpose: Prepare spatial covariate data for spatial analysis later, 
# this is objective 1 in lab 2

#################################### Intro #####################################

# Name: 02_Wolf_RSF_Prep_Spatial_Cov
# Description:   Clean up and prepare spatial covariate data for later analysis.

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-02-20

################################# Arguments ####################################

# cov_shp: 
#       List of shape file paths for any covariate shape files, these will get 
#       'rasterized' within the function

# cov_cont_rast:
#       List of raster files for any continuous variable rasters that will get 
#       'resampled' within function

# cov_cat_rast:
#       List of raster files for any categorical variable rasters that will get 
#       'resampled' within function

# cov_cont_rast_names:
#       Names for the continuous rasters in order which will be used for the names
#       of the SpatRasters that are created

# cov_cat_rast_names:
#       Names for the categorical rasters in order which will be used for the  
#       names of the SpatRasters that are created

# sa_ext: 
#       Study area extent for resampling rasters, must be using proper CRS and 
#       in format: 
#         c(xmin = #, 
#           xmax = #, 
#           ymin = #, 
#           ymax = #)

# ras_res:
#       Desired resolution of the rasters after resampling

# pref_crs:
#       Preferred CRS that will be used for all rasters for resampling 

################################# Output #######################################

# cov_rasters
#       List of SpatRaster objects for all covariates that have been resampled 
#       to match ext, resolution and CRS as provided in function arguments

################################################################################
## Function
prep_sp_cov <- function(cov_shp, 
                        cov_cont_rast,
                        cov_cat_rast,
                        cov_cont_rast_names,
                        cov_cat_rast_names,
                        sa_ext,
                        ras_res,
                        pref_crs){
  
  # Read in shapefiles
  cov_sf_lst <- lapply(as.list(cov_shp), st_read)
  
  # Read in ready made rasters 
  cov_cont_sr_lst <- lapply(cov_cont_rast, rast)
  names(cov_cont_sr_lst) <- cov_cont_rast_names
  cov_cat_sr_lst <- lapply(cov_cat_rast, rast)
  names(cov_cat_sr_lst) <- cov_cat_rast_names
  
  # Create a mask raster to use as a template for converting shape file data to 
  # rasters. First, create an empty raster
  mask.raster <- rast()
  
  # Set extent of that raster to cover all layers that will be included (from 
  # function arguments)
  ext(mask.raster) <- sa_ext	
  
  # Set the resolution to 30 m (use larger to speed up processing if needed, from
  # function arguments)
  res(mask.raster) <- ras_res
  
  # Match projection to preferred CRS provided in function arguments
  crs(mask.raster) <- pref_crs
  
  # Set all values of mask.raster to zero
  mask.raster[] <- 0
  
  # Make an empty list for storing all the rasters that the for loop will make
  raster_lst <- list()
    
  # Now use user input to rasterize the shp covariate layers to match the raster
  # mask extent, resolution, and CRS
  for (i in 1:length(cov_sf_lst)){
    
    col_names <- names(cov_sf_lst[[i]])
    showDialog(title = "Rasterize Attributes",
               message = "Please use the next prompts to select attributes to rasterize from each covariate shape file provided.")
    cov_names <- select.list(col_names, 
                             title = basename(cov_shp[[i]]),
                             graphics = TRUE,
                             multiple = TRUE)
    
    rasters <- lapply(cov_names, 
                      function(x) rasterize(cov_sf_lst[[i]],
                                            mask.raster,
                                            field = x))
    
    names(rasters) <- tolower(cov_names)
    
    raster_lst <- c(raster_lst,
                    rasters)
    
  }
  
  # In order to ensure everything lines up, resample the rasters that were 
  # loaded as rasters
  cov_sr_lst_resamp <- c(lapply(cov_cont_sr_lst, 
                                resample, 
                                y = mask.raster, 
                                method = "bilinear"),
                         lapply(cov_cat_sr_lst, 
                                resample, 
                                y = mask.raster, 
                                method = "near"))
  
  # Now add all the rasters to one list
  cov_rasters <- c(raster_lst, 
                   cov_sr_lst_resamp)
  
  # Rast() here stacks the rasters into one
  return(rast(cov_rasters))
  
}
