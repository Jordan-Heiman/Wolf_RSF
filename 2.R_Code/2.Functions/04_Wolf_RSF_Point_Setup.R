### WILD 562 - Wolf RSF
### Jordan Heiman
## Date: 2023-02-21

## Function purpose: Bring together wolf used and available points for both 
# packs and for KDE and MCP home ranges with corresponding covariates

#################################### Intro #####################################

# Name: 04_Wolf_RSF_Point_Setup
# Description:   Extract covariates for all points in the data set and all 
# available points then bring together all the points create a list of 2 tables. 
# If by.group = F then there will be one table for KDE and one for MCP home 
# ranges, if by.group = T then there will be a table for each group by KDE and 
# by MCP home ranges

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-02-21

################################# Arguments ####################################

# pts
#       Spatial layers of points that have been used by individuals as well as 
#       available points for each group and home range type

# rast
#       Stacked raster of all the covariates that will be used

# by.group
#       A logical operator for whether the data and models should be separated by 
#       grouping such as herds or packs similar to previous functions in this 
#       series

################################# Output #######################################

# pt_sets
#       A list of tables containing used and available points for each home range 
#       type and, if by.group = T, for each group

################################################################################
## Function

pt_setup <- function(pts,
                     rast,
                     by.group = FALSE){
  
  # Extract raster values for all points
  pts_df_lst <- lapply(pts, 
                       function(y) terra::extract(cov_rasters, 
                                                  st_as_sf(y),
                                                  xy = TRUE))
  
  # Based on the length of the points data.frame list, determine the number and 
  # names of the groups in the data (i.e. packs or herds). There are 3 types of 
  # point sets: KDE, MCP, and used. Therefore the length of the list divided by 
  # 3 gives the number of groups (j) and the last j data frames will be named 
  # after the groups.
  group_count <- length(pts_df_lst)/3
  group_names <- names(pts_df_lst)[(length(pts_df_lst)-group_count+1):length(pts_df_lst)]
  
  # This would probably all be easier to do when the data was first set up
  for (i in 1:length(pts_df_lst)){
    
    # Check for type of points (available points from MCP, or KDE home ranges, 
    # or Used points)
    if (str_detect(names(pts_df_lst)[[i]], "mcp")){
      pts_df_lst[[i]] <- mutate(pts_df_lst[[i]], type = "mcp")
    } else if (str_detect(names(pts_df_lst)[[i]], "kde")){
      pts_df_lst[[i]] <- mutate(pts_df_lst[[i]], type = "kde")
    } else {
      pts_df_lst[[i]] <- mutate(pts_df_lst[[i]], type = "used")
    }
    
    # Then check for which group the points belong to
    for (j in 1:group_count){
      if (str_detect(names(pts_df_lst)[[i]], group_names[[j]])){
        pts_df_lst[[i]] <- mutate(pts_df_lst[[i]], group = group_names[[j]])
      }
    }
  }
  
  # It will be easier to just join all the data.frames them separate them back 
  # out into the desired sets
  all_pts <- rbindlist(pts_df_lst)
  
  # Set up a data frame for KDE home range points with used points
  kde_pts <- all_pts[type == "kde"
                     | type == "used", 
                     # Set up a used/available column with used == 1 and 
                     # available == 0, using these because it lines up best for 
                     # some data analysis
                     ][, used := ifelse(type == "used", 1, 0)
                       # Also set up a used column that is entered as a factor 
                       # type for other kinds of data analysis
                       ][, used_fac := factor(used, 
                                             labels = c("Available", "Used"))
                         # Remove the unnecessary type column
                         ][, type := NULL]
  
  # Set up a data frame for MCP home range points with used points
  mcp_pts <- all_pts[type == "mcp"
                     | type == "used", 
                     # Set up a used/available column with used == 1 and 
                     # available == 0, using these because it lines up best for 
                     # some data analysis
                     ][, used := ifelse(type == "used", 1, 0)
                       # Also set up a used column that is entered as a factor 
                       # type for other kinds of data analysis
                       ][, used_fac := factor(used, 
                                            labels = c("Available", "Used"))
                         # Remove the unnecessary type column
                         ][, type := NULL]
  
  # Depending on whether by.groups is false or true, set up models 
  if (by.group == FALSE){
    
    # Put the data.tables back into a list so that each can be altered the same way
    pt_sets <- list(kde = kde_pts,
                    mcp = mcp_pts)
    
  } else if (by.group == TRUE){
    
    # Put the data.tables back into a list so that each can be altered the same way
    grouped_pt_sets <- list(kde = kde_pts,
                            mcp = mcp_pts) %>% 
      lapply(function(x) split(x, f = x$group))
    
    pt_sets <- unlist(grouped_pt_sets, recursive = FALSE)
    
  } else {stop("by.group must be either TRUE or FALSE")}
  
  return(pt_sets)
  
  }
  