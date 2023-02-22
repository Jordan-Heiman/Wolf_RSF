### WILD 562 - Wolf RSF
### Jordan Heiman
## Date: 2023-02-20

## Function purpose: Begin looking at the home range of the wolf packs being 
# studied for this RSF analysis

#################################### Intro #####################################

# Name: 02_Wolf_RSF_Home_Range
# Description:   Create home ranges for the wolf packs being studied for an RSF 
# analysis. This will create 2 kinds of home ranges, Minimum Convex Polygon and
# Fixed Kernel in order to analyze space use of the wolf packs. The function 
# will return sampled random points within all these home ranges. This aligns 
# with objectives 2 and 3 of lab 2

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-02-20

################################# Arguments ####################################

# ani_shp:
#       The file path to the shape file of GPS points for the animals being
#       studied

# group_col:
#       The column from the attribute table of the animal shape file that is 
#       used to group animals into the desired groupings, such as packs or herds. 
#       This column should use unique identifiers for each group.

# coor_col:
#       The set of columns within the shape file that hold the coordinates used 
#       by the data. This should be in the format c("x", "y")

# pref_crs:
#       Preferred CRS that was used in the data

################################# Output #######################################

# samp_pts
#       A list of Spatial Points Data Frames for 1000 randomly selected points 
#       within each groups MCP and KDE home range polygons as well as the used 
#       points from each group

################################################################################
## Function

home_range_pts <- function(ani_shp,
                           group_col,
                           coord_col,
                           pref_crs){
  
  # Read in the animal shape file with GPS points
  ani_sf <- st_read(ani_shp)
  
  # Create a table of just the groupings (we are eliminating all the extra columns)
  groupings <- as.data.table(ani_sf)[, .SD, .SDcols = group_col] %>% 
    as.data.table()
  
  # This changes the data to a Spatial Points Data Frame which is what is
  # needed by the MCP function
  coordinates(groupings) <- as.data.table(ani_sf)[, .SD, .SDcols = coord_col]
  crs(groupings) <- pref_crs
  
  # Create 99% MCPs for each group
  group_mcp <- mcp(groupings, percent = 99)
  
  # Create 99% KDEs for each group
  group_kde <- kernelUD(groupings, grid = 30, extent = 0.5, same4all = TRUE)
  
  # Convert the kde to polygons for KDE
  group_kde_hr <- getverticeshr(group_kde)
  
  # Make a list with both the KDE polygons and teh MCP polygons
  hr_polys <- list(mcp = group_mcp,
                   kde = group_kde_hr)
  
  # Create a list of the different groups
  group_names <- c(group_kde_hr@data$id,
                   group_mcp@data$id) %>% 
    unique() %>% 
    sort()
                               
  # Create an empty list to populate with the sampled points
  samp_pts <- list()
  count <- 1
  
  for (i in 1:length(hr_polys)){
    
    type <- names(hr_polys)[[i]]
    
    # Generate 1000 points from each groups polygon
    for (j in 1:length(group_names)){
      
      samp <- spsample(hr_polys[[i]][hr_polys[[i]]@data$id == group_names[[j]]], 
                       1000, 
                       "random")
      
      samp_pts[[count]] <- samp
      names(samp_pts)[[count]] <- paste(type, group_names[[j]])
      count <- count + 1
    }
  }
  
  # Also want to have a table of the used points for each group
  # Create a list of the different groups distinguished in the group column
  groups <- as.data.table(ani_sf)[, .SD, .SDcols = group_col][[1]] %>%
    unique() %>%
    sort()

  # Create an empty list for storing the separated data
  ani_grouped <- list()

  # Create a list of separate data.tables for each group distinguished
  for (i in 1:length(groups)){

    # Pull out just a single group and create a data.table
    group_i <- ani_sf[which(as.data.table(ani_sf)[,
                                                  .SD == groups[i],
                                                  .SDcols = group_col]), ] %>%
      as.data.table()

    # Create a table of just the names (we are eliminating all the extra columns)
    group_i_sub <- group_i[, .SD, .SDcols = group_col]

    # This changes the data to a Spatial Points Data Frame which is what is
    # needed by the MCP function
    coordinates(group_i_sub) <- group_i[, .SD, .SDcols = coord_col]
    crs(group_i_sub) <- pref_crs

    # Add the data to a running list of the grouped data
    ani_grouped[[i]] <- group_i_sub

    # And give it a name that matches the group
    names(ani_grouped)[[i]] <- groups[[i]]

  }
  
  
  return(c(samp_pts, 
           ani_grouped))
  
}

# # If doing home ranges by individual this is how to go about that via function
# # This would go right at the begining of the script
# #
# # Create a list of the different groups distinguished in the group column
# groups <- as.data.table(ani_sf)[, .SD, .SDcols = group_col][[1]] %>% 
#   unique() %>% 
#   sort()
# 
# # Create an empty list for storing the separated data
# ani_sub <- list()
# 
# # Create a list of separate data.tables for each group distinguished
# for (i in 1:length(groups)){
#   
#   # Pull out just a single group and create a data.table
#   group_i <- ani_sf[which(as.data.table(ani_sf)[, 
#                                                 .SD == groups[i], 
#                                                 .SDcols = group_col]), ] %>% 
#     as.data.table()
#   
#   # Now pull out only individuals with at least 5 locations for the MCP function
#   group_i_lg <- group_i[, count := .N, by = ind_col
#                            ][count > 4, 
#                              ][, -"count"]
#   
#   # Create a table of just the names (we are eliminating all the extra columns)
#   group_i_ind <- group_i_lg[, .SD, .SDcols = ind_col]
#   
#   # This changes the data to a Spatial Points Data Frame which is what is
#   # needed by the MCP function
#   coordinates(group_i_ind) <- group_i_lg[, .SD, .SDcols = coord_col]
#   crs(group_i_ind) <- pref_crs
#   
#   # Add the data to a running list of the grouped data
#   ani_sub[[i]] <- group_i_ind
#   
#   # And give it a name that matches the group
#   names(ani_sub)[[i]] <- groups[[i]]
#   
# }
# 
# # Fit 99% MCP for each group
# group_mcps <- lapply(ani_sub, mcp, percent = 99)
# 
# # Fit a 99% KDE for each group
# group_kde <- lapply(ani_sub, kernelUD, grid = 30, extent = 0.5, same4all = TRUE)