### WILD 562 - Wolf RSF
### Jordan Heiman
## Date: 2023-02-21

## Function purpose: Bring together wolf used and available points for both 
# packs and for KDE and MCP home ranges

#################################### Intro #####################################

# Name: 04_Wolf_RSF_GLM_Models
# Description:   Bring together the wolf points from previous functions to 
# create a list of 2 tables, one for KDE and one for MCP home ranges

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-02-21

################################# Arguments ####################################

# pts_df_lst
#       List of dataframes that contain the points and covariate information for 
#       used and available points for KDE and MCP home ranges as created by 
#       previous functions

# by.group
#       A logical operator for whether the data and models should be separated by 
#       grouping such as herds or packs similar to previous functions in this 
#       series

################################# Output #######################################

# mod_lst
#       A list of two model lists (one for KDE home ranges and one for MCP home 
#       range); all models are univariate and cover all covariates provided

################################################################################
## Function

glm_mods <- function(pts_df_lst,
                     by.group = FALSE){
  
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
  
  # Start with an empty list for KDE and MCP to populate with models
  mod_lst <- list()
  mod_counter <- 1
  
  for (i in 1:length(pt_sets)){
    
    data <- pt_sets[[i]]
    data_name <- names(pt_sets)[[i]]
    cov_lst <- names(data)[2:(length(names(data))-5)]
    
    # Create uni-variate models 
    for(j in 1:length(cov_lst)){
      
      cov_name <- cov_lst[[j]]
      model <- glm(reformulate(cov_name, "used"), 
                   data = data, 
                   family = binomial(logit))
      mod_lst[[mod_counter]] <- model
      names(mod_lst)[[mod_counter]] <- gsub(" |\\.|\\$", "_", data_name) %>% 
        paste0("~", cov_name) %>% 
        tolower()
      
      mod_counter <- mod_counter + 1
      
    }
  }
  
  return(mod_lst)
  
  }
  