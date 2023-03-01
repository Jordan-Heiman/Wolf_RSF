### WILD 562 - Wolf RSF
### Jordan Heiman
## Date: 2023-02-21

## Function purpose: Create univariate models for covariates 

#################################### Intro #####################################

# Name: 04_Wolf_RSF_GLM_Mods
# Description:   Using list of points that were created through previous 
# functions, create univariate habitat selection models

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-02-21

################################# Arguments ####################################

# pt_sets
#       A list of data.frames containing all used and available points by home 
#       range type and possibly by grouping

################################# Output #######################################

# mod_lst
#       A list of univariate models

################################################################################
## Function

glm_mods <- function(pt_sets){
  
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