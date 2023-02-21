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

#      Data                                                                 ####

################################################################################