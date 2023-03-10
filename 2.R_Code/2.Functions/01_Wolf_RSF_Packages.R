### WILD 562 - Wolf RSF
### Jordan Heiman
## Date: 2023-02-20

## Code purpose: Load packages (check if installed and if not install them)

################################################################################
## Load required packages
# Function from https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

## First specify the packages of interest
packages = c("data.table", "ks", "plotrix", "lattice", "adehabitatHR", "maptools", 
             "mapview", "ggplot2","colorRamps", "sf", "terra", "tmap", "stars", 
             "dplyr", "tmap", "rstudioapi", "tidyverse", "effects", "tools",
             "ggpubr")

## Now check each packages, if it needs to be installed, install it, then load it
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)