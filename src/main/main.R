##
## Main Script for DS421 TNC Project  |  April 12, 2020
## Lisa Rennels, Ben Goldstein, Michelle Yu
##

source("src/main/utils.R")
source("src/main/viz_leaflet.R")

##
## SETUP
##

# IMPT: SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.


## 
## load variables and set constants
##

# load the default variables
prism_stack <- stackOpen("data/climate_data/PRISM_proc_crop_raster_stack.stk")
all_covs <- read_csv("data/intermediate/model_coefficients.csv")
scaling_factors <- read_csv("data/intermediate/scale_factors.csv")

# pick a species
# options for species are: "Verdin", "Crissal_Thrasher", "Black-tailed-Gnatcatcher", and "Bells-Vireo"
species = "Verdin"

##
## Example 1. Baseline Climate Conditions
##

occu_surface <- predict_occu_surface(prism_stack, all_covs, species, scaling_factors)
get_leaflet(occu_surface, "%")

##
## Example 2. Use New Climate Conditions
##

climate_factors = c(0.8, 1, 1, 1) # 80% of precip, increases of 1 deg C for all temperature variables
new_prism_stack = scale_climate_stack(prism_stack, climate_factors)
occu_surface_new <- predict_occu_surface(new_prism_stack, all_covs, species, scaling_factors)
get_leaflet(occu_surface_new, "%")

##
## Example 3. Use built in functions (eventually wrap this in RShiny)
##

get_leaflet_occu_surface(prism_stack, all_covs, species, scaling_factors, c(0.1, 5, 5, 5)) # baseline climate
