##
## Main Script for DS421 TNC Project  |  April 12, 2020
## Lisa Rennels, Ben Goldstein, Michelle Yu
##

source("src/main/utils.R")
source("src/main/viz_leaflet.R")

##
## SETUP and INFO
##

# 1. SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.

# 2. Your options for species are: "Verdin", "Crissal_Thrasher", "Black-tailed-Gnatcatcher", and "Bells-Vireo"

# 3. If you want to visualize in your default RStudio viewer, no action is needed. If you would prefer to force
# the visualization to appear in a local html browser, toggle this on with set_null_viewer() and off at the
# end of the script with reset_viewer(), as below.  Note these custom funcs may be specific to RStudio version.

set_null_viewer() # set options$viewer to null so visualization is in local html browser
reset_viewer()    # reset options$viewer to RStudio Viewer

## 
## load variables and set constants
##

# load the default variables
precip_final <- raster("data/climate_data/precip_final.grd")
tmean_final <- raster("data/climate_data/tmean_final.grd")
tmin_final <- raster("data/climate_data/tmin_final.grd")
tmax_final <- raster("data/climate_data/tmax_final.grd")
elev_final <- raster("data/climate_data/elev_final.grd")
prism_stack <- stack(precip_final,
                     tmean_final,
                     tmin_final,
                     tmax_final,
                     elev_final)
all_covs <- read_csv("data/intermediate/model_coefficients.csv")
scaling_factors <- read_csv("data/intermediate/scale_factors.csv")

##
## Example 1. Baseline Climate Conditions
##

species = "Verdin"
occu_surface <- predict_occu_surface(prism_stack, all_covs, species, scaling_factors)
get_leaflet(occu_surface, "%")

##
## Example 2. Use New Climate Conditions
##

species = "Verdin"
climate_factors = c(0.8, 1, 1, 1) # 80% of precip, increases of 1 deg C for all temperature variables
new_prism_stack = scale_input_stack(prism_stack, climate_factors)
occu_surface_new <- predict_occu_surface(new_prism_stack, all_covs, species, scaling_factors)
get_leaflet(occu_surface_new, "%")

##
## Example 3. Use built in functions (eventually wrap this in RShiny)
##

species = "Crissal_Thrasher"
get_leaflet_occu_surface(prism_stack, all_covs, species, scaling_factors, c(1, 0, 0, 0)) # baseline climate
