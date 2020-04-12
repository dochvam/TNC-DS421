# IMPT: SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.

library(unmarked)
library(raster)
library(tidyverse)
library(prism)
library(locfit)

# predict_occu_surface: function to produce the predicted occupancy surface  
#   inputs: climate_stack, all_covs, target_species, scaling_factors
#   outputs: occupancy surface raster

predict_occu_surface <- function(climate_stack, all_covs, target_species, scaling_factors) {
  if (!(nlayers(climate_stack) == 4)) {
    stop("Raster stack must have 4 layers in order: precip, mean temp, min temp, and max temp.")
  }
  if (!(target_species %in% all_covs$species)) {
    stop(paste0("No model available for species ", target_species))
  }
  
  # Apply scaling to values
  climate_stack[[1]] <- (climate_stack[[1]] - scaling_factors$median[1]) / scaling_factors$sd[1]
  climate_stack[[2]] <- (climate_stack[[2]] - scaling_factors$median[2]) / scaling_factors$sd[2]
  climate_stack[[3]] <- (climate_stack[[3]] - scaling_factors$median[3]) / scaling_factors$sd[3]
  climate_stack[[4]] <- (climate_stack[[4]] - scaling_factors$median[4]) / scaling_factors$sd[4]
  
  latitude_raster <- (init(climate_stack[[1]], 'y') - scaling_factors$median[5]) / scaling_factors$sd[5]
  longitude_raster <- (init(climate_stack[[1]], 'x') - scaling_factors$median[6]) / scaling_factors$sd[6]
  
  spec_covs <- all_covs %>% filter(species == target_species)
  cov_vector <- as.numeric(spec_covs$est)
  
  transformed_mean <- cov_vector[1] +                     # Intercept
    climate_stack[[1]] * cov_vector[2] +  # Precip
    climate_stack[[3]] * cov_vector[3] +  # Tmin
    climate_stack[[2]] * cov_vector[4] +  # Tmean
    climate_stack[[4]] * cov_vector[5] +  # Tmax
    latitude_raster * cov_vector[6] +   # Latitude
    latitude_raster^2 * cov_vector[7] + # Latitude^2
    longitude_raster * cov_vector[8] +  # Longitude
    climate_stack[[1]] * climate_stack[[3]] # precip:tmin
  expit(transformed_mean)
}

# scale_climate_stack: function to scale the prism climate inputs
#   inputs: a 4 element vector in order [precip, tmean, tmax, tmin] of factors, which will be 
#           added in the case of temperature or multiplied in the case of precipitation
#   outputs: a new raster stack that has been transformed by these inputs
scale_climate_stack <- function(climate_stack, factors){
  new_climate_stack <- climate_stack
  new_climate_stack[[1]] <- climate_stack[[1]] * factors[1]
  new_climate_stack[[2]] <- climate_stack[[2]] + factors[2]
  new_climate_stack[[3]] <- climate_stack[[3]] + factors[3]
  new_climate_stack[[4]] <- climate_stack[[4]] + factors[4]
  return(new_climate_stack)
}

  
