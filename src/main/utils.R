# IMPT: SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.

library(unmarked)
library(raster)
library(tidyverse)
library(prism)
library(locfit)

# 
# viewer settings
#
set_null_viewer <- function() {
    options(viewer = NULL)
}

reset_viewer <- function() {
  options(viewer=function (url, height = NULL) 
  {
    if (!is.character(url) || (length(url) != 1)) 
      stop("url must be a single element character vector.", 
           call. = FALSE)
    if (identical(height, "maximize")) 
      height <- -1
    if (!is.null(height) && (!is.numeric(height) || (length(height) != 
                                                     1))) 
      stop("height must be a single element numeric vector or 'maximize'.", 
           call. = FALSE)
    invisible(.Call("rs_viewer", url, height))
  })
}

# small helper function 
expit <- function(x) {1 / (1 + exp(-x))}

# predict_occu_surface: function to produce the predicted occupancy surface  
#   inputs: input_stack, all_covs, target_species, scaling_factors
#   outputs: occupancy surface raster
predict_occu_surface <- function(input_stack, all_covs, target_species, scaling_factors) {
  if (!(nlayers(input_stack) == 5)) {
    stop("Input raster stack must have 4 layers in order: precip, mean temp, min temp, max temp, and elevation.")
  }
  if (!(target_species %in% all_covs$species)) {
    stop(paste0("No model available for species ", target_species))
  }
  
  # Apply scaling to values
  input_stack[[1]] <- (input_stack[[1]] - scaling_factors$median[1]) / scaling_factors$sd[1]
  input_stack[[2]] <- (input_stack[[2]] - scaling_factors$median[2]) / scaling_factors$sd[2]
  input_stack[[3]] <- (input_stack[[3]] - scaling_factors$median[3]) / scaling_factors$sd[3]
  input_stack[[4]] <- (input_stack[[4]] - scaling_factors$median[4]) / scaling_factors$sd[4]
  input_stack[[5]] <- (input_stack[[5]] - scaling_factors$median[7]) / scaling_factors$sd[7]
  
  latitude_raster <- (init(input_stack[[1]], 'y') - scaling_factors$median[5]) / scaling_factors$sd[5]
  longitude_raster <- (init(input_stack[[1]], 'x') - scaling_factors$median[6]) / scaling_factors$sd[6]
  
  spec_covs <- all_covs %>% filter(species == target_species)
  cov_vector <- as.numeric(spec_covs$est)
  
  transformed_mean <- cov_vector[1] +     # Intercept
    input_stack[[1]] * cov_vector[2] +    # Precip
    input_stack[[3]] * cov_vector[3] +    # Tmin
    input_stack[[2]] * cov_vector[4] +    # Tmean
    input_stack[[4]] * cov_vector[5] +    # Tmax
    latitude_raster * cov_vector[6] +     # Latitude
    latitude_raster^2 * cov_vector[7] +   # Latitude^2
    longitude_raster * cov_vector[8] +    # Longitude
    input_stack[[5]] * cov_vector[9] +    # elevation
    input_stack[[5]]^2 * cov_vector[10] + # elev^2
    input_stack[[1]] * input_stack[[3]] * # precip:tmin
    cov_vector[11]
  expit(transformed_mean)
}

# scale_input_stack: function to scale the prism climate inputs
#   inputs: a 4 element vector in order [precip, tmean, tmin, tamx] of factors, which will be 
#           added in the case of temperature or multiplied in the case of precipitation
#   outputs: a new raster stack that has been transformed by these inputs
scale_input_stack <- function(input_stack, factors){
  new_input_stack <- input_stack
  new_input_stack[[1]] <- input_stack[[1]] * factors[1]
  new_input_stack[[2]] <- input_stack[[2]] + factors[2]
  new_input_stack[[3]] <- input_stack[[3]] + factors[3]
  new_input_stack[[4]] <- input_stack[[4]] + factors[4]
  return(new_input_stack)
}

  
