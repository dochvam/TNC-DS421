library(unmarked)
library(raster)
library(tidyverse)
library(prism)
library(locfit)

prism_stack <- stackOpen("data/climate_data/PRISM_proc_crop_raster_stack.stk")
all_covs <- read_csv("data/intermediate/model_coefficients.csv")
scaling_factors <- read_csv("data/intermediate/scale_factors.csv")

predict_occu_surface <- function(input_stack, all_covs, target_species, scaling_factors) {
  if (!(nlayers(input_stack) == 4)) {
    stop("Raster stack must have 4 layers in order: precip, mean temp, min temp, and max temp.")
  }
  if (!(target_species %in% all_covs$species)) {
    stop(paste0("No model available for species ", target_species))
  }
  
  # Apply scaling to values
  input_stack[[1]] <- (input_stack[[1]] - scaling_factors$median[1]) / scaling_factors$sd[1]
  input_stack[[2]] <- (input_stack[[2]] - scaling_factors$median[2]) / scaling_factors$sd[2]
  input_stack[[3]] <- (input_stack[[3]] - scaling_factors$median[3]) / scaling_factors$sd[3]
  input_stack[[4]] <- (input_stack[[4]] - scaling_factors$median[4]) / scaling_factors$sd[4]
  
  latitude_raster <- (init(input_stack[[1]], 'y') - scaling_factors$median[5]) / scaling_factors$sd[5]
  longitude_raster <- (init(input_stack[[1]], 'x') - scaling_factors$median[6]) / scaling_factors$sd[6]
  
  spec_covs <- all_covs %>% filter(species == target_species)
  cov_vector <- as.numeric(spec_covs$est)
  
  transformed_mean <- cov_vector[1] +                     # Intercept
                      input_stack[[1]] * cov_vector[2] +  # Precip
                      input_stack[[3]] * cov_vector[3] +  # Tmin
                      input_stack[[2]] * cov_vector[4] +  # Tmean
                      input_stack[[4]] * cov_vector[5] +  # Tmax
                      latitude_raster * cov_vector[6] +   # Latitude
                      latitude_raster^2 * cov_vector[7] + # Latitude^2
                      longitude_raster * cov_vector[8] +  # Longitude
                      input_stack[[1]] * input_stack[[3]] # precip:tmin
  expit(transformed_mean)
}

# How long does it take to run?


plot(predict_occu_surface(prism_stack, all_covs, "Verdin", scaling_factors))
plot(predict_occu_surface(prism_stack, all_covs, "Crissal_Thrasher", scaling_factors))
plot(predict_occu_surface(prism_stack, all_covs, "Black-tailed_Gnatcatcher", scaling_factors))
plot(predict_occu_surface(prism_stack, all_covs, "Bells_Vireo", scaling_factors))


