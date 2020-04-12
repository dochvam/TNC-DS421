##
## Main Script for DS421 TNC Project  |  April 12, 2020
## Lisa Rennels, Ben Goldstein, Michelle Yu
##

# IMPT: SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.

# load libraries
library(unmarked)
library(raster)
library(tidyverse)
library(prism)
library(locfit)
library(leaflet)
library(shiny)
library(rgdal)

# load source files
source("src/main/utils.R")
source("src/main/viz.R") # in progress!!

# get inputs
prism_stack <- stackOpen("data/climate_data/PRISM_proc_crop_raster_stack.stk")
all_covs <- read_csv("data/intermediate/model_coefficients.csv")
scaling_factors <- read_csv("intermediate/scale_factors.csv")

# get occupancy
verdin_occu_surface <- predict_occu_surface(prism_stack, all_covs, "Verdin", scaling_factors)
thrasher_occu_surface <- predict_occu_surface(prism_stack, all_covs, "Crissal_Thrasher", scaling_factors)
gnatcatcher_occu_surface <- predict_occu_surface(prism_stack, all_covs, "Black-tailed_Gnatcatcher", scaling_factors)
vireo_occu_surface <- predict_occu_surface(prism_stack, all_covs, "Bells_Vireo", scaling_factors)

# plot surfaces
plot(verdin_occu_surface)
plot(thrasher_occu_surface)
plot(gnatcatcher_occu_surface)
plot(vireo_occu_surface)

# run visualization (in progress)