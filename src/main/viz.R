# IMPT: SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.

library(leaflet)
library(shiny)
library(raster)
library(tidyverse)
library(rgdal)

source("src/main/utils.R")

# project_raster_to_WGS84: function to project a raster to WGS84 if needed (this is the projection of PRISM)
#   inputs: input raster
#   outputs: NA (side effect is that theinput raster has been tagged with a projection)
project_raster_to_WGS84 <- function(myraster) {
  crs(precip) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # projection
}

# display_raster: function to use leaflet to display a raster
#   inputs: input raster and title
#   outputs: leaflet visualization
display_raster <- function(myraster, mytitle) {
  
  # get color palette
  pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(myraster),
                      na.color = "transparent")
  
  # use leaflet to display the raster (https://rstudio.github.io/leaflet/raster.html)
  leaflet() %>% addTiles() %>%
    addRasterImage(myraster, colors = pal, opacity = 0.8) %>%
    addLegend(pal = pal, values = values(myraster),
              title = mytitle)
}

# display_occu_surface: function to display an occupancy surface
#   inputs: climate variables, covariates, species, scaling factors, and climate factors (to scale climate)
#   outputs: leaflet visualization

display_occu_surface <- function(climate_stack, all_covs, species, scaling_factors, climate_factors) {
  
  climate_stack_scaled = scale_climate_stack(climate_stack, climate_factors)
  occu_surface <- predict_occu_surface(climate_stack_scaled, all_covs, species ,scaling_factors)
  display_raster(occu_surface, "%")
  
}

