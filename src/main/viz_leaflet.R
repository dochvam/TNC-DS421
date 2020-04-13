# IMPT: SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.

library(leaflet)
library(shiny)
library(raster)
library(tidyverse)
library(rgdal)
library(devtools)

source("src/main/utils.R")

# project_raster_to_WGS84: function to project a raster to WGS84 if needed (this is the projection of PRISM)
#   inputs: input raster
#   outputs: NA (side effect is that theinput raster has been tagged with a projection)
project_raster_to_WGS84 <- function(myraster) {
  crs(precip) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # projection
}

# get_leaflet: function to use leaflet to display a raster
#   inputs: input raster and title
#   outputs: leaflet visualization
get_leaflet <- function(myraster, mytitle) {
  
  # get color palette
  pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(myraster),
                      na.color = "transparent")
  
  # use leaflet to display the raster (https://rstudio.github.io/leaflet/raster.html)
  map <- leaflet() %>% 
    
    # base groups
    addTiles(group = "State Lines (default)") %>%
    
    # map groups
    addRasterImage(myraster, colors = pal, opacity = 0.8, group = "Occupancy Surface") %>%
    addLegend(pal = pal, values = values(myraster),title = mytitle, group = "Occupancy Surface") %>%
  
    # marker groups
    # TODO - add points
  
    # layer control (https://rstudio.github.io/leaflet/showhide.html)
    addLayersControl(
      baseGroups = c("State Lines (default)"),
      overlayGroups = c("Occupancy Surface"),
      options = layersControlOptions(collapsed = FALSE)
     ) %>% 
    
    hideGroup("Occupancy Surface") # start with occupancy surface hidden
  
  return (map)
}

# get_leaflet_occu_surface: function to display an occupancy surface
#   inputs: climate variables, covariates, species, scaling factors, and climate factors (to scale climate)
#   outputs: leaflet visualization

get_leaflet_occu_surface <- function(climate_stack, all_covs, species, scaling_factors, climate_factors) {
  
  climate_stack_scaled = scale_climate_stack(climate_stack, climate_factors)
  occu_surface <- predict_occu_surface(climate_stack_scaled, all_covs, species ,scaling_factors)
  map <- get_leaflet(occu_surface, "%")
  
  return(map)
  
}

