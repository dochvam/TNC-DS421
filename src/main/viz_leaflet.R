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
#   inputs: input raster
#   outputs: leaflet visualization
get_leaflet <- function(myraster) {
  
  # get color palette
  pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(myraster),
                      na.color = "transparent")
  
  # use leaflet to display the raster (https://rstudio.github.io/leaflet/raster.html)
  map <- leaflet() %>% 
    
    # base groups
    addTiles(group = "State Lines (default)") %>%
    
    # map groups
    addRasterImage(myraster, colors = pal, opacity = 0.8, group = "Occupancy Surface") %>%
    addLegend(pal = pal, values = values(myraster),title = "probability", group = "Occupancy Surface") %>%
  
    # marker groups
    # TODO - add points
  
    # layer control (https://rstudio.github.io/leaflet/showhide.html)
    addLayersControl(
      baseGroups = c("State Lines (default)"),
      overlayGroups = c("Occupancy Surface"),
      options = layersControlOptions(collapsed = FALSE)
     )
    
  return (map)
}

# get_leaflet_allspecies: function to use leaflet to display all four occupancy surface rasters, note that as of now
# this function hard codes the four species but can be expanded to deal with a varying number of species and
# species names
#   inputs: input raster stack
#   outputs: leaflet visualization
get_leaflet_allspecies <- function(rasters) {
  
  # Species are assumed to be as follows and in order:
  #
  # 1. "Verdin"
  # 2. "Crissal_Thrasher"
  # 3. "Black-tailed_Gnatcatcher"
  # 4. "Bells_Vireo"

  
  # get color palette
  vals = c(values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]), values(rasters[[4]]))
  pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), NULL,
                      na.color = "transparent")
  
  # use leaflet to display the raster (https://rstudio.github.io/leaflet/raster.html)
  map <- leaflet() %>% 
    
   
    # base layers
    addTiles(group = "Base") %>%
    addLegend(pal = pal, values = vals,title = "probability", group = "Base") %>%
    
    # overlay layers -markers and maps
    
    # TODO - add points which will have group "Observation Points"
    
    addRasterImage(rasters[[1]], colors = pal, opacity = 0.8, group = "Verdin") %>%
    addRasterImage(rasters[[2]], colors = pal, opacity = 0.8, group = "Crissal_Thrasher") %>%
    addRasterImage(rasters[[3]], colors = pal, opacity = 0.8, group = "Black-tailed_Gnatcatcher") %>%
    addRasterImage(rasters[[4]], colors = pal, opacity = 0.8, group = "Bells_Vireo") %>%
    
  
    # layer control (https://rstudio.github.io/leaflet/showhide.html)
    addLayersControl(
      baseGroups = c("Verdin", "Crissal_Thrasher", "Black-tailed_Gnatcatcher", "Bells_Vireo"),
      overlayGroups = c("Observation Points"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    
    hideGroup("Observation Points") # start with points hidden
  
  return (map)
}

# get_leaflet_occu_surface: function to display an occupancy surface
#   inputs: climate variables, covariates, species, scaling factors, and climate factors (to scale climate)
#   outputs: leaflet visualization

get_leaflet_occu_surface <- function(input_stack, all_covs, species, scaling_factors, climate_factors) {
  
  input_stack_scaled = scale_input_stack(input_stack, climate_factors)
  occu_surface <- predict_occu_surface(input_stack_scaled, all_covs, species ,scaling_factors)
  map <- get_leaflet(occu_surface)
  
  return(map)
  
}

# get_leaflet_occu_surface_allspecies: function to display all four occupancy surfaces, note that as of now
# this function hard codes the four species but can be expanded to deal with a varying number of species and
# species names
#   inputs: climate variables, covariates, scaling factors, and climate factors (to scale climate)
#   outputs: leaflet visualization

get_leaflet_occu_surface_allspecies <- function(input_stack, all_covs, scaling_factors, climate_factors){
  
  input_stack_scaled = scale_input_stack(input_stack, climate_factors)
  
  occu_surface_thrasher     <- predict_occu_surface(prism_stack, all_covs, "Crissal_Thrasher", scaling_factors)
  occu_surface_verdin       <- predict_occu_surface(prism_stack, all_covs, "Verdin", scaling_factors)
  occu_surface_gnatcatcher  <-predict_occu_surface(prism_stack, all_covs, "Black-tailed_Gnatcatcher", scaling_factors)
  occu_surface_vireo        <- predict_occu_surface(prism_stack, all_covs, "Bells_Vireo", scaling_factors)
  
  occu_surface_stack <- stack(occu_surface_thrasher,
                              occu_surface_verdin,
                              occu_surface_gnatcatcher,
                              occu_surface_vireo)
  
  map <- get_leaflet_allspecies(occu_surface_stack)
  
  return(map)
}

