# IMPT: SET YOUR WORKING DIRECTORY to the main TNC-DS421 project folder, which is two levels 
# above this file and contains the `data` and `src` primary folders. All paths are relative to this
# directory.

library(shiny)
library(leaflet)

source("src/main/utils.R")
source("src/main/viz_leaflet.R")

run_shiny <- function(climate_stack, all_covs, species, scaling_factors, climate_factors) {
    
  # make ui
  ui <- fluidPage(
    leafletOutput("mymap"),
    p()
  )
  
  # make server
  server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
      
      # get the occupancy surface leaflet map
      get_leaflet_occu_surface(climate_stack, all_covs, species, scaling_factors, climate_factors)
      
    })
  }
  
  # display
  shinyApp(ui, server)
  
}