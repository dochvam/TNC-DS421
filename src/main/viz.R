library(leaflet)
library(shiny)
library(unmarked)
library(raster)
library(tidyverse)
library(rgdal)

# function to grab a dummy raster
get_precip_raster <- function(){
  precip <- raster("data/PRISM_ppt_30yr_normal_4kmM2_annual_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
  crs(precip) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # projection
  return(precip)
}

# function to use leaflet to display a raster statically
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
