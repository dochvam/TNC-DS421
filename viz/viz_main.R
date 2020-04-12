library(leaflet)
library(shiny)

## Step 0. grab a dummy raster for now - took code from proc2
library(unmarked)
library(raster)
library(tidyverse)
library(rgdal)

precip <- raster("viz/PRISM_ppt_30yr_normal_4kmM2_annual_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")

## Step 1. Leaflet - we will start by getting leaflet to load and display the raster properly, and then move on to
## wrapping it in RShiny and adding interactivity

# project the raster
crs(precip) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# get color palette
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(precip),
                    na.color = "transparent")

# display the raster
leaflet() %>% addTiles() %>%
  addRasterImage(precip, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(precip),
            title = "precip")

