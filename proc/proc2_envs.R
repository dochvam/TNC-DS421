library(unmarked)
library(raster)
library(tidyverse)
library(prism)

distinct_locations <- read_csv("intermediate/distinct_locations.csv")
points <- SpatialPointsDataFrame(coords = distinct_locations[, c("LONGITUDE", "LATITUDE")],
                                 data = distinct_locations, 
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

if (!file.exists("climate_data/PRISM_proc_crop_raster_stack.grd")) {
  options(prism.path = "/Volumes/deValpineLab/TNC-DS421/climate_data/")
  get_prism_normals(type = "ppt", resolution = "800m", mon = 1:12, keepZip = TRUE)
  get_prism_normals(type = "tmean", resolution = "800m", mon = 1:12, keepZip = TRUE)
  get_prism_normals(type = "tmin", resolution = "800m", mon = 1:12, keepZip = TRUE)
  get_prism_normals(type = "tmax", resolution = "800m", mon = 1:12, keepZip = TRUE)
  
  raster_files <- list.files(path = "climate_data/", 
                             full.names = TRUE, recursive = T, pattern = "\\.bil$")
  
  precip_files <- raster_files[grepl("ppt", raster_files)]
  tmean_files <- raster_files[grepl("tmean", raster_files)]
  tmin_files <- raster_files[grepl("tmin", raster_files)]
  tmax_files <- raster_files[grepl("tmax", raster_files)]
  
  precip_rasters <- purrr::map(precip_files, raster)
  precip_rasters <- crop(stack(precip_rasters), extent(points) + 0.2)
  precip <- mean(precip_rasters)
  
  tmean_rasters <- purrr::map(tmean_files, raster)
  tmean_rasters <- crop(stack(tmean_rasters), extent(points) + 0.2)
  tmean <- mean(tmean_rasters)
  
  tmin_rasters <- purrr::map(tmin_files, raster)
  tmin_rasters <- crop(stack(tmin_rasters), extent(points) + 0.2)
  tmin <- min(tmin_rasters)
  # 
  tmax_rasters <- purrr::map(tmax_files, raster)
  tmax_rasters <- crop(stack(tmax_rasters), extent(points) + 0.2)
  tmax <- max(tmax_rasters)
  
  climate_rasters <- stack(precip,
                           tmean,
                           tmin,
                           tmax)
  
  stackSave(climate_rasters, "climate_data/PRISM_proc_crop_raster_stack.stk")
} else {
  climate_rasters <- stackOpen("climate_data/PRISM_proc_crop_raster_stack.stk")
}
env_covs <- raster::extract(climate_rasters, points)

distinct_locations$precip <- env_covs[,1]
distinct_locations$tmean <- env_covs[,2]
distinct_locations$tmin <- env_covs[,3]
distinct_locations$tmax <- env_covs[,4]





