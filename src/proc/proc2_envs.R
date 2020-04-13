library(unmarked)
library(raster)
library(tidyverse)
library(prism)

distinct_locations <- read_csv("data/intermediate/distinct_locations.csv")
points <- SpatialPointsDataFrame(coords = distinct_locations[, c("LONGITUDE", "LATITUDE")],
                                 data = distinct_locations, 
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

if (!all(file.exists("data/climate_data/precip_final.grd"),
         file.exists("data/climate_data/tmean_final.grd"),
         file.exists("data/climate_data/tmin_final.grd"),
         file.exists("data/climate_data/tmax_final.grd"),
         file.exists("data/climate_data/elev_final.grd"))) {
  options(prism.path = "data/climate_data/")
  get_prism_normals(type = "ppt", resolution = "800m", mon = 1:12, keepZip = TRUE)
  get_prism_normals(type = "tmean", resolution = "800m", mon = 1:12, keepZip = TRUE)
  get_prism_normals(type = "tmin", resolution = "800m", mon = 1:12, keepZip = TRUE)
  get_prism_normals(type = "tmax", resolution = "800m", mon = 1:12, keepZip = TRUE)

  raster_files <- list.files(path = "data/climate_data", 
                             full.names = TRUE, recursive = T, pattern = "\\.bil$")
  
  precip_files <- raster_files[grepl("ppt", raster_files)]
  tmean_files <- raster_files[grepl("tmean", raster_files)]
  tmin_files <- raster_files[grepl("tmin", raster_files)]
  tmax_files <- raster_files[grepl("tmax", raster_files)]
  elev_file <- raster_files[grepl("us_dem", raster_files)]
  
  precip_rasters <- purrr::map(precip_files, raster)
  precip_rasters <- crop(stack(precip_rasters), extent(points))
  precip <- mean(precip_rasters)
  
  tmean_rasters <- purrr::map(tmean_files, raster)
  tmean_rasters <- crop(stack(tmean_rasters), extent(points))
  tmean <- mean(tmean_rasters)
  
  tmin_rasters <- purrr::map(tmin_files, raster)
  tmin_rasters <- crop(stack(tmin_rasters), extent(points))
  tmin <- min(tmin_rasters)
  
  tmax_rasters <- purrr::map(tmax_files, raster)
  tmax_rasters <- crop(stack(tmax_rasters), extent(points))
  tmax <- max(tmax_rasters)
  
  elev_raster <- raster(elev_file)
  elev_raster <- crop(elev_raster, extent(points))
  
  # save the individual raster files
  precip_final <- writeRaster(precip, filename = "data/climate_data/precip_final.grd", 
                              prj=TRUE, overwrite = T)
  tmean_final <- writeRaster(tmean, filename = "data/climate_data/tmean_final.grd", 
                             prj=TRUE, overwrite = T)
  tmin_final <- writeRaster(tmin, filename = "data/climate_data/tmin_final.grd", 
                            prj=TRUE, overwrite = T)
  tmax_final <- writeRaster(tmax, filename = "data/climate_data/tmax_final.grd", 
                            prj=TRUE, overwrite = T)
  elev_final <- writeRaster(elev_raster, filename = "data/climate_data/elev_final.grd", 
                            prj=TRUE, overwrite = T)
  
  # save the stack
  climate_rasters <- stack(precip_final,
                           tmean_final,
                           tmin_final,
                           tmax_final,
                           elev_final)
} else {
  precip_final <- raster("data/climate_data/precip_final.grd")
  tmean_final <- raster("data/climate_data/tmean_final.grd")
  tmin_final <- raster("data/climate_data/tmin_final.grd")
  tmax_final <- raster("data/climate_data/tmax_final.grd")
  elev_final <- raster("data/climate_data/elev_final.grd")
  climate_rasters <- stack(precip_final,
                           tmean_final,
                           tmin_final,
                           tmax_final,
                           elev_final)
}
env_covs <- raster::extract(climate_rasters, points)

distinct_locations$precip <- env_covs[,1]
distinct_locations$tmean <- env_covs[,2]
distinct_locations$tmin <- env_covs[,3]
distinct_locations$tmax <- env_covs[,4]
distinct_locations$elev <- env_covs[,5]


nlcd_legend <- data.frame(cover_key = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 
                                        51, 52, 71, 72, 73, 74, 81, 82, 90, 95),
                          cover = c("Open Water",
                                    "Perennial Ice/Snow",
                                    "Developed, Open Space",
                                    "Developed, Low Intensity",
                                    "Developed, Medium Intensity",
                                    "Developed, High Intensity",
                                    "Barren Land (Rock/Sand/Clay)",
                                    "Deciduous Forest",
                                    "Evergreen Forest",
                                    "Mixed Forest",
                                    "Dwarf Scrub",
                                    "Shrub/Scrub",
                                    "Grassland/Herbaceous",
                                    "Sedge/Herbaceous",
                                    "Lichens",
                                    "Moss",
                                    "Pasture/Hay",
                                    "Cultivated Crops",
                                    "Woody Wetlands",
                                    "Emergent Herbaceous Wetlands"))

r_nlcd <- raster("../eBird_heatwave/data/nlcd_landcover") # local to Ben's computer
landcover <- raster::extract(r_nlcd, points)

data.frame(cover_key = landcover) %>% 
  count(cover_key) %>% 
  left_join(nlcd_legend) %>% 
  write_csv("data/intermediate/landcover_summary")

distinct_locations$landcover <- landcover


pop_density <- raster("../eBird_heatwave/data/pop_density") # local to Ben's computer
distinct_locations$pop_density <- raster::extract(pop_density, points)


write_csv(distinct_locations, "data/intermediate/distinct_locations_w_env.csv")
