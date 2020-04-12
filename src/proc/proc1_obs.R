library(unmarked)
library(raster)
library(tidyverse)
library(prism)

target_common_names <- c("Verdin", "Black-tailed_Gnatcatcher", "Crissal_Thrasher", "Bells_Vireo")

ebird_CA_obs <- read_csv("data/eBird_data/ebird_target_spec_obs_CA.csv") %>% 
                  filter(name_clean %in% target_common_names)
ebird_CA_checklists <- read_csv("data/eBird_data/ebird_target_spec_checklists_CA.csv")
ebird_NV_obs <- read_csv("data/eBird_data/ebird_target_spec_obs_NV.csv") %>% 
                  filter(name_clean %in% target_common_names)
ebird_NV_checklists <- read_csv("data/eBird_data/ebird_target_spec_checklists_NV.csv")
ebird_AZ_obs <- read_csv("data/eBird_data/ebird_target_spec_obs_AZ.csv") %>% 
                  filter(name_clean %in% target_common_names)
ebird_AZ_checklists <- read_csv("data/eBird_data/ebird_target_spec_checklists_AZ.csv")
ebird_UT_obs <- read_csv("data/eBird_data/ebird_target_spec_obs_UT.csv") %>% 
                  filter(name_clean %in% target_common_names)
ebird_UT_checklists <- read_csv("data/eBird_data/ebird_target_spec_checklists_UT.csv")

# Get unique species scientific - common name pairs
species_names <- ebird_CA_obs %>% 
                 dplyr::select(name_clean, SCIENTIFIC.NAME) %>% 
                 distinct(name_clean, SCIENTIFIC.NAME)

# Get ebird protocol codes
protocol_codes <- read_csv("data/eBird_data/ebird_protocol_codes.csv")

all_checklist_info <- bind_rows(ebird_AZ_checklists, ebird_CA_checklists, 
                                ebird_NV_checklists, ebird_UT_checklists) %>% 
                      filter(ALL.SPECIES.REPORTED == 1)

distinct_locations_test <- all_checklist_info %>%
                      count(LATITUDE, LONGITUDE)

all_checklist_info$lat_rd <- round(all_checklist_info$LATITUDE, 2)
all_checklist_info$lon_rd <- round(all_checklist_info$LONGITUDE, 2)

distinct_locations_rd <- all_checklist_info %>%
                         count(lat_rd, lon_rd)


all_checklist_info %>% 
  count(PROTOCOL.CODE, sort = T) %>% 
  left_join(protocol_codes)


good_checklist_info <- 
  all_checklist_info %>% 
  mutate(year = lubridate::year(OBSERVATION.DATE),
         month = lubridate::month(OBSERVATION.DATE),
         mday = lubridate::mday(OBSERVATION.DATE),
         yday = lubridate::yday(OBSERVATION.DATE),
         tod = lubridate::hour(TIME.OBSERVATIONS.STARTED) * 60 + 
               lubridate::minute(TIME.OBSERVATIONS.STARTED)) %>% 
  filter(PROTOCOL.CODE %in% c("P22", "P21") &
         month %in% 3:7 &
         !is.na(OBSERVATION.DATE) &
         !is.na(TIME.OBSERVATIONS.STARTED) &
         !is.na(PROTOCOL.CODE) &
         !is.na(DURATION.MINUTES) &
         !is.na(LATITUDE) &
         !is.na(LONGITUDE))

good_checklist_info$EFFORT.DISTANCE.KM[is.na(good_checklist_info$EFFORT.DISTANCE.KM) & 
                                       good_checklist_info$PROTOCOL.CODE == "P21"] <- 0

distinct_locations_test <- good_checklist_info %>%
                      count(LATITUDE, LONGITUDE)


all_obs <- bind_rows(ebird_NV_obs, ebird_CA_obs, ebird_AZ_obs, ebird_UT_obs)
btg_obs <- all_obs %>% filter(name_clean == "Black-tailed_Gnatcatcher")
vrd_obs <- all_obs %>% filter(name_clean == "Verdin")
cth_obs <- all_obs %>% filter(name_clean == "Crissal_Thrasher")
bvo_obs <- all_obs %>% filter(name_clean == "Bells_Vireo")

good_checklist_info$btg_observed <- 
  good_checklist_info$SAMPLING.EVENT.IDENTIFIER %in% btg_obs$SAMPLING.EVENT.IDENTIFIER
good_checklist_info$vrd_observed <- 
  good_checklist_info$SAMPLING.EVENT.IDENTIFIER %in% vrd_obs$SAMPLING.EVENT.IDENTIFIER
good_checklist_info$cth_observed <- 
  good_checklist_info$SAMPLING.EVENT.IDENTIFIER %in% cth_obs$SAMPLING.EVENT.IDENTIFIER
good_checklist_info$bvo_observed <- 
  good_checklist_info$SAMPLING.EVENT.IDENTIFIER %in% bvo_obs$SAMPLING.EVENT.IDENTIFIER




sum(!(btg_obs$SAMPLING.EVENT.IDENTIFIER %in% good_checklist_info$SAMPLING.EVENT.IDENTIFIER)) /
  nrow(btg_obs)
cropped_checklists <- good_checklist_info %>% 
                      filter(LONGITUDE < -109 & LONGITUDE > -120 &
                             LATITUDE < 40 & LATITUDE > 30)
distinct_locations <- cropped_checklists %>%
                      count(LATITUDE, LONGITUDE, sort = T) %>% 
                      filter(n > 1) %>% 
                      mutate(id = row_number())

write_csv(distinct_locations, "data/intermediate/distinct_locations.csv")
write_csv(cropped_checklists, "data/intermediate/cropped_checklists.csv")
