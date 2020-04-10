library(unmarked)
library(raster)
library(tidyverse)
library(prism)


distinct_locations <- read_csv("intermediate/distinct_locations_w_env.csv")
cropped_checklists <- read_csv("intermediate/cropped_checklists.csv")

cropped_checklists <- left_join(cropped_checklists, 
                                distinct_locations,
                                by = c("LONGITUDE", "LATITUDE")) %>% 
                      filter(!is.na(id))


# Function
get_var_wide <- function(df, col, maxobs) {
  df <- df[, c("id", col)] %>% arrange(id)
  colnames(df)[2] <- "target_col"
  
  df %>% 
    group_by(id) %>% 
    mutate(visit = paste0("X", str_pad(1:n(), 5, pad = "0"))) %>%
    filter(visit <= paste0("X", str_pad(maxobs, 5, pad = "0"))) %>% 
    spread(key = visit, value = target_col) %>% 
    arrange(id) %>% 
    ungroup() %>% 
    dplyr::select(-id) %>% 
    as.matrix()
}

# Randomize the order of rows for the sake of cropping (to improve computation time during testing)
cropped_checklists_shuffled <- sample(cropped_checklists) %>% 
  filter(!is.na(landcover)) %>% 
  filter(!is.na(pop_density))

nsites <- 47343
nvisits <- 5

# Start getting matrices of each variable of interest
distance_mtx <- get_var_wide(cropped_checklists_shuffled, "EFFORT.DISTANCE.KM", nvisits)
tod_mtx <- get_var_wide(cropped_checklists_shuffled, "tod", nvisits)
doy_mtx <- get_var_wide(cropped_checklists_shuffled, "yday", nvisits)
protocol_mtx <- get_var_wide(cropped_checklists_shuffled, "PROTOCOL.CODE", nvisits)
duration_mtx <- get_var_wide(cropped_checklists_shuffled, "DURATION.MINUTES", nvisits)

scale_factors <- 
  data.frame(param = c("precip", "tmean", "tmin", "tmax", "latitude", "longitude"),
             median = c(
               median(cropped_checklists_shuffled$precip, na.rm = T),
               median(cropped_checklists_shuffled$tmean, na.rm = T),
               median(cropped_checklists_shuffled$tmin, na.rm = T),
               median(cropped_checklists_shuffled$tmax, na.rm = T),
               median(cropped_checklists_shuffled$LATITUDE, na.rm = T),
               median(cropped_checklists_shuffled$LONGITUDE, na.rm = T)
             ), sd = c(
               sd(cropped_checklists_shuffled$precip, na.rm = T),
               sd(cropped_checklists_shuffled$tmean, na.rm = T),
               sd(cropped_checklists_shuffled$tmin, na.rm = T),
               sd(cropped_checklists_shuffled$tmax, na.rm = T),
               sd(cropped_checklists_shuffled$LATITUDE, na.rm = T),
               sd(cropped_checklists_shuffled$LONGITUDE, na.rm = T)
             ))
write_csv(scale_factors, "intermediate/scale_factors.csv")

site_covariates <- data.frame(precip = (cropped_checklists_shuffled$precip - scale_factors$median[1]) / scale_factors$sd[1],
                              precip_sq = ((cropped_checklists_shuffled$precip - scale_factors$median[1]) / scale_factors$sd[1])^2,
                              tmean = (cropped_checklists_shuffled$tmean - scale_factors$median[2]) / scale_factors$sd[2],
                              tmax = (cropped_checklists_shuffled$tmin - scale_factors$median[3]) / scale_factors$sd[3],
                              tmin = (cropped_checklists_shuffled$tmax - scale_factors$median[4]) / scale_factors$sd[4],
                              latitude = (cropped_checklists_shuffled$LATITUDE - scale_factors$median[5]) / scale_factors$sd[5],
                              latitude_sq = ((cropped_checklists_shuffled$LATITUDE - scale_factors$median[5]) / scale_factors$sd[5])^2,
                              longitude = (cropped_checklists_shuffled$LONGITUDE - scale_factors$median[6]) / scale_factors$sd[6],
                              longitude_sq = ((cropped_checklists_shuffled$LATITUDE - scale_factors$median[6]) / scale_factors$sd[6])^2,
                              popdensity = scale(cropped_checklists_shuffled$pop_density),
                              landcover = as.factor(cropped_checklists_shuffled$landcover))[1:nsites,]
observation_covariates <- list(distance = scale(distance_mtx[1:nsites,]),
                               tod = scale(tod_mtx[1:nsites,]), tod_sq = scale(tod_mtx[1:nsites,])^2,
                               doy = scale(doy_mtx[1:nsites,]), doy_sq = scale(doy_mtx[1:nsites,])^2,
                               duration = duration_mtx[1:nsites,],
                               protocol = protocol_mtx[1:nsites,])


observed_mtx <- get_var_wide(cropped_checklists_shuffled, "cth_observed", nvisits)
occu_frame <- unmarkedFrameOccu(y = observed_mtx, 
                                siteCovs = site_covariates, 
                                obsCovs = observation_covariates)

# Fit model for crissal thrasher
system.time(
  cth_occufit <- occu(~ distance + tod + tod_sq + doy + doy_sq + duration + protocol 
                  ~ precip * tmin + tmean + tmax + latitude + latitude_sq + longitude,
                  data = occu_frame)
)
cth_covs <- data.frame(species = "Crissal_Thrasher",
                       param = names(cth_occufit@estimates@estimates$state@estimates),
                       est = cth_occufit@estimates@estimates$state@estimates)

# Fit for black tailed gnatcatcher
observed_mtx <- get_var_wide(cropped_checklists_shuffled, "btg_observed", nvisits)
occu_frame <- unmarkedFrameOccu(y = observed_mtx, 
                                siteCovs = site_covariates, 
                                obsCovs = observation_covariates)
system.time(
  btg_occufit <- occu(~ distance + tod + tod_sq + doy + doy_sq + duration + protocol 
                  ~ precip * tmin + tmean + tmax + latitude + latitude_sq + longitude,
                  data = occu_frame)
)
btg_covs <- data.frame(species = "Black-tailed_Gnatcatcher",
                       param = names(btg_occufit@estimates@estimates$state@estimates),
                       est = btg_occufit@estimates@estimates$state@estimates)

# Fit for Bell's vireo
observed_mtx <- get_var_wide(cropped_checklists_shuffled, "bvo_observed", nvisits)
occu_frame <- unmarkedFrameOccu(y = observed_mtx, 
                                siteCovs = site_covariates, 
                                obsCovs = observation_covariates)
system.time(
  bvo_occufit <- occu(~ distance + tod + tod_sq + doy + doy_sq + duration + protocol 
                  ~ precip * tmin + tmean + tmax + latitude + latitude_sq + longitude,
                  data = occu_frame)
)
bvo_covs <- data.frame(species = "Bells_Vireo",
                       param = names(bvo_occufit@estimates@estimates$state@estimates),
                       est = bvo_occufit@estimates@estimates$state@estimates)

# Fit for Verdin
observed_mtx <- get_var_wide(cropped_checklists_shuffled, "vrd_observed", nvisits)
occu_frame <- unmarkedFrameOccu(y = observed_mtx, 
                                siteCovs = site_covariates, 
                                obsCovs = observation_covariates)
system.time(
  vrd_occufit <- occu(~ distance + tod + tod_sq + doy + doy_sq + duration + protocol 
                  ~ precip * tmin + tmean + tmax + latitude + latitude_sq + longitude,
                  data = occu_frame)
)
vrd_covs <- data.frame(species = "Verdin",
                       param = names(vrd_occufit@estimates@estimates$state@estimates),
                       est = vrd_occufit@estimates@estimates$state@estimates)


all_covs <- bind_rows(vrd_covs, bvo_covs, btg_covs, cth_covs)
write_csv(all_covs, "intermediate/model_coefficients.csv")

