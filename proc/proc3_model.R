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
nvisits <- 10

# Start getting matrices of each variable of interest
observed_mtx <- get_var_wide(cropped_checklists_shuffled, "cth_observed", nvisits)
distance_mtx <- get_var_wide(cropped_checklists_shuffled, "EFFORT.DISTANCE.KM", nvisits)
tod_mtx <- get_var_wide(cropped_checklists_shuffled, "tod", nvisits)
doy_mtx <- get_var_wide(cropped_checklists_shuffled, "yday", nvisits)
protocol_mtx <- get_var_wide(cropped_checklists_shuffled, "PROTOCOL.CODE", nvisits)
duration_mtx <- get_var_wide(cropped_checklists_shuffled, "DURATION.MINUTES", nvisits)


site_covariates <- data.frame(precip = cropped_checklists_shuffled$precip,
                              tmean = cropped_checklists_shuffled$tmean,
                              tmax = cropped_checklists_shuffled$tmax,
                              tmin = cropped_checklists_shuffled$tmin,
                              latitude = cropped_checklists_shuffled$LATITUDE,
                              longitude = cropped_checklists_shuffled$LONGITUDE,
                              popdensity = scale(cropped_checklists_shuffled$pop_density),
                              landcover = as.factor(cropped_checklists_shuffled$landcover))[1:nsites,]
observation_covariates <- list(distance = scale(distance_mtx[1:nsites,]),
                               tod = scale(tod_mtx[1:nsites,]), tod_sq = scale(tod_mtx[1:nsites,])^2,
                               doy = scale(doy_mtx[1:nsites,]), doy_sq = scale(doy_mtx[1:nsites,])^2,
                               duration = duration_mtx[1:nsites,],
                               protocol = protocol_mtx[1:nsites,])



occu_frame <- unmarkedFrameOccu(y = observed_mtx, 
                                siteCovs = site_covariates, 
                                obsCovs = observation_covariates)
summary(occu_frame)


system.time(
  occufit <- occu(~ distance + tod + tod_sq + doy + doy_sq + duration + protocol 
                  ~ precip * tmean * tmin + tmax + landcover + popdensity + latitude + longitude,
                  data = occu_frame)
)

summary(occufit)
