library(r4ss)
library(here)

exe_loc <- here::here('models/ss3_win.exe')

#model directory
model_dir <- here::here("models/base_placeholder")

# Copy the template files directly from the package sample data
# 'simple_small' or basic templates provide a clean starting point
r4ss::copy_SS_inputs(
  dir.old = system.file("extdata", "simple_small", package = "r4ss"),
  dir.new = model_dir,
  overwrite = TRUE
)

head(inputs$dat)

inputs <- r4ss::SS_read(dir = model_dir)
inputs$dat$Nsexes #number of sexes is 2, because of differences in growth curves and weight at age

inputs$dat$N_areas #number of areas is 1, coastwide

inputs$dat$styr #????

inputs$dat$endyr <- 2026 #is this correct? the end year for data, right?

#########################################
### HISTORICAL CATCH
#########################################

#no catch info?

inputs$dat$catch

#fleets (all commercial, no rec...?)?
#TWL
#NONTWL
#MID



#########################################
### CONTEMPORARY CATCH
#########################################

#no catch info?

inputs$dat$catch

#fleets (all commercial, no rec...?)?
#TWL
#NONTWL
#MID

colnames_c <- c("year", "seas", "fleet", "catch", "catch_se")

#########################################
### INDICES
#########################################

inputs$dat$CPUEinfo #CHANGE THIS

inputs$dat$CPUE

colnames_i <- c("year", "month", "index", "obs", "se_log")

#Triennial survey - fleet 1
#placeholder, probably will update with a covariate for timing
triennial_combined <- read.csv(here::here("data/data_processed/survey_indices/triennial/lat_34.5-48.5_years_1980-2004_combined/delta_gamma/index/est_by_area.csv"))
triennial_combined_index <- triennial_combined |>
  dplyr::select(year, est, se) |>
  dplyr::mutate(month = 7, #why do I need month?
                fleet = 1) |>
  dplyr::select(year, month, fleet, est, se)

colnames(triennial_combined_index) <- colnames_i

#WCGBTS - fleet 2

wcgbts_coastwide <- read.csv(here::here("data/data_processed/survey_indices/wcgbts_rerun/redbanded_rockfish/wcgbts/lat_33.5-48.5_years_2003-2025/delta_gengamma/index/est_by_area.csv"))

wcgbts_coastwide_index <- wcgbts_coastwide |>
  dplyr::select(year, est, se) |>
  dplyr::mutate(month = 7, #why do I need month?
                fleet = 2) |>
  dplyr::select(year, month, fleet, est, se)

colnames(wcgbts_coastwide_index) <- colnames_i

all_indices <- do.call(
  "rbind",
  list(triennial_combined_index,
       wcgbts_coastwide_index
      )
    )

r4ss::SS_write(inputs, dir = model_dir, overwrite = TRUE)

#########################################
### COMMERCIAL LENCOMP
#########################################
inputs$dat$len_info #CHANGE THIS

inputs$dat$lencomp

#PacFIN WCGOP
#PacFIN
#WCGOP 

#no rec???

#########################################
### SURVEY LENCOMP
#########################################
#????? Nsamp method used for all survey length comps is the old Stewart Hamel method from the 2017 assessment where
#????? Nsamp = n_trips + 0.0707 * n_fish when n_fish/n_tows < 55 and
#????? Nsamp = 4.89 * n_trips when n_fish/n_tows >= 55

inputs$dat$len_info #CHANGE THIS

inputs$dat$lencomp

#Triennial - fleet 1

#WCGBTS - fleet 2

#########################################
### COMMERCIAL AGECOMP
#########################################

inputs$dat$age_info #CHANGE THIS

inputs$dat$agecomp

#no rec????

#########################################
### SURVEY AGECOMP
#########################################
#???? Nsamp method used for all survey maal is the old Stewart Hamel method from the 2017 assessment where
#???? Nsamp = n_trips + 0.0707 * n_fish when n_fish/n_tows < 55 and
#???? Nsamp = 4.89 * n_trips when n_fish/n_tows >= 55

#no Triennial

#WCGBTS - fleet 2


