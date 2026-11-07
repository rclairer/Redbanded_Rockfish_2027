library(r4ss)
library(here)

exe_loc <- here::here('models/ss3_win.exe')

#model directory
model_dir <- here::here("models/base_placeholder")

# Copy the template files directly from the package sample data
# 'simple_small' or basic templates provide a clean starting point
#r4ss::copy_SS_inputs(
#  dir.old = system.file("extdata", "simple_small", package = "r4ss"),
#  dir.new = model_dir,
#  overwrite = TRUE
#)

head(inputs$dat)

inputs <- r4ss::SS_read(dir = model_dir)
inputs$dat$Nsexes #number of sexes is 2, because of differences in growth curves and weight at age

inputs$dat$N_areas #number of areas is 1, coastwide

inputs$dat$styr #????

inputs$dat$endyr <- 2026 #is this correct? the end year for data, right?

inputs$dat$Nfleets <- 5 #TWL, NONTWL, MID, TRI, WCGBTS
inputs$dat$fleetnames <- c("TWL", "NONTWL", "MID", "TRI", "WCGBTS")

inputs$dat$fleetinfo #?????
inputs$dat$fleetinfo1 #?????
inputs$dat$fleetinfo2 #??????

#########################################
### HISTORICAL CATCH
#########################################

inputs$dat$catch

#fleets (all commercial, no rec...?)?
#TWL - fleet 1
#NONTWL - fleet 2
#MID

colnames_c <- c("year", "seas", "fleet", "catch", "catch_se")

CA_com_catch_reconstruction <- read.csv(here::here("data/data_processed/com_catch_reconstructions/CA_catch_reconstruction.csv"))
#years 1916 to 1980
#proportion into separate fleets, what to do with UNK????
CA_com_catch_historic_TWL <- CA_com_catch_reconstruction |>
  dplyr::select(Year, TWL_mt) |>
  dplyr::mutate(seas = 1,
                fleet = 1,
                se = 0.01)|> #is 0.01 standard? 
  dplyr::select(Year, seas, fleet, TWL_mt, se)
#Add start line?
# start_line <- data.frame(
#   year = -999,
#   seas = 1,
#   fleet = 1,
#   catch = 0,
#   catch_se = 0.01
# )
colnames(CA_com_catch_historic_TWL) <- colnames_c

CA_com_catch_historic_NONTWL <- CA_com_catch_reconstruction |>
  dplyr::select(Year, NONTWL_mt) |>
  dplyr::mutate(seas = 1,
                fleet = 2,
                se = 0.01)|> #is 0.01 standard? 
  dplyr::select(Year, seas, fleet, NONTWL_mt, se)
#Add start line?
# start_line <- data.frame(
#   year = -999,
#   seas = 1,
#   fleet = 2,
#   catch = 0,
#   catch_se = 0.01
# )
colnames(CA_com_catch_historic_NONTWL) <- colnames_c

  
#########################################
### CONTEMPORARY CATCH
#########################################

inputs$dat$catch

#fleets (all commercial, no rec...?)?
#TWL
#NONTWL
#MID

#########################################
### INDICES
#########################################

inputs$dat$CPUEinfo #CHANGE THIS

inputs$dat$CPUE

colnames_i <- c("year", "month", "index", "obs", "se_log")

#Triennial survey - fleet 4
#placeholder, probably will update with a covariate for timing
triennial_combined <- read.csv(here::here("data/data_processed/survey_indices/triennial/lat_34.5-48.5_years_1980-2004_combined/delta_gamma/index/est_by_area.csv"))
triennial_combined_index <- triennial_combined |>
  dplyr::select(year, est, se) |>
  dplyr::mutate(month = 7, #why do I need month?
                fleet = 4) |>
  dplyr::select(year, month, fleet, est, se)

colnames(triennial_combined_index) <- colnames_i

#WCGBTS - fleet 5

wcgbts_coastwide <- read.csv(here::here("data/data_processed/survey_indices/wcgbts_rerun/redbanded_rockfish/wcgbts/lat_33.5-48.5_years_2003-2025/delta_gengamma/index/est_by_area.csv"))

wcgbts_coastwide_index <- wcgbts_coastwide |>
  dplyr::select(year, est, se) |>
  dplyr::mutate(month = 7, #why do I need month?
                fleet = 5) |>
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

#Triennial - fleet 4

#WCGBTS - fleet 5

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

#WCGBTS - fleet 5

r4ss::SS_write(inputs, dir = model_dir, overwrite = TRUE)

