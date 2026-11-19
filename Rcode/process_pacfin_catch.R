library(tidyverse)
library(pacfintools)
library(nwfscSurvey)
library(r4ss)

#load(here::here("data/data_raw/PacFIN.RDBD.bds.14.Aug.2026.RData")) 
load(here::here("data/data_raw/PacFIN.RDBD.CompFT.14.Aug.2026.RData")) 

# summary(as.factor(catch.pacfin$PACFIN_GROUP_GEAR_CODE))
#HKL    MSC    NET    POT    TLS    TWL    TWS 
#66661      1  10215   1357      1 270065   1055 

# Code to remove records from OR prior to 1987, WA prior to 1995, remove records
# Do I need to do this?
# outside the EEZ, and assign gears to a fleet
pacfin_catch <- catch.pacfin |>
  dplyr::rename(Year = LANDING_YEAR) |>
  dplyr::filter(
    # To do: The removal of WA data prior to 1995 needs to be reviewed
    #!(AGENCY_CODE == "W" & Year %in% 0:(catch_comm_WA_yearstartpacfin - 1)),
    #!(AGENCY_CODE == "O" & Year < or_first_pacfin_year),
    INPFC_AREA_TYPE_CODE != "XX"
  ) |>
  dplyr::filter(PACFIN_GROUP_GEAR_CODE != "MSC") %>%
  dplyr::filter(PACFIN_GROUP_GEAR_CODE != "NET") %>%
  dplyr::filter(PACFIN_GROUP_GEAR_CODE != "TLS") %>%
  dplyr::filter(PACFIN_GROUP_GEAR_CODE != "TWS") %>%
  dplyr::mutate(
    PACFIN_GROUP_GEAR_CODE=replace_when(PACFIN_GROUP_GEAR_CODE, PACFIN_GEAR_CODE=="MDT"~"MID")
  )|>
  dplyr::mutate(
    fleet=case_when(PACFIN_GROUP_GEAR_CODE=="MID"~"MID",
                    PACFIN_GROUP_GEAR_CODE=="TWL"~'TWL',
                    PACFIN_GROUP_GEAR_CODE=="HKL"~'NONTWL',
                    PACFIN_GROUP_GEAR_CODE=="POT"~"NONTWL"
  ))|>
dplyr::mutate(
  state = dplyr::case_when(
    AGENCY_CODE == "C" ~ "CA",
    AGENCY_CODE == "O" ~ "OR",
    .default = "WA"
 ) ) |>
  dplyr::summarize(
    .by = c(Year, fleet),
    source = "pacfin",
    landings_mt = sum(ROUND_WEIGHT_MTONS)
  )
    
save(pacfin_catch, file = here::here("data/data_processed/com_length_age_comps/pacfin_catch_aggregated.rda"))
