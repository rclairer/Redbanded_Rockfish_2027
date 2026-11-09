library(tidyverse)
library(pacfintools)
library(nwfscSurvey)
library(r4ss)

load(here::here("data/data_raw/PacFIN.RDBD.bds.14.Aug.2026.RData")) 
load(here::here("data/data_raw/PacFIN.RDBD.CompFT.14.Aug.2026.RData")) 

#ss_data <- SS_readdat(file="../../model/2017_yelloweye_model_updated_ss3_exe/yelloweye_data.ss")

#path for storing
file_path = here::here("data/data_processed/length_age_comps/")

#Bins for biological data
#some length is in mm, will have to convert
length_bins <- seq(14, 76, by = 2) #min and max of bds.pacfin$FISH_LENGTH
age_bins <- seq(0, 67, by = 1) 

used_gears <- c("HKL","MID","NET","POT","TWL", "TWS","TLS")
good_lengths <- "F"
good_samples <- c("","M","C")
good_methods <- "R"
good_age_method <- c("B", "BB","1")

bds_cleaned <- pacfintools::cleanPacFIN(
  Pdata = bds.pacfin,
  keep_gears = used_gears,
  keep_length_type = good_lengths,
  keep_sample_type = good_samples,
  keep_sample_method = good_methods,
  keep_age_method = good_age_method,
  clean = TRUE
  )#|>
  #mutate(geargroup = ifelse(geargroup!='TWL','NONTWL',geargroup)) 

## make the same but keeping al the records so I can see what was removed
bds_cleaned_full <- pacfintools::cleanPacFIN(
  Pdata = bds.pacfin,
  keep_gears = used_gears,
  keep_length_type = good_lengths,
  keep_sample_type = good_samples,
  keep_sample_method = good_methods,
  keep_age_method = good_age_method,
  clean = FALSE
)#|>
#mutate(geargroup = ifelse(geargroup!='TWL','NONTWL',geargroup)) 
