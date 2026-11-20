library(tidyverse)
library(pacfintools)
library(nwfscSurvey)
library(r4ss)

load(here::here("data/data_raw/PacFIN.RDBD.bds.14.Aug.2026.RData")) 
#load(here::here("data/data_raw/PacFIN.RDBD.CompFT.14.Aug.2026.RData")) 

#path for saving
save_dir = here::here("data/data_processed/com_length_age_comps")

#Bins for biological data
#some length is in mm, will have to convert
length_bins <- seq(14, 78, by = 2) #min and max of bds_cleaned$lengthcm
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

#remove NET, TLS, and TWS
bds_cleaned <- bds_cleaned %>%
  dplyr::filter(geargroup != "NET") %>%
  dplyr::filter(geargroup != "TLS") %>%
  dplyr::filter(geargroup != "TWS") %>%
  mutate(fleet=replace_when(fleet, geargroup=="HKL" ~ "NONTWL"))%>%
  mutate(fleet=replace_when(fleet, geargroup=="POT" ~ "NONTWL"))

#add sex column
bds_cleaned <- bds_cleaned %>%
#  dplyr::mutate(sex = SEX_CODE)%>%
  dplyr::mutate(stratification = fleet)

#################################################################################
#get_pacfin_expansion function from PacFIN

load(here::here("data/data_processed/survey_length_weight/length_weight_parameters.rda"))#should this always be the parameters form the survey? or could it be ODFW parameters?
load(here::here("data/data_processed/com_length_age_comps/pacfin_catch_aggregated.rda"))

formatted_pacfin_catch <- pacfin_catch |>
  pacfintools::formatCatch(
    strat = c("fleet"),
    valuename = "landings_mt")
    
#remove earliest years of pacfin bds because not available in catch data - look into this - why
bds_cleaned <- bds_cleaned %>%
  dplyr::filter(year > 1980)

#1st and 2nd stage expansion combined
expanded_comps <- pacfintools::get_pacfin_expansions(
  Pdata = bds_cleaned,
  Catch =  formatted_pacfin_catch,
  weight_length_estimates = pars,
  verbose = TRUE
)

#process data
length_comps_long <- pacfintools::getComps(
  Pdata = expanded_comps |>
    dplyr::filter(!is.na(lengthcm)),
  Comps = "LEN",
  weightid = "Final_Sample_Size_L",
  verbose = FALSE
)

data_commercial_lengths_expanded <- pacfintools::writeComps(
  inComps = length_comps_long,
  column_with_input_n = "n_stewart",
  comp_bins = length_bins,
  verbose = FALSE
) |>
  dplyr::arrange(fleet)

utils::write.csv(
  data_commercial_lengths_expanded,
  file = here::here(
    "data", "data_processed","com_length_age_comps",
    paste0("data-commercial-comps-lengths-catch-expanded.csv")#because this is second stage expansion
  ),
  row.names = FALSE
)

length_trips_samples <- length_comps_long |>
  dplyr::ungroup() |>
  dplyr::summarise(
    .by = c(fishyr, fleet),
    trips = sum(unique(n_tows)),
    samples = sum(unique(n_fish)),
    input_n = sum(unique(n_stewart))
  ) |>
  dplyr::rename(year = fishyr)

utils::write.csv(
  length_trips_samples,
  file = here::here(
    "data", "data_processed","com_length_age_comps",
    paste0("data-commercial-bds-n-length.csv")
  ),
  row.names = FALSE
)


#data plots

# investigate sex ratios
table(bds_cleaned$SAMPLE_YEAR, bds_cleaned$SEX_CODE)
table(
  bds_cleaned$SAMPLE_YEAR,
  bds_cleaned$SEX_CODE,
  bds_cleaned$fleet
)

# fraction of unsexed fish by year
unsexed_fracs <- data.frame(
  yr = unique(bds_cleaned$SAMPLE_YEAR),
  unsexed_frac = NA
)
for (irow in 1:nrow(unsexed_fracs)) {
  yr <- unsexed_fracs$yr[irow]
  
  sub <- bds_cleaned$SAMPLE_YEAR == yr
  unsexed_fracs$unsexed_frac[irow] <- sum(
    bds_cleaned[sub, "SEX_CODE"] == "U"
  ) /
    sum(bds_cleaned[sub, "SEX_CODE"] %in% c("F", "M", "U"))
}
unsexed_fracs$unsexed_frac <- round(unsexed_fracs$unsexed_frac, 2)

test <- aggregate(
  bds_cleaned$lengthcm,
  by = list(
    yr = bds_cleaned$SAMPLE_YEAR,
    sex = bds_cleaned$SEX_CODE,
    fleet = bds_cleaned$fleet
  ),
  FUN = mean,
  na.rm = TRUE
)
plot(
  0,
  type = 'n',
  xlim = c(1980, 2026),
  ylim = c(0, 100),
  xlab = "year",
  ylab = "Mean length (cm)"
)
for (fleet in c("MID","NONTWL","TWL")) {
  for (sex in c("F", "M", "U")) {
    lines(
      test[test$fleet == fleet & test$sex == sex, c("yr", "x")],
      pch = sex
    )
  }
}

# Look at the data between lengths and lengths with ages
#will have to add age later
length_sampling <- 
  bds_cleaned |>
    dplyr::mutate(source = "length") |>
    dplyr::select(year, geargroup, fleet, lengthcm, area, source)


gg <- ggplot2::ggplot(
  length_sampling,
  ggplot2::aes(x = lengthcm, fill = source) #would add length-age source when we have age data
) +
  ggplot2::geom_density(alpha = 0.20) +
  ggplot2::facet_grid(. ~ fleet) +
  ggplot2::theme_bw() +
#  ggplot2::theme(
#    legend.background = element_rect(fill = alpha("white", 0.1)),
#    text = ggplot2::element_text(size = 14),
#    strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
#    legend.position = c(0.80, 0.85)
#  ) +
  ggplot2::xlab("Length (cm)") +
  ggplot2::ylab("Density")


ggplot2::ggsave(
  plot = gg,
  filename = here::here(
    "data","data_processed","com_length_age_comps",
    "PacFIN_length_sampling_byfleet.png"
  ),
  width = 10,
  height = 8
)
