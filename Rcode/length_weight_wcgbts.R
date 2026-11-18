library(nwfscSurvey)

bds_survey <- nwfscSurvey::pull_bio(
  common_name = "redbanded rockfish",
  survey = "NWFSC.Combo"
)

pars <- nwfscSurvey::estimate_weight_length(
  data = bds_survey,
  col_length = "length_cm",
  col_weight = "weight_kg",
  verbose = FALSE
)

plot_weight_length(
  bds_survey,
  dir = here::here("data/data_processed/survey_length_weight"),
  estimates = pars,
  col_length = "length_cm",
  col_weight = "weight_kg",
  two_sex = TRUE,
  add_save_name = NULL,
  height = 7,
  width = 7,
  dpi = 300
)
