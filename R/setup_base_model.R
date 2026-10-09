library(r4ss)
library(here)

exe_loc <- here::here('models/ss3_win.exe')

#model directory
model_dir <- here::here("models/base")

# Copy the template files directly from the package sample data
# 'simple_small' or basic templates provide a clean starting point
r4ss::copy_SS_inputs(
  dir.old = system.file("extdata", "simple_small", package = "r4ss"),
  dir.new = model_dir,
  overwrite = TRUE
)
