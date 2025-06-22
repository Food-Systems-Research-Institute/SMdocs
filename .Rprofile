suppressPackageStartupMessages(
  pacman::p_load(
    dplyr,
    conflicted
  )
)

pacman::p_load_gh('ChrisDonovan307/projecter')

conflicted::conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::arrange(),
  dplyr::as_data_frame(),
  dplyr::summarize(),
  dplyr::arrange(),
  dplyr::pull(),
  .quiet = TRUE
)

options(
  scipen = 999,
  pillar.print_max = 950,
  pilar.print_min = 950
)

# Wrapper function for vim setup
try(vim <- function() rstudiovim::rsvim_exec_file())

# Convenience function to load SMdata package
sm_load <- function() devtools::load_all('../SMdata')
  
cat("\n.Rprofile loaded")
source("renv/activate.R")
