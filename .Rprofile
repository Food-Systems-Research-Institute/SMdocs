suppressPackageStartupMessages(
  pacman::p_load(
    dplyr,
    conflicted,
    projecter
  )
)

conflicted::conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::rename(),
  dplyr::as_data_frame(),
  .quiet = TRUE
)

options(scipen = 999)

source('startup.R')

cat("\n.Rprofile loaded")
