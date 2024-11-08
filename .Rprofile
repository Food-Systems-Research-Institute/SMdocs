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
  .quiet = TRUE
)

options(scipen = 999)

source('startup.R')

cat("\n.Rprofile loaded")
