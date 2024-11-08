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

source('startup.R')

cat("\n.Rprofile loaded")
