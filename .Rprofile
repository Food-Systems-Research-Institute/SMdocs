suppressPackageStartupMessages(
  pacman::p_load(
    dplyr,
    conflicted,
    projecter,
    quarto
  )
)

conflicted::conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::arrange(),
  dplyr::rename(),
  dplyr::summarize(),
  dplyr::bind_rows(),
  dplyr::as_data_frame(),
  .quiet = TRUE
)

options(
  scipen = 999,
  pillar.print_max = 950,
  pilar.print_min = 950
)

source('startup.R')

cat("\n.Rprofile loaded")
