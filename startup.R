pacman::p_load(
    conflicted
)

conflicted::conflicts_prefer(
    dplyr::filter(),
    dplyr::select(),
    dplyr::summarize(),
    dplyr::pivot_wider(),
    dplyr::rename(),
    ape::rotate(),
    .quiet = TRUE
)

cat("\nstartup.R loaded")