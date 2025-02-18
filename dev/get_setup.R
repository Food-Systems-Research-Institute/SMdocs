# Get Conflicted
# 2025-02-18


# Description -------------------------------------------------------------

#' Convenience function to load conflicted and prevent common conflicts at the 
#' top of each Quarto script. Note that this shouldn't be necessary, as there
#' must be a better way for them to carry over, but we have not found that way
#' yet. 


# Function ----------------------------------------------------------------


get_setup <- function() {
  pacman::p_load(
    dplyr,
    conflicted
  )
  conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    dplyr::arrange(),
    dplyr::summarize(),
    dplyr::as_data_frame(),
    .quiet = TRUE
  )
  pacman::p_load_gh('ChrisDonovan307/projecter')
}

