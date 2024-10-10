pacman::p_load(
    conflicted
)

conflicted::conflicts_prefer(
    dplyr::filter(),
    dplyr::select(),
    dplyr::summarize(),
    dplyr::pivot_wider()
)

print("startup.R loaded")