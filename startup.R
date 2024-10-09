pacman::p_load(
    conflicted
)

conflicted::conflicts_prefer(
    dplyr::filter(),
    dplyr::select()
)

print("startup.R loaded")