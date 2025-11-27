## code to prepare `dp_fill_palette` dataset goes here

pacman::p_load(
  ggplot2
)

# Palette for figures
dp_fill_palette <- c(
  "Economics"  = "#BF7417FF",
  "Environment"  = "#A8BE74FF",
  "Health" = "#9CB6A9FF",
  "Production" = "#635C72FF",
  "Social" = "#784116FF"
)
usethis::use_data(dp_fill_palette, overwrite = TRUE)


# Palette for text -
dp_text_palette <- c(
  "Economics"  = "#BF7417FF",
  "Environment"  = "#95A150FF",
  "Health" = "#6692BBFF",
  "Production" = "#635C72FF",
  "Social" = "#784116FF"
)
usethis::use_data(dp_text_palette, overwrite = TRUE)

dp_theme <- theme_classic() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title.position = "top",
    panel.grid = element_blank(),
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman", hjust = 0.5),
    legend.text = element_text(family = "Times New Roman")
  )
usethis::use_data(dp_theme, overwrite = TRUE)
