get_res_plots <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
}