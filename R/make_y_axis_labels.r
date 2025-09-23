#' Create a function for making y-axis labels for regression graphs
#'
#' This function returns a function for making y-axis labels using a palette for
#' text colors and label dimension lookup from prepare_figure_data(). The labels
#' are colored by dimension, and the indicator half of the combined label is
#' bold.
#'
#' @param label_dimension_lookup A lookup created using the prepare_figure_data
#'   function.
#' @param dp_text_palette A color palette assigning color to dimension to use
#'   when dimensions are colored by text.
#'
#' @return A function for creating y-axis labels to use for regression graphs.
#'
#' @details The function takes label_dimension_lookup and dp_text_palette to
#'   assign palette colors to each dimension. It splits the combined label back
#'   into indicator and metric to bold the indicator then recombines them. It
#'   pastes the newly formatted label and color assignment together.
#'
#' @examples
#' \dontrun{
#' # Sample data frame
#' df <- data.frame(
#'   dimension = c("d1", "d2", "d3", "d4"),
#'   indicator = c("earth", "wind", "fire", "music"),
#'   metric = c("soil", "air", "heat", "notes"),
#'   combined_label = c("earth - soil", "wind - air", "fire - heat", "music - notes"),
#'   estimate = c(0.01, 0.05, -0.2, 0.07)
#'   )
#'
#' # Use make_y_axis_labels when plotting
#' ggplot(data = df, mapping = aes(x = estimate, y = combined_label)) +
#'   geom_point() +
#'   geom_errorbarh() +
#'   geom_vline(xintercept = 0) +
#'   scale_y_discrete(
#'     labels = make_y_axis_labels(label_dimension_lookup, dp_text_palette))
#' }
#'
#' @export
make_y_axis_labels <- function(label_dimension_lookup, dp_text_palette) {
  function(x) {
    sapply(x, function(y) {
      dim_val <- label_dimension_lookup$dimension[label_dimension_lookup$combined_label == y]
      dimension_label_color <- dp_text_palette[as.character(dim_val)]

      # Split the combined label into indicator and metric
      parts <- strsplit(y, " - ", fixed = TRUE)[[1]]
      indicator_bold <- paste0("<b>", parts[1], "</b>")
      metric <- parts[2]

      # Recombine with bold indicator and normal metric
      formatted_label <- paste(indicator_bold, "-", metric)

      paste0('<span style="color:', dimension_label_color, ';">', formatted_label, '</span>')
    })
  }
}
