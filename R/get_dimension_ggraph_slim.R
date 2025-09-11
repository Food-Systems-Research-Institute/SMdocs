#' Create a slim horizontal ggraph visualization for a dimension
#'
#' This function creates a simplified version of the dimension ggraph that shows
#' direct connections from dimension to indicators, bypassing the index level.
#' It's designed for cleaner visualizations when the index level is not needed.
#'
#' @param csv_path Character string specifying the path to a CSV file containing
#'   the framework data. Either this or \code{framework_df} must be provided.
#' @param framework_df Data frame containing the framework data. Either this or
#'   \code{csv_path} must be provided.
#' @param dimension_in Character string specifying the dimension to visualize.
#'   Will be converted to lowercase for consistency.
#' @param include_metrics Logical indicating whether to include metrics in the
#'   visualization. Default is FALSE.
#' @param x_limits Numeric vector of length 2 specifying x-axis limits.
#'   Default is c(0, 0).
#' @param y_limits Numeric vector of length 2 specifying y-axis limits.
#'   Default is c(-1.5, 2.1).
#' @param leaf_font_size Numeric specifying the font size for leaf nodes.
#'   Default is 4.
#' @param index_label_size Numeric specifying the label size for index labels.
#'   Default is 0.1.
#' @param index_font_size Numeric specifying the font size for index labels.
#'   Default is 4.
#' @param palette Character string specifying the color palette to use.
#'   Default is 'basetheme::royal'. Use 'black' for monochrome visualization.
#' @param arrow Arrow specification for edges. Default is NULL.
#' @param slim Logical parameter for compatibility. Not used in this function.
#'   Default is FALSE.
#'
#' @return A ggplot object containing the slim dimension ggraph visualization.
#'
#' @details This slim version creates a two-level hierarchy showing only
#'   dimension-to-indicator connections (and optionally indicator-to-metric).
#'   It provides a cleaner view when the index grouping level is not necessary
#'   for the visualization. The function includes special handling for black
#'   and white color schemes.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter select arrange rename mutate across any_of bind_rows
#' @importFrom stringr str_to_lower str_to_title
#' @importFrom ggraph ggraph geom_edge_diagonal geom_node_text geom_node_label
#'   scale_edge_color_manual
#' @importFrom igraph graph_from_data_frame
#' @importFrom paletteer paletteer_c
#' @importFrom ggplot2 aes scale_colour_manual scale_size_continuous theme_void
#'   theme expand_limits coord_flip scale_y_reverse unit
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Basic slim visualization
#' plot <- get_dimension_ggraph_slim(
#'   csv_path = "framework.csv",
#'   dimension_in = "economic"
#' )
#'
#' # With metrics and custom palette
#' plot <- get_dimension_ggraph_slim(
#'   framework_df = my_data,
#'   dimension_in = "social",
#'   include_metrics = TRUE,
#'   palette = "black"
#' )
#' }
#'
#' @export
get_dimension_ggraph_slim <- function(csv_path = NULL,
                                      framework_df = NULL,
                                      dimension_in,
                                      include_metrics = FALSE,
                                      x_limits = c(0, 0), 
                                      y_limits = c(-1.5, 2.1),
                                      leaf_font_size = 4,
                                      index_label_size = 0.1,
                                      index_font_size = 4,
                                      palette = 'basetheme::royal',
                                      arrow = NULL,
                                      slim = FALSE) {
  # Put input in lower case for consistency
  dimension_in <- stringr::str_to_lower(dimension_in)
  
  # Logic to take either path to csv 
  if (!is.null(csv_path)) {
    df <- readr::read_csv(csv_path)
  } else if (!is.null(framework_df)) {
    df <- framework_df
  } else {
    stop('\nMust provide either path or framework as a dataframe.')
  }
  
  # Filter to dimension, but put back to title case
  df <- df %>% 
    setNames(c(stringr::str_to_lower(names(.)))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c('dimension', 'index', 'indicator')), ~ stringr::str_to_lower(.x))) %>% 
    dplyr::filter(dimension == dimension_in)
  
  # Metric logic
  if (include_metrics == TRUE) {
    df <- df %>% 
      dplyr::select(dimension, indicator, metric) %>% 
      dplyr::arrange(desc(indicator), desc(metric))
  } else if (include_metrics == FALSE) {
    df <- df %>% 
      dplyr::select(dimension, index, indicator) %>% 
      dplyr::arrange(desc(indicator))
  }
  
  ## Make edges
  # Include groupings by dimension, then combine them
  edges <- list()
  
  # Logic to allow for slim graph without dimension or index
  edges$dim_ind <- df %>% 
    dplyr::select(dimension, indicator) %>% 
    unique() %>% 
    dplyr::rename(from = dimension, to = indicator) %>% 
    dplyr::mutate(group = to)
  
  # Logic for include_metrics
  edges$ind_met <- df %>% 
    dplyr::select(indicator, metric) %>% 
    unique() %>% 
    dplyr::rename(from = indicator, to = metric) %>% 
    dplyr::mutate(group = from)
    
  edges <- dplyr::bind_rows(edges)
  
  ## Make vertices
  # Each line is a single vertex (dimension, index, or indicator)
  # We are just giving them random values to control point size for now
  vertices = data.frame(
    name = unique(c(as.character(edges$from), as.character(edges$to)))
    # value = runif(nrow(edges) + 1)
  )
  
  # Add the dimension groupings to the vertices as well
  vertices$group = edges$group[match(vertices$name, edges$to)]
  
  # IDs for vertices
  vertices$id = NA
  myleaves = which(is.na(match(vertices$name, edges$from)))
  nleaves = length(myleaves)
  vertices$id[myleaves] = seq(1:nleaves)
  
  
  ## Sort for colors
  unique_groups <- na.omit(unique(vertices$group))
  if (palette == 'black') {
    n <- length(unique_groups)
    group_colors = setNames(
      rep('black', n),
      unique_groups
    )
  } else {
    n <- length(unique_groups)
    full_pal <- paletteer::paletteer_c(palette, 100)
    half_n <- 50
    first_half <- full_pal[1:half_n]
    idx <- round(seq(1, half_n, length.out = n))
    even_colors <- first_half[idx]
    group_colors <- setNames(
      even_colors,
      unique_groups
    )
    
    # group_colors <- setNames(
    #   rev(paletteer_d(palette, length(unique_groups), direction = -1)), 
    #   unique_groups
    # )
  }
  
  edges <- edges %>%
    dplyr::mutate(group = factor(group, levels = names(group_colors)))
  
  # If including metrics, save names of indicators, used later for labeling
  if (include_metrics == TRUE) {
    indicator_names <- unique(df$indicator)
  } else {
    indicator_names <- NULL
  }
  
  ## Create graph
  # Make ggraph object from edges and vertices
  graph <- igraph::graph_from_data_frame(edges, vertices = vertices)
  
  # Plot the graph
  plot <- ggraph::ggraph(graph, layout = 'dendrogram', circular = FALSE) +
    
    # Color edges by dimension
    ggraph::geom_edge_diagonal(
      ggplot2::aes(color = group), 
      width = 0.6, 
      arrow = arrow
    ) +
    
    # Create text for indicators using angles, hjust, and dimension groupings
    ggraph::geom_node_text(
      ggplot2::aes(
        x = x,
        y = y,
        filter = leaf,
        label = name,
        # colour = group
        colour = ifelse(grepl('NONE', name), 'red', group)
        # This doesn't work, but making them disappear is just as good
      ),
      size = leaf_font_size,
      alpha = 1,
      hjust = -0.01,
      vjust = 0.5
    ) +
    
    # # Label the Indices within the graph
    # geom_node_label(
    #   aes(label = ifelse(
    #     name == group | name == dimension_in | name %in% indicator_names, 
    #     str_to_title(name), 
    #     NA
    #   )),
    #   label.padding = unit(0.2, "lines"),
    #   label.r = unit(0.3, "lines"),
    #   label.size = index_label_size,
    #   size = index_font_size
    # ) +
    
    # Various formatting options
    ggplot2::scale_colour_manual(values = group_colors) +
    ggraph::scale_edge_color_manual(values = group_colors) +
    ggplot2::scale_size_continuous(range = c(0.1, 7)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
    ) +
    ggplot2::expand_limits(x = x_limits, y = y_limits) +
    
    # Flip it so it oges left to right
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse()
  
  # Node labels
  plot <- plot +
    ggraph::geom_node_label(
      ggplot2::aes(label = ifelse(
        name == group | name == dimension_in | name %in% indicator_names | name == 'root',
        stringr::str_to_title(name),
        NA
      )),
      label.padding = ggplot2::unit(0.2, "lines"),
      label.r = ggplot2::unit(0.3, "lines"),
      label.size = index_label_size,
      size = index_font_size
    )

  return(plot)
}