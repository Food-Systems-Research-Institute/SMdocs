# Get dimension ggraph
# 2024-12-02

# Take tree dat for a dimension and make horizontal ggraph.
# Requires data with Dimension, Index, and Indicator, alphabetical by index
# and indicator

pacman::p_load(
  ggraph,
  igraph,
  dplyr,
  readr,
  RColorBrewer,
  paletteer,
  snakecase
)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::summarize(),
  dplyr::arrange(),
  .quiet = TRUE
)

get_dimension_ggraph <- function(csv_path = NULL,
                                 framework_df = NULL,
                                 dimension_in,
                                 include_metrics = FALSE,
                                 x_limits = c(0, 0), 
                                 y_limits = c(-1.5, 2.1),
                                 leaf_font_size = 4,
                                 index_label_size = 0.1,
                                 index_font_size = 4,
                                 palette = 'basetheme::royal') {
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
  
  # Filter to dimension
  df <- df %>% 
    setNames(c(stringr::str_to_lower(names(.)))) %>%
    mutate(across(any_of(c('dimension', 'index', 'indicator')), ~ stringr::str_to_lower(.x))) %>% 
    dplyr::filter(dimension == dimension_in)
  
  # Metric logic
  if (include_metrics == TRUE) {
    df <- df %>% 
      dplyr::select(dimension, index, indicator, metric) %>% 
      dplyr::arrange(desc(index), desc(indicator), desc(metric))
  } else {
    df <- df %>% 
      select(dimension, index, indicator) %>% 
      dplyr::arrange(desc(index), desc(indicator))
  }
  
  ## Make edges
  # Include groupings by dimension, then combine them
  edges <- list()
  edges$dim_ind <- df %>% 
    dplyr::select(dimension, index) %>% 
    unique() %>% 
    dplyr::rename(from = dimension, to = index) %>% 
    dplyr::mutate(group = to)
  edges$ind_ind <- df %>% 
    dplyr::select(index, indicator) %>% 
    unique() %>% 
    dplyr::rename(from = index, to = indicator) %>% 
    dplyr::mutate(group = from)
  
  # Logic for include_metrics
  if (include_metrics == TRUE) {
    edges$ind_met <- df %>% 
      dplyr::select(indicator, metric) %>% 
      unique() %>% 
      dplyr::rename(from = indicator, to = metric) %>% 
      dplyr::mutate(group = edges$ind_ind$group[match(.$from, edges$ind_ind$to)])
  }
  edges <- bind_rows(edges)
  
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
  group_colors <- setNames(
    paletteer_d(palette, length(unique_groups), direction = -1), 
    unique_groups
  )
  edges <- edges %>%
    mutate(group = factor(group, levels = names(group_colors)))
  
  # If including metrics, save names of indicators, used later for labeling
  if (include_metrics == TRUE) {
    indicator_names <- unique(df$indicator)
  } else {
    indicator_names <- NULL
  }
  
  ## Create graph
  # Make ggraph object from edges and vertices
  graph <- graph_from_data_frame(edges, vertices = vertices)
  
  # Plot the graph
  ggraph(graph, layout = 'dendrogram', circular = FALSE) +
    
    # Color edges by dimension
    geom_edge_diagonal(aes(color = group), width = 0.6) +
    
    # Create text for indicators using angles, hjust, and dimension groupings
    geom_node_text(
      aes(
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
      hjust = 0,
      vjust = 0.5
    ) +
    
    # Label the Indices within the graph
    geom_node_label(
      aes(label = ifelse(
        name == group | name == dimension_in | name %in% indicator_names, 
        name, 
        NA
      )),
      label.padding = unit(0.2, "lines"),
      label.r = unit(0.3, "lines"),
      label.size = index_label_size,
      size = index_font_size
    ) +
    
    # Various formatting options
    scale_colour_manual(values = group_colors) +
    scale_edge_color_manual(values = group_colors) +
    scale_size_continuous(range = c(0.1, 7)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    expand_limits(x = x_limits, y = y_limits) +
    
    # Flip it so it oges left to right
    coord_flip() +
    scale_y_reverse()
}