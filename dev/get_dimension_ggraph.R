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
  paletteer
)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::summarize(),
  dplyr::arrange(),
  .quiet = TRUE
)

get_dimension_ggraph <- function(path, 
                                 dimension,
                                 x_limits = c(0, 0), 
                                 y_limits = c(-1.5, 2.1),
                                 leaf_font_size = 4,
                                 index_label_size = 0.1,
                                 index_font_size = 4,
                                 palette = 'basetheme::royal') {
  df <- readr::read_csv(path) %>% 
    dplyr::filter(Dimension == dimension) %>% 
    dplyr::arrange(desc(Index), desc(Indicator))
  
  ## Make edges
  # Include groupings by dimension, then combine them
  edges <- list()
  edges$dim_ind <- df %>% 
    dplyr::select(Dimension, Index) %>% 
    unique() %>% 
    dplyr::rename(from = Dimension, to = Index) %>% 
    dplyr::mutate(group = to)
  edges$ind_ind <- df %>% 
    dplyr::select(Index, Indicator) %>% 
    unique() %>% 
    dplyr::rename(from = Index, to = Indicator) %>% 
    dplyr::mutate(group = from)
  edges <- bind_rows(edges)
  
  ## Make vertices
  # Each line is a single vertex (dimension, index, or indicator)
  # We are just giving them random values to control point size for now
  vertices = data.frame(
    name = unique(c(as.character(edges$from), as.character(edges$to))),
    value = runif(nrow(edges) + 1)
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
  
  
  ## Create graph
  # Make ggraph object from edges and vertices
  graph <- graph_from_data_frame(edges, vertices = vertices)
  
  # Plot the graph
  ggraph(graph, layout = 'dendrogram', circular = FALSE) +
    
    # Color edges by dimension
    geom_edge_diagonal(aes(color = group), width = 0.5) +
    
    # Create text for indicators using angles, hjust, and dimension groupings
    geom_node_text(
      aes(
        x = x,
        y = y,
        filter = leaf,
        label = name,
        colour = group
      ),
      size = leaf_font_size,
      alpha = 1,
      hjust = 0,
      vjust = 0.5
    ) +
    
    # Label the Indices within the graph
    geom_node_label(
      aes(label = ifelse(name == group | name == dimension, name, NA)),
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