---
title: "Economic Indicator Refinement"
---

```{r}
#| label: not_setup
#| echo: false
pacman::p_load(
  conflicted,
  dplyr,
  skimr,
  readr,
  stringr,
  purrr,
  ggplot2,
  ggpubr,
  reactable,
  tidyr,
  forcats,
  paletteer
)

conflicts_prefer(
  dplyr::select(),
  dplyr::filter(),
  dplyr::mutate(),
  dplyr::select(),
  dplyr::full_join(),
  dplyr::arrange(),
  dplyr::bind_rows(),
  dplyr::group_by(),
  dplyr::summarize(),
  .quiet = TRUE
)
```

This page describes the various iterations of indicator sets for the economics dimensions. First, we observe the indicators included in the dimension at three points in time. The second section then shows the results of the survey following the indicator refinement meeting. A final set of indicators to incorporate into the next RFP is still in the works!

## Indicator Progression

### Wiltshire

This graph shows the original framework as described in the Wiltshire et al. paper.

```{r}
#| label: wiltshire_dendro_test
#| code-fold: true
#| warning: false
#| fig-height: 6
#| fig-width: 8
#| fig-align: center
get_dimension_ggraph(
 # 'data/trees/econ_wiltshire_tree.csv',
 # 'data/trees/econ_meeting_tree.csv',
 'data/trees/econ_tree.csv',
 'Economics',
 y_limits = c(-1.5, -2.1)
)
```

```{r}
#| label: wiltshire_dendro
#| code-fold: true
#| warning: false
#| fig-height: 6
#| fig-width: 8
#| fig-align: center
## Load packages
pacman::p_load(
  ggraph,
  igraph,
  dplyr,
  RColorBrewer,
  viridisLite
)

## Load data and add an origin level
dat <- read_csv('data/trees/econ_wiltshire_tree.csv') %>% 
  select(Dimension, Index, Indicator) %>% 
  arrange(desc(Index), desc(Indicator))

## Make edges
# include groupings by dimension, then combine them
edges <- list()
edges$dim_ind <- dat %>% 
  select(Dimension, Index) %>% 
  unique() %>% 
  rename(from = Dimension, to = Index) %>% 
  mutate(group = to)
edges$ind_ind <- dat %>% 
  select(Index, Indicator) %>% 
  unique() %>% 
  rename(from = Index, to = Indicator) %>% 
  mutate(group = from)
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
# group_colors <- setNames(viridis(length(unique_groups)), unique_groups)
group_colors <- setNames(
  paletteer_d("ggthemes::few_Dark", length(unique_groups), direction = -1), 
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
    size = 4,
    alpha = 1,
    hjust = 0,
    vjust = 0.5
  ) +
  
  # Label the Indices within the graph
  geom_node_label(
    aes(label = ifelse(name == group | name == 'Economics', name, NA)),
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.3, "lines"),
    label.size = 0.1,
    size = 4
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
  expand_limits(x = c(0, 0), y = c(-1.5, 2.1)) +
  
  # Flip it so it oges left to right
  coord_flip() +
  scale_y_reverse()
```

### Matrix

Here is the current set of indicators in the matrix, following the Sustainability Metrics workshop in July, 2024

```{r}
#| label: matrix_dendro
#| code-fold: true
#| warning: false
#| fig-height: 8
#| fig-width: 8
#| fig-align: center
## Load packages
pacman::p_load(
  ggraph,
  igraph,
  dplyr,
  RColorBrewer,
  viridisLite
)

## Load data
dat <- read_csv('data/trees/econ_tree.csv') %>% 
  select(Dimension, Index, Indicator) %>%
  arrange(desc(Index), desc(Indicator))

## Make edges
# include groupings by dimension, then combine them
edges <- list()
edges$dim_ind <- dat %>% 
  select(Dimension, Index) %>% 
  unique() %>% 
  rename(from = Dimension, to = Index) %>% 
  mutate(group = to)
edges$ind_ind <- dat %>% 
  select(Index, Indicator) %>% 
  unique() %>% 
  rename(from = Index, to = Indicator) %>% 
  mutate(group = from)
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
  paletteer_d("ggthemes::few_Dark", length(unique_groups), direction = -1), 
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
    size = 4,
    alpha = 1,
    hjust = 0,
    vjust = 0.5
  ) +
  
  # Label the Indices within the graph
  geom_node_label(
    aes(label = ifelse(name == group | name == 'Economics', name, NA)),
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.3, "lines"),
    label.size = 0.1,
    size = 4
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
  expand_limits(x = c(0, 0), y = c(-1.5, 2.1)) +
  
  # Flip it so it oges left to right
  coord_flip() +
  scale_y_reverse()
```

### Refinement Meeting

Finally, the tentative set of indicators following the indicator refinement meeting on November 15th, 2024

```{r}
#| label: meeting_dendro
#| code-fold: true
#| warning: false
#| fig-height: 3
#| fig-width: 8
#| fig-align: center
## Load packages
pacman::p_load(
  ggraph,
  igraph,
  dplyr,
  RColorBrewer,
  viridisLite
)

## Load data and add an origin level
dat <- read_csv('data/trees/econ_meeting_tree.csv') %>%
  select(Dimension, Index, Indicator) %>% 
  arrange(desc(Index), desc(Indicator))

## Make edges
# include groupings by dimension, then combine them
edges <- list()
edges$dim_ind <- dat %>% 
  select(Dimension, Index) %>% 
  unique() %>% 
  rename(from = Dimension, to = Index) %>% 
  mutate(group = to)
edges$ind_ind <- dat %>% 
  select(Index, Indicator) %>% 
  unique() %>% 
  rename(from = Index, to = Indicator) %>% 
  mutate(group = from)
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
  paletteer_d("ggthemes::few_Dark", length(unique_groups), direction = -1), 
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
    size = 4,
    alpha = 1,
    hjust = 0,
    vjust = 0.5
  ) +
  
  # Label the Indices within the graph
  geom_node_label(
    aes(label = ifelse(name == group | name == 'Economics', name, NA)),
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.3, "lines"),
    label.size = 0.1,
    size = 4
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
  expand_limits(x = c(0, 0), y = c(-1.5, 2.15)) +
  
  # Flip it so it goes left to right
  coord_flip() +
  scale_y_reverse()
```

## Survey

These are the results from the follow-up survey to the economic indicator refinement meeting on November 15th. This feedback will be used to refine the framework for the next RFP.

### Indicators

```{r}
#| label: survey_prep
#| warnings: false
raw <- read_csv('data/surveys/econ_survey.csv')

dat <- raw %>% 
  select(
    starts_with('Q'),
    -ends_with('RANK')
  ) %>% 
  setNames(c(
    'indi_must',
    'indi_probably',
    'indi_probably_not',
    'indi_must_not',
    paste0('add_indi_', 1:3),
    'notes',
    'idx_must',
    'idx_probably',
    'idx_probably_not',
    'idx_must_not',
    paste0('add_idx_', 1:3),
    'idx_notes',
    'final_notes'
  )) %>% 
  .[-c(1:2), ]

groups <- select(dat, indi_must:indi_must_not, idx_must:idx_probably_not)

to_df <- function(x) {
  x %>% 
    str_split(',') %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame() %>% 
    setNames(c('indicator', 'freq')) %>% 
    arrange(desc(freq))
}

indi_out <- map(groups[1:4], to_df)
idx_out <- map(groups[5:7], to_df)

# Add scores by multipliers
multipliers <- c(3:0)
ind_tables <- map2(indi_out, multipliers, ~ {
  .x %>% 
    mutate(
      freq = as.numeric(freq),
      multiplier = .y,
      score = freq * multiplier,
    ) %>% 
    select(indicator, freq, score)
})

# Set up DF for color graph 
graph_table <- imap(ind_tables, ~ {
  col_name <- str_remove(.y, 'indi_')
  .x %>% 
    rename(!!sym(col_name) := freq) %>% 
    select(-score)
}) %>% 
  reduce(full_join) %>% 
  mutate(
    across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)),
    sort_key = must * 1e6 + probably * 1e4 + probably_not * 1e2 + must_not,
    indicator = fct_reorder(indicator, sort_key, .desc = TRUE)
  ) %>% 
  pivot_longer(
    cols = must:must_not,
    names_to = "category",
    values_to = "count"
  ) %>% 
  mutate(
    category = fct_relevel(
      category, 
      "must_not",
      "probably_not", 
      "probably", 
      "must"
    )
  ) %>%
  group_by(indicator) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()
```

```{r}
#| label: indi_graph
#| fig-width: 15
#| fig-height: 12
#| fig-align: center
#| out-width: 90%
ggplot(graph_table, aes(
  y = reorder(indicator, sort_key),
  x = proportion, 
  fill = category
)) +
  geom_col(position = "stack") +  
  labs(
    y = "Indicator",
    x = "Proportion",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = 'top'
    ) +
  scale_fill_brewer(
    palette = "RdBu", 
    direction = -1,
    limits = c(
      "must",
      "probably", 
      "probably_not", 
      "must_not" 
    ),
    labels = c(
      "Must Include", 
      "Probably Include", 
      "Probably Not Include", 
      "Must Not Include"
    )
  )
```

We are coding this so "Must Include" is worth 3 points, "Probably Include" is worth 2 points, "Probably Not Include" is worth 1 point, and "Must Not Include" is worth 0 points. Note that the last column is the sum of proportions of "Must Include" and "Probably Include". You can sort, search, expand, or page through the table below.

```{r}
#| label: indicator_scores
#| warnings: false
# Add category to tables
props <- ind_tables %>% 
  imap(~ .x %>% mutate(cat = .y)) %>% 
  bind_rows() %>% 
  select(-score)
 
# Get proportion of probably include OR must include
prop_prob_or_must_include <- props %>% 
  filter(cat %in% c('indi_must', 'indi_probably')) %>% 
  group_by(indicator) %>% 
  summarize(prop_include = sum(freq) / 6) %>% 
  arrange(desc(prop_include))

# Get proportion of must include
prop_must_include <- props %>% 
  filter(cat == 'indi_must') %>% 
  group_by(indicator) %>% 
  summarize(prop_must = sum(freq) / 6) %>% 
  arrange(desc(prop_must))

# Add up weighted scores
ind_scores <- ind_tables %>% 
  bind_rows() %>% 
  group_by(indicator) %>% 
  summarize(score = sum(score, na.rm = TRUE)) %>% 
  arrange(desc(score))

# Join everything together
scores_table <- ind_scores %>% 
  full_join(prop_must_include) %>% 
  full_join(prop_prob_or_must_include) %>% 
  arrange(desc(score)) %>% 
  mutate(
    across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)),
    across(c(3:4), ~ format(round(.x, 2), nsmall = 2))
  ) %>% 
  setNames(c('Indicator', 'Score', 'Proportion Must Include', 'Proportion Must OR Probably Include'))
```

```{r}
#| label: indi_reactable 
#| class: centered-table
reactable(
  scores_table,
  sortable = TRUE,
  resizable = TRUE,
  filterable = TRUE,
  searchable = TRUE,
  pagination = TRUE,
  bordered = TRUE,
  wrap = TRUE,
  rownames = FALSE,
  striped = TRUE,
  pageSizeOptions = c(5, 10, 25, 50, 100),
  defaultPageSize = 10,
  showPageSizeOptions = TRUE,
  compact = TRUE,
  fullWidth = FALSE, #
  defaultColDef = colDef(
    minWidth = 200, 
    align = 'right',
    format = colFormat(digits = 2)
  ),
  columns = list(
    Indicator = colDef(align = 'left'),
    Score = colDef(minWidth = 75, format = colFormat(digits = 0))
  )
)
```

### Indices

```{r}
#| label: idx_graphs
#| warnings: false
#| fig-width: 10
#| fig-height: 4
#| fig-align: center
#| out-width: 75%
idx_out <- map(groups[5:7], to_df)

# Add scores by multipliers
multipliers <- c(3:1)
idx_tables <- map2(idx_out, multipliers, ~ {
  .x %>% 
    mutate(
      freq = as.numeric(freq),
      multiplier = .y,
      score = freq * multiplier,
    ) %>% 
    select(index = indicator, freq, score)
})

# Set up DF for color graph 
graph_table <- imap(idx_tables, ~ {
  col_name <- str_remove(.y, 'idx_')
  .x %>% 
    rename(!!sym(col_name) := freq) %>% 
    select(-score)
}) %>% 
  reduce(full_join) %>% 
  mutate(
    across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)),
    sort_key = must * 1e6 + probably * 1e4 + probably_not,
    index = fct_reorder(index, sort_key, .desc = TRUE)
  ) %>% 
  pivot_longer(
    cols = must:probably_not,
    names_to = "category",
    values_to = "count"
  ) %>% 
  mutate(
    category = fct_relevel(
      category, 
      "probably_not", 
      "probably", 
      "must"
    )
  ) %>%
  group_by(index) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()


colors <- RColorBrewer::brewer.pal(4, 'RdBu')[2:4]

ggplot(graph_table, aes(
  y = reorder(index, sort_key),
  x = proportion, 
  fill = category
)) +
  geom_col(position = "stack") +  
  labs(
    y = "Index",
    x = "Proportion",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    legend.position = 'top'
    ) +
  scale_fill_manual(
    values = rev(colors),
    limits = c(
      "must",
      "probably",
      "probably_not"
    ),
    labels = c(
      "Must Include",
      "Probably Include",
      "Probably Not Include"
    )
  )
```

The indices are going through the same treatment as indicators above - scored from 3 to 0. Note that there were no indices rated as "Must Not Include". 

```{r}
#| label: index_score_table
#| warnings: false
#| class: centered-table
# Add category to tables
props <- idx_tables %>% 
  imap(~ .x %>% mutate(cat = .y)) %>% 
  bind_rows() %>% 
  select(-score)
 
# Get proportion of probably include OR must include
prop_prob_or_must_include <- props %>% 
  filter(cat %in% c('idx_must', 'idx_probably')) %>% 
  group_by(index) %>% 
  summarize(prop_include = sum(freq) / 6) %>% 
  arrange(desc(prop_include))

# Get proportion of must include
prop_must_include <- props %>% 
  filter(cat == 'idx_must') %>% 
  group_by(index) %>% 
  summarize(prop_must = sum(freq) / 6) %>% 
  arrange(desc(prop_must))

# Add up weighted scores
idx_scores <- idx_tables %>% 
  bind_rows() %>% 
  group_by(index) %>% 
  summarize(score = sum(score, na.rm = TRUE)) %>% 
  arrange(desc(score))

# Join everything together
idx_scores_table <- idx_scores %>% 
  full_join(prop_must_include) %>% 
  full_join(prop_prob_or_must_include) %>% 
  arrange(desc(score)) %>% 
  mutate(
    across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)),
    across(c(3:4), ~ format(round(.x, 2), nsmall = 2))
  ) %>% 
  setNames(c('Index', 'Score', 'Proportion Must Include', 'Proportion Must OR Probably Include'))

reactable(
  idx_scores_table,
  sortable = TRUE,
  resizable = TRUE,
  filterable = TRUE,
  searchable = TRUE,
  pagination = FALSE,
  bordered = TRUE,
  wrap = TRUE,
  rownames = FALSE,
  striped = TRUE,
  defaultPageSize = 10,
  showPageSizeOptions = FALSE,
  compact = TRUE,
  fullWidth = FALSE,
  defaultColDef = colDef(
    minWidth = 200, 
    align = 'right',
    format = colFormat(digits = 2)
  ),
  columns = list(
    Index = colDef(align = 'left'),
    Score = colDef(minWidth = 75, format = colFormat(digits = 0))
  )
)
```
