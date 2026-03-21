pacman::p_load(collapsibleTree, dplyr, data.world)

# -------------------------------------------------------------------------

# From docs
summary(Geography)
data(Geography)

collapsibleTree(
  Geography,
  hierarchy = c("continent", "type", "country"),
  width = 800,
  zoomable = FALSE
)

# -------------------------------------------------------------------------


# Example data with hierarchy
df <- data.frame(
  level1 = "USA",
  level2 = c("Northeast", "South"),
  level3 = c("New York", "Texas"),
  url = c("https://en.wikipedia.org/wiki/New_York", "https://en.wikipedia.org/wiki/Texas")
)

collapsibleTree(
  df,
  hierarchy = c("level1", "level2", "level3"),
  linkLength = 200,
  collapsed = FALSE,
  nodeSize = "url",
  tooltip = TRUE,
  fill = "level2"
  # onClick = htmlwidgets::JS("
  #   function(node) {
  #     if (node.url) {
  #       window.open(node.url, '_blank');
  #     }
  #   }
  # ")
)


# -------------------------------------------------------------------------


