# Interactive Network
# 2025-06-05


# Description -------------------------------------------------------------


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  networkD3,
  htmlwidgets
)


# -------------------------------------------------------------------------


edge_df <- tibble::tribble(
  ~id, ~from, ~to, ~rel, ~label, ~penwidth, ~color, ~fontname, ~fontsize, ~weight, ~constraint,
  1,    1,  6, NA,   183, 5.000000, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  2,    2,  5, NA,     1, 1.021858, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  3,    2,  7, NA,    10, 1.218579, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  4,    2,  8, NA,     5, 1.109290, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  5,    2,  9, NA,     2, 1.043716, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  6,    3,  3, NA,     2, 1.043716, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  7,    3, 10, NA,     1, 1.021858, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  8,    4,  6, NA,    17, 1.371585, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  9,    5,  5, NA,     1, 1.021858, "dodgerblue4",    "Arial",       10,      1,       TRUE,
  10,   5, 10, NA,     5, 1.109290, "dodgerblue4",    "Arial",       10,      1,       TRUE
)

node_df <- tibble::tribble(
  ~id, ~type,               ~label,     ~shape,    ~color_level,      ~style,           ~fontcolor,    ~color, ~tooltip, ~penwidth, ~fixedsize, ~fontname, ~fontsize, ~fillcolor,                     ~Group,
  1,  NA,                      "End",    "circle",         Inf, "rounded,filled",      "brown4",      "brown4",       "ARTIFICIAL_END\n1831",       1.5,     FALSE,    "Arial",       10,     "white",                       "End",
  2,  NA,                    "Start",    "circle",         Inf, "rounded,filled", "chartreuse4", "chartreuse4",     "ARTIFICIAL_START\n1832",       1.5,     FALSE,    "Arial",       10,     "white",                     "Start",
  3,  NA,         "Analyze Done\n18", "rectangle",  0.08333333, "rounded,filled",       "black",        "grey",          "Analyze Done\n183",       1.5,     FALSE,    "Arial",       10,   "#ECE7F2",          "Analyze Done\n18",
  4,  NA,              "Approved\n3", "rectangle",  0.01388889, "rounded,filled",       "black",        "grey",               "Approved\n34",       1.5,     FALSE,    "Arial",       10,   "#FFF7FB",               "Approved\n3",
  5,  NA,"Back from Development\n17", "rectangle",  0.07870370, "rounded,filled",       "black",        "grey", "Back from Development\n175",       1.5,     FALSE,    "Arial",       10,   "#ECE7F2", "Back from Development\n17",
  6,  NA,               "Backlog\n9", "rectangle",  0.04166667, "rounded,filled",       "black",        "grey",                "Backlog\n96",       1.5,     FALSE,    "Arial",       10,   "#FFF7FB",                "Backlog\n9",
  7,  NA,             "Cancelled\n1", "rectangle",  0.00462963, "rounded,filled",       "black",        "grey",              "Cancelled\n17",       1.5,     FALSE,    "Arial",       10,   "#FFF7FB",              "Cancelled\n1",
  8,  NA,              "Closed\n138", "rectangle",  0.63888889, "rounded,filled",       "white",        "grey",               "Closed\n1388",       1.5,     FALSE,    "Arial",       10,   "#74A9CF",               "Closed\n138",
  9,  NA,          "Dispatched\n166", "rectangle",  0.76851852, "rounded,filled",       "white",        "grey",           "Dispatched\n1669",       1.5,     FALSE,    "Arial",       10,   "#3690C0",           "Dispatched\n166",
  10, NA,                "Done\n216", "rectangle",  1.00000000, "rounded,filled",       "white",        "grey",                "Done\n21610",      1.5,     FALSE,    "Arial",        10,    "#034E7B",                 "Done\n216"
)



# Add a dummy "Group" column to nodes dataframe
node_df$Group <- node_df$label

# 
edges_net <- edge_df[, c("from", "to", "label")]
colnames(edges_net) <- c("from", "to", "title")

nodes_net <- node_df[, c("id", "label", "Group", "fillcolor")]
colnames(nodes_net) <- c("id", "node_label", "Group", "nodes_color")

# Subtract 1 from the "from" and "to" columns to zero-index them
edges_net$from <- edges_net$from - 1
edges_net$to <- edges_net$to - 1

# set node size
nodes_net$NodeSize <- 20

# JS 
clickJS <- "
d3.selectAll('.xtooltip').remove(); 
d3.select('body').append('div')
  .attr('class', 'xtooltip')
  .style('position', 'absolute')
  .style('border', '1px solid #999')
  .style('border-radius', '3px')
  .style('padding', '5px')
  .style('opacity', '0.85')
  .style('background-color', '#fff')
  .style('box-shadow', '2px 2px 6px #888888')
  .html(\"<a href=\\\"https://www.npr.org/\" + d.name + \"\\\" target=\\\"_blank\\\">name: \" + d.name + \"<br>group: \" + d.group + \"</a>\")
  .style('left', (d3.event.pageX) + 'px')
  .style('top', (d3.event.pageY - 28) + 'px');
"

clickJS <- "
function(d, i) {
  d3.selectAll('.xtooltip').remove(); 

  // Stop the event from bubbling to the SVG background
  d3.event.stopPropagation();

  d3.select('body').append('div')
    .attr('class', 'xtooltip')
    .style('position', 'absolute')
    .style('border', '1px solid #999')
    .style('border-radius', '3px')
    .style('padding', '5px')
    .style('opacity', '0.85')
    .style('background-color', '#fff')
    .style('box-shadow', '2px 2px 6px #888888')
    .html(\"<a href=\\\"https://www.npr.org/\" + d.name + \"\\\" target=\\\"_blank\\\">name: \" + d.name + \"<br>group: \" + d.group + \"</a>\")
    .style('left', (d3.event.pageX) + 'px')
    .style('top', (d3.event.pageY - 28) + 'px');

  // Add a one-time listener to body to remove the tooltip on background click
  d3.select('body').on('click.xtooltip', function() {
    d3.selectAll('.xtooltip').remove();
    d3.select('body').on('click.xtooltip', null); // Remove the listener
  });
}
"

# plot network
my_network<- networkD3::forceNetwork(
  Links = edges_net,
  Nodes = nodes_net,
  Source = "from",
  Target = "to", 
  Value = "title", 
  NodeID = "node_label",
  Group = "Group",
  colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20);"),
  linkDistance = 300,
  linkWidth = networkD3::JS("function(d) { return Math.sqrt(d.value);}"),
  radiusCalculation = networkD3::JS(" Math.sqrt(d.nodesize)+6"),
  Nodesize = "NodeSize",
  charge = - 30,,
  linkColour = "black",
  opacity = 0.8,
  zoom = T,
  legend = T,
  arrows = FALSE,
  bounded = T, 
  opacityNoHover = 1.5,
  fontSize = 12,
  clickAction = clickJS
  # 
)

# Increase the size of nodes
my_network$x$width <- '1200px'
my_network$x$height <- '800px'


# Get the target variable in fn$x$links (an integer id) to show up as a tooltip when user hovers over a link (i.e. edge) in the graph
fnrender <- htmlwidgets::onRender(
  my_network,
  '
  function(el, x) {
    d3.selectAll(".link").append("svg:title")
      .text(function(d) { return d.source.name + " -> " + d.target.name; })
  }
  '
)
# display the result
fnrender






# -------------------------------------------------------------------------

library(networkD3)
library(htmlwidgets)

# Create hierarchical data
data <- list(
  name = "Root",
  group = "A",
  link = "https://example.com/root",
  children = list(
    list(
      name = "Child 1",
      group = "B",
      link = "https://example.com/child1"
    ),
    list(
      name = "Child 2",
      group = "B",
      link = "https://example.com/child2",
      children = list(
        list(
          name = "Grandchild",
          group = "C",
          link = "https://example.com/grandchild"
        )
      )
    )
  )
)

# Create diagonal network
p <- diagonalNetwork(
  List = data,
  fontSize = 14,
  linkColour = "#ccc",
  nodeColour = "#3182bd",
  height = 600,
  width = 800
)

# Add tooltips and clickable links via JavaScript
customJS <- "
function(el) {
  d3.select(el).selectAll('.node')
    .on('mouseover', function(event, d) {
      d3.selectAll('.xtooltip').remove();
      d3.select('body').append('div')
        .attr('class', 'xtooltip')
        .style('position', 'absolute')
        .style('border', '1px solid #999')
        .style('border-radius', '3px')
        .style('padding', '5px')
        .style('opacity', '0.85')
        .style('background-color', '#fff')
        .style('box-shadow', '2px 2px 6px #888888')
        .html('<a href=\"' + d.data.link + '\" target=\"_blank\">name: ' + d.data.name + '<br>group: ' + d.data.group + '</a>')
        .style('left', (event.pageX + 10) + 'px')
        .style('top', (event.pageY - 28) + 'px');
    })
    .on('mouseout', function() {
      d3.selectAll('.xtooltip').remove();
    });
}
"

# Add custom JS to the widget
p <- htmlwidgets::onRender(p, customJS)

# Save or view
p  # In viewer
# saveWidget(p, "diagonal_network_with_tooltips.html")



# -------------------------------------------------------------------------

URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata//flare.json")

## Convert to list format
Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)

# Use subset of data for more readable diagram
Flare$children = Flare$children[1:3]

radialNetwork(List = Flare, fontSize = 10, opacity = 0.9)

# -------------------------------------------------------------------------


diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9)


