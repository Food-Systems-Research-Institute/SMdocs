
# Edge Bundling -----------------------------------------------------------


# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

# create a data frame giving the hierarchical structure of your individuals
set.seed(1234)
d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges <- rbind(d1, d2)

# create a dataframe with connection between leaves (individuals)
all_leaves <- paste("subgroup", seq(1,100), sep="_")
connect <- rbind( 
  data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
  data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
  data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
  data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
connect$value <- runif(nrow(connect))

# create a vertices data.frame. One line per object of our hierarchy
vertices  <-  data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(111)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]





#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id <- NA
myleaves <- which(is.na( match(vertices$name, edges$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)
vertices$angle <- 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)





# Create a graph object
mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )

# The connection object must refer to the ids of the leaves:
from  <-  match( connect$from, vertices$name)
to  <-  match( connect$to, vertices$name)

# Basic usual argument
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", width=0.9) +
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=1.5, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))



# With colors and such
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  geom_conn_bundle(
    data = get_con(from = from, to = to),
    alpha = 0.2,
    width = 0.9,
    aes(colour = ..index..)
  ) +
  scale_edge_colour_distiller(palette = "RdPu") +
  
  geom_node_text(
    aes(
      x = x * 1.15,
      y = y * 1.15,
      filter = leaf,
      label = name,
      angle = angle,
      hjust = hjust,
      colour = group
    ),
    size = 2,
    alpha = 1
  ) +
  
  geom_node_point(aes(
    filter = leaf,
    x = x * 1.07,
    y = y * 1.07,
    colour = group,
    size = value,
    alpha = 0.2
  )) +
  scale_colour_manual(values = rep(brewer.pal(9, "Paired") , 30)) +
  scale_size_continuous(range = c(0.1, 10)) +
  
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))



# Chord Diagram -----------------------------------------------------------

# https://r-graph-gallery.com/chord-diagram-interactive.html

devtools::install_github("mattflor/chorddiag")
library(chorddiag)

# Create dummy data
m <- matrix(
  c(
    11975, 5871, 8916, 2868,
    1951, 10048, 2060, 6171,
    8010, 16145, 8090, 8045,
    1013, 990, 940, 6907
  ),
  byrow = TRUE,
  nrow = 4, ncol = 4
)

# A vector of 4 colors for 4 groups
haircolors <- c("black", "blonde", "brown", "red")
dimnames(m) <- list(
  have = haircolors,
  prefer = haircolors
)
groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")

# Build the chord diagram:
p <- chorddiag(m, groupColors = groupColors, groupnamePadding = 20)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/chord_interactive.html"))



# edgebundleR -------------------------------------------------------------


# http://garthtarr.github.io/edgebundleR/articles/edgebundleR.html

pacman::p_load(
  edgebundleR,
  igraph,
  data.table
)



## Example -----------------------------------------------------------------


d <- structure(list(ID = c("KP1009", "GP3040", "KP1757", "GP2243",
                           "KP682", "KP1789", "KP1933", "KP1662", "KP1718", "GP3339", "GP4007",
                           "GP3398", "GP6720", "KP808", "KP1154", "KP748", "GP4263", "GP1132",
                           "GP5881", "GP6291", "KP1004", "KP1998", "GP4123", "GP5930", "KP1070",
                           "KP905", "KP579", "KP1100", "KP587", "GP913", "GP4864", "KP1513",
                           "GP5979", "KP730", "KP1412", "KP615", "KP1315", "KP993", "GP1521",
                           "KP1034", "KP651", "GP2876", "GP4715", "GP5056", "GP555", "GP408",
                           "GP4217", "GP641"),
                    Type = c("B", "A", "B", "A", "B", "B", "B",
                             "B", "B", "A", "A", "A", "A", "B", "B", "B", "A", "A", "A", "A",
                             "B", "B", "A", "A", "B", "B", "B", "B", "B", "A", "A", "B", "A",
                             "B", "B", "B", "B", "B", "A", "B", "B", "A", "A", "A", "A", "A",
                             "A", "A"),
                    Set = c(15L, 1L, 10L, 21L, 5L, 9L, 12L, 15L, 16L,
                            19L, 22L, 3L, 12L, 22L, 15L, 25L, 10L, 25L, 12L, 3L, 10L, 8L,
                            8L, 20L, 20L, 19L, 25L, 15L, 6L, 21L, 9L, 5L, 24L, 9L, 20L, 5L,
                            2L, 2L, 11L, 9L, 16L, 10L, 21L, 4L, 1L, 8L, 5L, 11L),
                    # Loc = c(3L, 2L, 3L, 1L, 3L, 3L, 3L, 1L, 2L,
                    #         1L, 3L, 1L, 1L, 2L, 2L, 1L, 3L,
                    #         2L, 2L, 2L, 3L, 2L, 3L, 2L, 1L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L,
                    #         1L, 3L, 2L, 3L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 2L, 2L, 3L, 3L)),
                    Loc = c(
                      rep(1, 12),
                      rep(2, 12),
                      rep(3, 12),
                      rep(4, 12)
                    )),
               .Names = c("ID", "Type", "Set", "Loc"), class = "data.frame",
               row.names = c(NA, -48L))

# let's add Loc to our ID
d$key <- d$ID
d$ID <- paste0(d$Loc,".",d$ID)

# Get vertex relationships
sets <- unique(d$Set[duplicated(d$Set)])
rel <-  vector("list", length(sets))
for (i in 1:length(sets)) {
  rel[[i]] <- as.data.frame(t(combn(subset(d, d$Set == sets[i])$ID, 2)))
}
rel <- rbindlist(rel)

# Get the graph
g <- graph.data.frame(rel, directed=F, vertices=d)
clr <- as.factor(V(g)$Loc)
levels(clr) <- c("salmon", "wheat", "lightskyblue", 'black')
V(g)$color <- as.character(clr)
V(g)$size = degree(g)*3
E(g)$weight <- c(
  rep(1, 15),
  rep(3, 15),
  rep(4, 16)
)

# igraph static plot
# plot(g, layout = layout.circle, vertex.label=NA)

edgebundle( g )


## Adjust first example ----------------------------------------------------



# Example edge list
edges <- data.frame(from = c("A", "B", "C"), to = c("D", "E", "F"), weight = c(1, 3, 50))

# Create an igraph object
g <- graph_from_data_frame(edges, directed = FALSE)

# Assign edge weight
E(g)$weight <- edges$weight

# Plot with edge bundling
edgebundle(g)



## Fake hierarchy ----------------------------------------------------------


library(igraph)
library(edgebundleR)
library(data.table)

# Example dataset (Pairs with correlation strengths)
edges <- data.frame(
  from = c("A", "B", "C", "D", "E"),
  to = c("F", "G", "H", "I", "J"),
  correlation = c(0.2, 0.8, 0.5, 0.9, 0.4)  # Strength of connection
)

# Create a "fake" hierarchy (each node is its own category)
vertices <- data.frame(
  name = unique(c(edges$from, edges$to)),
  group = rep(1, length(unique(c(edges$from, edges$to)))) # Dummy grouping
)

# Convert to igraph
g <- graph_from_data_frame(edges, directed = FALSE, vertices = vertices)

# Set edge width based on correlation (normalized)
E(g)$width <- edges$correlation * 5  # Adjust scale as needed

# Set edge color based on strength (gradient from blue to red)
color_scale <- colorRampPalette(c("blue", "red"))
E(g)$color <- color_scale(100)[as.numeric(cut(edges$correlation, breaks = 100))]

# Plot with edge bundling
edgebundle(g)




library(ggraph)
library(tidygraph)

g_tbl <- as_tbl_graph(g)

ggraph(g_tbl, layout = "circle") + 
  geom_edge_arc(aes(edge_width = correlation, color = correlation)) + 
  scale_edge_color_gradient(low = "blue", high = "red") + 
  geom_node_point(size = 5) +
  theme_void()



flaregraph <- tbl_graph(flare$vertices, flare$edges)
from <- match(flare$imports$from, flare$vertices$name)
to <- match(flare$imports$to, flare$vertices$name)
ggraph(flaregraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1) + 
  coord_fixed()



## Example -----------------------------------------------------------------


require(edgebundleR)
require(MASS)
sig = kronecker(diag(12),matrix(2,5,5)) + 3*diag(60)
X = MASS::mvrnorm(n=100,mu=rep(0,60),Sigma = sig)
colnames(X) = paste(rep(c("Sample1.Left.A.A","Sample1.Left.B.B","Sample1.Left.C.C",
                          "Sample1.Right.A.A","Sample1.Right.B.B","Sample1.Right.C.C",
                          "Sample2.Left.A.A","Sample2.Left.B.B","Sample2.Left.C.C",
                          "Sample2.Right.A.A","Sample2.Right.B.B","Sample2.Right.C.C"),
                        each=5),1:5,sep="")
Y = X[,sample(dim(X)[2])]
edgebundle(cor(Y),cutoff=0.2,tension=0.8,fontsize = 14)



pacman::p_load(
  huge
)
data("stockdata")
# generate returns sequences
X = log(stockdata$data[2:1258,]/stockdata$data[1:1257,])
# perform some regularisation
out.huge = huge(cor(X),method = "glasso",lambda=0.56,verbose = FALSE)
# identify the linkages
adj.mat = as.matrix(out.huge$path[[1]])
# format the colnames
nodenames = paste(gsub("","",stockdata$info[,2]),stockdata$info[,1],sep=".")
head(cbind(stockdata$info[,2],stockdata$info[,1],nodenames))
colnames(adj.mat) = rownames(adj.mat) = nodenames
# restrict attention to the connected stocks:
adj.mat = adj.mat[rowSums(adj.mat)>0,colSums(adj.mat)>0]
# plot the result
edgebundle(adj.mat,tension=0.8,fontsize = 10)




library(igraph)
library(edgebundleR)

# Compute correlation matrix
cor_mat <- cor(Y)

# Convert correlation matrix to an edge list (only strong correlations)
cutoff <- 0.2  # Adjust as needed
edges <- which(abs(cor_mat) > cutoff, arr.ind = TRUE)

# Remove self-loops (correlation of variables with themselves)
edges <- edges[edges[,1] != edges[,2],]

# Create a data frame with the correlations as weights
edge_list <- data.frame(
  from = colnames(Y)[edges[,1]],
  to = colnames(Y)[edges[,2]],
  weight = cor_mat[edges]  # Correlation value as edge weight
)

# Convert to igraph object
g <- graph_from_data_frame(edge_list, directed = FALSE)

# Set edge width based on correlation strength
E(g)$width <- abs(E(g)$weight) * 5  # Scale factor for better visibility
E(g)$width <- abs(E(g)$weight) * 100  # Scale factor for better visibility

# Set edge color (e.g., positive = blue, negative = red)
E(g)$color <- ifelse(E(g)$weight > 0, "blue", "red")

# Plot with edgebundleR
edgebundle(g, tension = 0.4, fontsize = 14)




library(igraph)
library(ggraph)
library(ggplot2)
library(tidygraph)

# Compute correlation matrix
cor_mat <- cor(Y)

# Define cutoff for significant correlations
cutoff <- 0.2  
edges <- which(abs(cor_mat) > cutoff, arr.ind = TRUE)
edges <- edges[edges[,1] != edges[,2],]

# Create an edge list with correlation strength
edge_list <- data.frame(
  from = colnames(Y)[edges[,1]],
  to = colnames(Y)[edges[,2]],
  weight = cor_mat[edges]
)

# Convert to igraph object
g <- graph_from_data_frame(edge_list, directed = FALSE)

# Convert to tbl_graph for ggraph
g_tbl <- as_tbl_graph(g)

# Plot using ggraph with edge bundling
ggraph(g_tbl, layout = "dendrogram", circular = TRUE) +
  geom_edge_bundle(aes(width = abs(weight), color = weight), alpha = 0.8) +
  geom_node_point(size = 3, color = "black") +
  scale_edge_width(range = c(0.2, 3)) +  # Adjust range for visibility
  scale_edge_color_gradient2(low = "red", mid = "white", high = "blue") +
  theme_void()
