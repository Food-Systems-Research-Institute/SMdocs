# Example from R Graph Gallery

# Libraries
library(ggraph)
library(igraph)

# create a data frame giving the hierarchical structure of your individuals. 
# Origin on top, then groups, then subgroups
d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
hierarchy <- rbind(d1, d2)

# create a vertices data.frame. One line per object of our hierarchy, giving features of nodes.
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) )

mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )

# right: using the bundle method (tension = 1)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(alpha=0.1) +
  geom_conn_bundle(data = get_con(from = c(18,20,30), to = c(19, 50, 70)), alpha=1, width=1, colour="skyblue", tension = 1) +
  theme_void()

# create a dataframe with connection between leaves (individuals)
all_leaves <- paste("subgroup", seq(1,100), sep="_")
connect <- rbind( 
  data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
  data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
  data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
  data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) 
)

# The connection object must refer to the ids of the leaves:
from <- match( connect$from, vertices$name)
to <- match( connect$to, vertices$name)

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()



# -------------------------------------------------------------------------

# install arcdiagram
remotes::install_github('gastonstat/arcdiagram')

# load arcdiagram
library(arcdiagram)

# location of 'gml' file
mis_file = "/Users/gaston/lesmiserables.txt"

# read 'gml' file
mis_graph = read.graph(mis_file, format="gml")

# get edgelist
edgelist = get.edgelist(mis_graph)

# get vertex labels
vlabels = get.vertex.attribute(mis_graph, "label")

# get vertex groups
vgroups = get.vertex.attribute(mis_graph, "group")

# get vertex fill color
vfill = get.vertex.attribute(mis_graph, "fill")

# get vertex border color
vborders = get.vertex.attribute(mis_graph, "border")

# get vertex degree
degrees = degree(mis_graph)

# get edges value
values = get.edge.attribute(mis_graph, "value")

# load reshape
library(reshape)

# data frame with vgroups, degree, vlabels and ind
x = data.frame(vgroups, degrees, vlabels, ind=1:vcount(mis_graph))

# arranging by vgroups and degrees
y = arrange(x, desc(vgroups), desc(degrees))

# get ordering 'ind'
new_ord = y$ind

# plot arc diagram
arcplot(edgelist, ordering=new_ord, labels=vlabels, cex.labels=0.8,
        show.nodes=TRUE, col.nodes=vborders, bg.nodes=vfill,
        cex.nodes = log(degrees)+0.5, pch.nodes=21,
        lwd.nodes = 2, line=-0.5,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values)
