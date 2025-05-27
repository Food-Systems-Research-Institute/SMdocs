## Edge Bundling Diagram

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
 
# create a data frame giving the hierarchical structure of your individuals
set.seed(1234)
d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
hierarchy <- rbind(d1, d2)
 
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
  name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) , 
  value = runif(111)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group  <-  hierarchy$from[ match( vertices$name, hierarchy$to ) ]
 
 
# Create a graph object
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )
 
# The connection object must refer to the ids of the leaves:
from  <-  match( connect$from, vertices$name)
to  <-  match( connect$to, vertices$name)
 

p <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()



# Use the 'value' column of the connection data frame for the color:
p +  geom_conn_bundle(data = get_con(from = from, to = to), aes(colour=value, alpha=value)) 
 
# In this case you can change the color palette
p +  
  geom_conn_bundle(data = get_con(from = from, to = to), aes(colour=value)) +
  scale_edge_color_continuous(low="white", high="red")
p +  
  geom_conn_bundle(data = get_con(from = from, to = to), aes(colour=value)) +
  scale_edge_colour_distiller(palette = "BuPu")
 
# Color depends of the index: the from and the to are different
p +  
  geom_conn_bundle(data = get_con(from = from, to = to), width=1, alpha=0.2, aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  theme(legend.position = "none")

cor_dat <- cor_r %>% 
  mutate(across(c(1:2), as.character)) %>% 
  filter(var_1 != var_2) %>% 
  rowwise() %>%
  mutate(pair = paste(sort(c(var_1, var_2)), collapse = "_")) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)
get_str(cor_dat)


## Vertices
# Need a vertices data frame with every leaf. The example provides a random value as well, but I don't think we need this.
# Example:
vertices

# Make it:
vert <- data.frame(
  name = unique(c(cor_dat$var_1, cor_dat$var_2)),
  group = '1',
  value = runif(35)
) %>% 
  bind_rows(c(name = 'origin', group = NA_character_))


## Hierarchy
hierarchy
# Do we really need this? I don't care about groups
# Try making it with a single group. Pull it from cor_r
hier <- data.frame(
  from = 'origin',
  to = vert$name
)


## Connections
connect
# cor_dat is this already, just clean it up
con <- cor_dat %>% 
  setNames(c('from', 'to', 'value'))



## Graph
# Create a graph object
mygraph <- graph_from_data_frame(hier, vertices = vert)
 
# The connection object must refer to the ids of the leaves:
from  <-  match( con$from, vert$name)
to  <-  match( con$to, vert$name)
 


p <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()




# Use the 'value' column of the connection data frame for the color:
p +  geom_conn_bundle(
  data = get_con(from = from, to = to), 
  aes(
    # colour = value, 
    alpha = value
  )
) 
 
# In this case you can change the color palette
p +  
  geom_conn_bundle(data = get_con(from = from, to = to), aes(colour=value)) +
  scale_edge_color_continuous(low="white", high="red")
p +  
  geom_conn_bundle(data = get_con(from = from, to = to), aes(colour=value)) +
  scale_edge_colour_distiller(palette = "BuPu")
 
# Color depends of the index: the from and the to are different
p +  
  geom_conn_bundle(data = get_con(from = from, to = to), width=1, alpha=0.2, aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  theme(legend.position = "none")
