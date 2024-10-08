
# Example -----------------------------------------------------------------


pacman::p_load(
  ggraph,
  igraph,
  dplyr,
  RColorBrewer,
  viridisLite
)

# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges=rbind(d1, d2)

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(111)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name, edges$to ) ]


#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))



# Example data conversion -------------------------------------------------


data <- data.frame(
  level1="CEO",
  level2=c( rep("boss1",4), rep("boss2",4)),
  level3=paste0("mister_", letters[1:8])
)

edges_level1_2 <- data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 <- data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)

mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()



# Try it ------------------------------------------------------------------


dat <- readRDS('data/tree_dat.rds') %>% 
  mutate(Framework = 'Sustainability') %>% 
  select(Framework, Dimension:Indicator)
get_str(dat)

# Make edge levels, then combine
edges <- list()
edges$sm_dim <- dat %>% 
  select(Framework, Dimension) %>% 
  unique() %>% 
  rename(from = Framework, to = Dimension) %>% 
  mutate(group = to)
edges$dim_ind <- dat %>% 
  select(Dimension, Index) %>% 
  unique() %>% 
  rename(from = Dimension, to = Index) %>% 
  mutate(group = from)
edges$ind_ind <- dat %>% 
  select(Index, Indicator) %>% 
  unique() %>% 
  rename(from = Index, to = Indicator) %>% 
  mutate(group = edges$dim_ind$from[match(.$from, edges$dim_ind$to)])
edges <- bind_rows(edges)

# Create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(nrow(edges) + 1)
) 

# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$group[match(vertices$name, edges$to)]

# calculate the ANGLE of the labels
vertices$id = NA
myleaves = which(is.na(match(vertices$name, edges$from)))
nleaves = length(myleaves)
vertices$id[myleaves] = seq(1:nleaves)
vertices$angle = 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse(vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)



# Create a graph object
mygraph <- graph_from_data_frame(edges, vertices = vertices)



# Make the plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  # geom_edge_diagonal() +
  geom_edge_diagonal(aes(color = group), width = 0.5) +
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
    size = 2.7,
    alpha = 1
  ) +
  geom_node_point(aes(
    filter = leaf,
    # label = name,
    x = x * 1.07,
    y = y * 1.07,
    colour = group,
    size = value,
    alpha = 0.2
  )) +
  geom_node_label(
    aes(
      label = name,
      filter = !leaf
    ),
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.3, "lines"),
    label.size = 0.1,
    repel = TRUE,
    max.overlaps = 100,
    seed = 42,
    size = 3,
    arrow = arrow(
      angle = 30,
      length = unit(0.05, "inches"),
      ends = "last",
      type = "open"
    )
  ) +
  scale_colour_manual(values = brewer.pal(5, 'Set1')) +
  scale_edge_color_manual(values = brewer.pal(5, 'Set1')) +
  scale_size_continuous(range = c(0.1, 10)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  expand_limits(x = c(-1.75, 1.75), y = c(-1.75, 2))



