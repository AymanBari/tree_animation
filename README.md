# Complete tree animation 
Gerentates edges and nodes dataframes to represent complete tree graphs. 

## Required libraries 
``` 
library(tidyverse)  # dplyr, tibble 
library(igraph)     # Converting dataframes to graph objects 
library(tidygraph)  # as_tbl_graph() -> convert dataframes to table graph objects
library(ggraph)     # Plot tbl_graph objects 
library(gganimate)  # animate graph plots  
library(babynames)  # Optional - for adding names to edge ids 
```

## Sample Usage 
```R
# Create edges and nodes dataframes for complete trees. 
# Add generation attribute to each entry (used for transition states in animation aesthetic)
edges <- edges_data_frame_2(children = 2, height = 5) %>% add_generation_edges(1, 5, .)
nodes <- create_nodes_df(edges_r0_1) %>% add_generation_nodes(1, 5, .)

# Plot and animate with ggraph and gganimage 
g <- graph_from_data_frame(edges, vertices = nodes) %>%
  as_tbl_graph() %>% ggraph(layout = "tree", circular = TRUE) + 
  transition_states(states = generation, transition_length = 0, state_length = 0.2) +
  shadow_mark(color = "grey", size = 2) +
  geom_node_point(size = 3, color = "red") +
  geom_edge_fan(color = "grey") + 
  theme_void()
# Review animation  
animate(g, fps = 25)
# Save animation 
anim_save("g.gif", last_animation())

```
