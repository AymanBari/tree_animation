##############################
# Author(s): Ayman Bari 
# 13-April-2020 
# Description: Generate animatiosn of 
#   Use case in mind - illustration for reproduction factor in spread of edpidemics: 
#   https://towardsdatascience.com/will-covid-19-overwhelm-our-healthcare-systems-f5bc49bc77a2
##############################


##### LOAD LIBRARIES #####
# library(animation)  
# library(DataCombine)
# library(ggthemes)
# library(ggplot2)    
library(igraph)     # Converting dataframes to graph objects 
library(tidyverse)  # dplyr, tibble 
library(babynames)  # Not used here - for converting node / vertex ids to names
library(ggraph)     # Plot tbl_graph objects 
library(tidygraph)  # as_tbl_graph -> convert dataframes to table graph objects
library(gganimate)  # animate graphs 


##### FUNCITONS ##### 
source("full_tree_animation_src.R")


##### CREATE DATA FRAMES FOR GRAPHS ##### 
#Graph for R0 = 1, height = 5, single chain 
edges_r0_1 <- edges_data_frame_2(children = 1, height = 5) %>% add_generation_edges(1, 5, .)
nodes_r0_1 <- create_nodes_df(edges_r0_1) %>% add_generation_nodes(1, 5, .)

# Graph for R0 = 5, height = 5, single chain 
edges_r0_2 <- edges_data_frame_2(children = 2, height = 5) %>% add_generation_edges(2, 5, .)
nodes_r0_2 <- create_nodes_df(edges_r0_2) %>% add_generation_nodes(2, 5, .)

# Graph for R0 = 5, height = 5, single chain 
edges_r0_5 <- edges_data_frame_2(children = 5, height = 5) %>% add_generation_edges(5, 5, .)
nodes_r0_5 <- create_nodes_df(edges_r0_5) %>% add_generation_nodes(5, 5, .)


##### CREATE GIFS WITH GGANIMATE ##### 
# GIF for R0 = 1, height = 5
g_r0_1 <- graph_from_data_frame(edges_r0_1, vertices = nodes_r0_1) %>%
  as_tbl_graph() %>% ggraph(layout = "tree", circular = TRUE) + 
  transition_states(states = generation, transition_length = 0, state_length = 0.2) +
  shadow_mark(color = "grey", size = 2) +
  geom_node_point(size = 3, color = "red") +
  geom_edge_fan(color = "grey") + 
  theme_void()
# Review animation  
animate(g_r0_1, fps = 25)
# Save animation
anim_save("r0_1.gif", last_animation())

# GIF for R0 = 1, height = 5
g_r0_2 <- graph_from_data_frame(edges_r0_2, vertices = nodes_r0_2) %>%
  as_tbl_graph() %>% ggraph(layout = "tree", circular = T) + 
  transition_states(states = generation, transition_length = 0, state_length = 0.2) +
  shadow_mark(color = "grey", size = 2) +
  geom_node_point(size = 3, color = "red") +
  geom_edge_bend(color = "grey") +
  theme_void()
# Review animation 
animate(g_r0_2, nframes = 20, duration = 5, end_pause = 5)
# Save animation
anim_save("r0_2.gif", last_animation())

# GIF for R0 = 1, height = 5
g_r0_5 <- graph_from_data_frame(edges_r0_5, vertices = nodes_r0_5) %>%
  as_tbl_graph() %>% ggraph(layout = "stress", circular = T) + 
  transition_states(states = generation, transition_length = 0, state_length = 0.2) +
  shadow_mark(color = "grey", size = 2) +
  geom_node_point(size = 3, color = "red") + 
  geom_edge_fan(color = "grey") + 
  theme_void()
# Review animation  
animate(g_r0_5, nframes = 20, duration = 5, end_pause = 5)
# Save animation
anim_save("r0_5.gif", last_animation())


##### PLOT IMPACT OF R0 ON TOTAL AND ACTIVE NUMBER OF CASES ##### 
# Dataframe to plot active / total cases for R0=1
r0_1_df <- nodes_r0_1 %>% 
  group_by(generation) %>% 
  summarise(active_cases = n()) %>% 
  mutate(total_cases = 0, week = (generation*5) - 5, R0 = "1") %>% 
  total_cases() 

# Dataframe to plot active / total cases for R0=2
r0_2_df <- nodes_r0_2 %>% 
  group_by(generation) %>% 
  summarise(active_cases = n()) %>% 
  mutate(total_cases = 0, week = (generation*5) - 5, R0 = "2") %>% 
  total_cases()

# Dataframe to plot active / total cases for R0=5
r0_5_df <- nodes_r0_5 %>% 
  group_by(generation) %>% 
  summarise(active_cases = n()) %>% 
  mutate(total_cases = 0, week = (generation*5) - 5, R0 = "5") %>% 
  total_cases()

# Bind rows, all in single DF to plot  
r0_125_df <- bind_rows(r0_1_df, r0_2_df, r0_5_df)

# Plot graph for active cases each week 
r0_125_plot <- r0_125_df %>% ggplot(aes(x = week, y = active_cases)) + 
  transition_reveal(along = week) + 
  geom_line(aes(color = R0)) + 
  geom_point(aes(color = R0)) + 
  theme_minimal() + 
  labs(x = "Week", y = "Active Cases") 
# Review animation  
animate(r0_125_plot, nframes = 50, duration = 5, end_pause = 10)
# Save animation
anim_save("r0_125_plot.gif", last_animation())

