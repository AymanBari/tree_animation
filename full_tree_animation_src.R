##############################
# Author(s): Ayman Bari 
# 13-April-2020 
# Description: Source file - 
#   Functions to create dataframes for network plots.  
#   Use case in mind - illustration for reproduction factor in spread of edpidemics: 
#   https://towardsdatascience.com/will-covid-19-overwhelm-our-healthcare-systems-f5bc49bc77a2 
##############################

##### FUNCTIONS #####
# Function to get the number of nodes in full tree 
number_of_nodes <- function(children, height) {
  x <- rep(children:children, len = height)
  y <- seq(0, height-1)
  z <- x^y
  n <- sum(z)
  n
}

# Function to get the number of edges in full tree 
number_of_edges <- function(number_of_nodes) {
  number_of_nodes - 1
}

# Function to create edges data frame for igraph plots - depracated 
edges_data_frame <- function(number_of_edges, children) {
  from <- rep(1:floor(number_of_edges/children), each = children)
  to <- 2:(number_of_edges+1)
  edges_df <- cbind(from, to)
  edges_df
}

# Alternate (simpler) function to create edges data frame 
edges_data_frame_2 <- function(children, height) {
  nodes <- number_of_nodes(children, height)
  edges <- number_of_edges(nodes)
  from <- rep(1:floor(edges/children), each = children)
  to <- 2:(nodes)
  edges_df <- cbind(from, to) %>%
    as.data.frame() %>%
    mutate(generation = 0, state = 0)
  edges_df
}

# Function to create nodes_df from tree edges_df, with generation and state columns 
create_nodes_df <- function(edges_df) {
  nodes_df <- edges_df %>% 
    graph_from_data_frame() %>% 
    V() %>% 
    as.vector() %>% 
    as.tibble() %>% 
    mutate(generation = 0, state = 0)
  nodes_df
}

# Function to fill generation column in nodes_df 
add_generation_nodes <- function(children, height, nodes_df) {
  # Ensure that the nodes_df passed matches the number of children and height paramaetes 
  no_of_nodes <- number_of_nodes(children, height)
  test_case <- no_of_nodes == nrow(nodes_df)
  if (test_case) {
    # Reset the nodes_df generation column 
    nodes_df$generation <- 0
    power_counter <- 0
    # Each generation is one level in the tree. For each level,  
    for (gen in 1:height) {
      # Get a vector of indicies where the generation is not yet set in nodes_df
      index <- which(nodes_df$generation == 0) 
      # find the node with the lowest index in this generation, and take that as the starting position 
      start_position <- min(index) 
      # find the node with the highest index in this generation, set to end_position 
      end_position <- (start_position + children^power_counter)-1
      # write the current generation number into the df generation column 
      nodes_df$generation[start_position:end_position] <- gen 
      # Increment counter used to calculate end_position for each generation (for each loop)
      power_counter <- power_counter + 1
    }
  } else {
    print("children and height parameters do not match the number of rows in the edges_df parameter passed")
  }
  nodes_df
}

# Function to add generation column to edges_df 
add_generation_edges <- function(children, height, edges_df) {
  # Ensure that the edges_df passed matches the number of children and height paramaetes 
  no_of_edges <- number_of_edges(number_of_nodes(children, height))
  test_case <- no_of_edges == nrow(edges_df)
  if (test_case) {
    # Reset the edges_df generation column 
    edges_df$generation = 0
    power_counter <- 1
    # Each generation is one level in the tree. For each level,  
    for (gen in 1:(height-1)) {
      # Get a vector of indicies where the generation is not yet set in edges_df
      index <- which(edges_df$generation == 0) 
      # find the node with the lowest index in this generation, and take that as the starting position 
      start_position <- min(index) 
      # find the node with the highest index in this generation, set to end_position 
      end_position <- (start_position + children^power_counter)-1
      # write the current generation number into the df generation column 
      edges_df$generation[start_position:end_position] <- gen 
      # Increment counter used to calculate end_position for each generation (for each loop)
      power_counter <- power_counter + 1
    }
  } else {
    print("children and height parameters do not match the number of rows in the edges_df parameter passed")
  }
  edges_df$generation <- edges_df$generation + 1
  edges_df
}

# Function to get vector of random, unique names 
# Used in edges_to_names function 
get_random_name <- function(n) {
  name <- sample(babynames$name, n, replace = FALSE)
  name
}

# Function to convert numbers in edges data fram to names
# Use if we want to add name labels to the network 
edges_to_names <- function(edges_df) { 
  # Get the number of edges required 
  n <- nrow(edges_df) + 1 
  # Create names and indicies df - key for FindReplace 
  names <- get_random_name(n) %>% 
    as.data.frame() %>% 
    mutate(index = 1:n) 
  # Set column names in df 
  colnames(names) <- c("name", "index") 
  # Convert edges_df columnts from numeric to character - required for FindReplace function 
  edges_df[, 'from'] <- as.character(edges_df[, 'from'])
  edges_df[, 'to'] <- as.character(edges_df[, 'to'])
  # Replace all occurances of the numbers in edges_df$to column, with the corresponding name in names_df 
  named_edges_df <- FindReplace(data = edges_df, 
                                Var = "to", 
                                replaceData = names, 
                                from = "index", 
                                to = "name") 
  # Ditto for the edges_df$from column  
  named_edges_df <- FindReplace(data = named_edges_df, 
                                Var = "from", 
                                replaceData = names, 
                                from = "index", 
                                to = "name") 
  # Retrun updated df
  named_edges_df
}

# Function to populate the infection status colum of the nodes df - one generation at a time 
# If using the animate package instead of gganimate - run this inside the animate for loop 
update_status_gen <- function(nodes_df) {
  # Check if the generations column is filled (no 0 values) 
  stopifnot(is.data.frame(nodes_df))
  if ( (min(nodes_df$generation) != 0) & (min(nodes_df$state) == 0) ) {
    # find the index for the first un-infected / in-active node 
    i_min <- match(0, nodes_df$state)
    # find the corresponding generation number for this node 
    current_gen <- nodes_df$generation[i_min]
    # get indicies for all nodes in that generation
    i_list <- which(nodes_df$generation %in% current_gen)
    # update all nodes in that generation to infected / active
    nodes_df$state[i_min:max(i_list)] <- 1
    # return the updated nodes_df 
    nodes_df
  } else {
    print("Generations column incomplete, or the status column complete - check nodes_df paramenter.") 
    # return same df supplied with no edits 
    nodes_df
  }
}

# Quick and dirty function to calculate total cases from grouped dataframes
total_cases <- function(tbl) {
  for(i in 1:nrow(tbl)) {
    v <- tbl$active_cases[1:i]
    tbl$total_cases[i] <- sum(v)
  }
  tbl
}