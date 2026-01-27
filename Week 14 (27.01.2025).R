
# Assignment Week 14

# 1. Load the trade data from the Dropbox folder (please also check the pdf 
# codebook for the names of the variables).
# 2. Draw a plot of the trade network across the main European countries (select
# the countries you prefer).
# 3. Make sure the size of the node represents the openness of the economy 
# (imports + exports) / GDP) while the thickness of the edge represents the size
# of the exchange.


setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/QMH data")
library(readxl)
trade_data <- read_excel("trade_data.xlsx")
GDP_data <- read_excel("TRADHIST_GDP_POP.xlsx")

library(ggplot2)
library(ggraph)

library(dplyr)

df <- trade_data %>%
  left_join(GDP_data, by = c("year" = "year", "iso_o" = "iso"))

df <- df %>% filter(year == 1913 | year == 1970) %>%
                     select(iso_o, iso_d, FLOW, GDP, year)

#Figuring out the names of the countries
unique(df$iso_o)

# Select main Western countries
main_countries <- c("FRA", "DEU", "GBR", "ITA", "ESP", 
                    "BEL", "NLD", "CHE", "AUT", "SWE",
                    "USA", "GBR", "USSR", "GBRIND",
                    "CAN", "CHN", "JPN")

df <- df %>% filter(iso_o %in% main_countries) %>%
             filter(iso_d %in% main_countries)

df_1913 <- df %>%
             filter(year == 1913)

#Define nodes
nodes <- df_1913 %>%
  select(name = iso_o, GDP) %>%
  distinct(name, .keep_all = TRUE)

trade_graph_1913 <- graph_from_data_frame(d = df_1913, vertices = nodes, directed = TRUE)

trade_graph_1970 <- graph_from_data_frame(d = df_1970, vertices = nodes, directed = TRUE)


# Plot the trade network
plot_1913 <- ggraph(trade_graph_1913, layout = "fr") +  # Fruchterman-Reingold layout
  geom_edge_link(aes(width = FLOW), alpha = 0.8, color = "gray") + # Set 
  # edge width based on total flow
  geom_node_point(aes(size = GDP), color = "blue") + # Node sizes based on 
  # openness
  geom_node_text(aes(label = name), repel = TRUE) + # Add country labels
  scale_edge_width(range = c(0.1, 2)) +  # Scale edge thickness for visibility
  theme_void() +  # Remove background and axes
  ggtitle("Trade Network 1913")

print(plot_1913)


#Compute main measures
glimpse(trade_graph_1913)

edge_density(trade_graph_1913)

transitivity(trade_graph_1913, type = "global")

# Calculate centrality
ev_cent <- eigen_centrality(trade_graph_1913, directed = TRUE, weights = E(trade_graph_1913)$FLOW)
ev_cent$vector



df_1970 <- df %>%
  filter(year == 1970)

#Define nodes
nodes <- df_1970 %>%
  select(name = iso_o, GDP) %>%
  distinct(name, .keep_all = TRUE)

trade_graph_1970 <- graph_from_data_frame(d = df_1970, vertices = nodes, directed = TRUE)

ev_cent <- eigen_centrality(trade_graph_1970, directed = TRUE, weights = E(trade_graph_1970)$FLOW)
ev_cent$vector







library(tidygraph)
library(ggraph)
library(dplyr)
library(igraph)

library(tidygraph)
library(ggraph)
library(dplyr)
library(igraph)

# --- 1. DATA PREPARATION ---
# Create a unique node list from all possible countries in the data
nodes <- df %>%
  select(name = iso_o, GDP) %>%
  distinct(name, .keep_all = TRUE)

# Build the base graph
trade_graph <- graph_from_data_frame(d = df, vertices = nodes, directed = TRUE) %>%
  as_tbl_graph()

# --- 2. PLOT 1913 ---
plot_1913 <- trade_graph %>%
  activate(edges) %>%
  filter(year == 1913) %>%
  activate(nodes) %>%
  # Calculate centrality specifically for this year's connectivity
  mutate(centrality = centrality_eigen(weights = E(.)$FLOW)) %>%
  # Remove countries that have no trade in this specific year (isolates)
  filter(!node_is_isolated()) %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = FLOW), alpha = 0.2, color = "gray70") +
  geom_node_point(aes(size = centrality), color = "darkred") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_size(range = c(2, 12)) +
  theme_void() +
  labs(title = "Global Trade Network: 1913", size = "Influence")

# --- 3. PLOT 1970 ---
plot_1970 <- trade_graph %>%
  # Step A: Filter the Edges (Time)
  activate(edges) %>%
  filter(year == 1970) %>%
  # Step B: Filter the Nodes (Specific Countries)
  activate(nodes) %>%
  filter(!name %in% c("DEU", "GBRIND")) %>%
  # Step C: Calculate metrics for the remaining network
  mutate(centrality = centrality_eigen(weights = E(.)$FLOW)) %>%
  filter(!node_is_isolated()) %>%
  # Step D: Visualise
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = FLOW), alpha = 0.2, color = "gray70") +
  geom_node_point(aes(size = centrality), color = "steelblue") + 
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_size(range = c(2, 12)) +
  theme_void() +
  labs(title = "Global Trade Network: 1970", size = "Influence")

# Print results
print(plot_1913)
print(plot_1970)
