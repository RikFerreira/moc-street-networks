library(tidyverse)
library(sf)

library(igraph)
library(tidygraph)
library(sfnetworks)
library(netrankr)

library(tmap)
library(tmaptools)

library(reticulate)
library(sisyphos)


resize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


# Execute python script
use_condaenv("ox")
source_python("./scripts/download-moc-network.py")

# Read gpkg
moc_nodes <- st_read("./data/moc_2010_network.gpkg", layer = "nodes") %>%
  select(osmid) %>%
  mutate(osmid = as.character(osmid))

moc_edges <- st_read("./data/moc_2010_network.gpkg", layer = "edges") %>%
  select(from, to, length) %>%
  mutate(
    from = as.character(from),
    to = as.character(to)
  )

moc_regplan <- st_read("/mnt/HDD/STORAGE/Bases vetoriais/REGIOES_DE_PLANEJAMENTO.gpkg")

# Create spatial graph
moc_graph <- sfnetwork(
  nodes = moc_nodes,
  edges = moc_edges,
  node_key = "osmid",
  directed = FALSE,
  edges_as_lines = TRUE,
  force = TRUE
)

# Calculate centralities
final_graph <- moc_graph %>%
  activate("edges") %>%
  mutate(
    edge_betweenness = centrality_edge_betweenness(weights = length, directed = FALSE)
  ) %>%
  activate("nodes") %>%
  mutate(
    node_closeness = centrality_closeness(),
  )

tm_shape(final_graph %>% activate("edges") %>% st_as_sf()) +
  tm_lines(
    lwd = 2,
    col = "edge_betweenness",
    palette = "Spectral",
    breaks = final_graph %>% activate("edges") %>% st_as_sf() %>% pull(edge_betweenness) %>% boxcut,
    legend.col.show = FALSE
  ) +
tm_shape(moc_regplan) +
  tm_borders(lwd = 4) +
  tmap_style("natural") +
  tmap_save("./plots/moc-betweenness.png")