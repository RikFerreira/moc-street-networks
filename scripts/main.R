library(tidyverse)
library(sf)
library(geobr)

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

moc_weighting_area <- read_weighting_area(3143302, simplified = FALSE) %>%
  filter(code_weighting_area != 3143302005022)

osm_basemap <- read_osm(moc_regplan %>% st_bbox())

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

tm_shape(osm_basemap) + tm_rgb() +
tm_shape(final_graph %>% activate("edges") %>% st_as_sf() %>% arrange(edge_betweenness), bbox = moc_regplan %>% st_bbox) +
  tm_lines(
    lwd = 3,
    col = "edge_betweenness",
    palette = "cividis",
    breaks = final_graph %>% activate("edges") %>% st_as_sf() %>% pull(edge_betweenness) %>% boxcut,
    legend.col.show = FALSE
  ) +
tm_shape(moc_weighting_area) +
  tm_borders(
    lwd = 4,
    col = "black"
  )
  

tmap_save(filename = "./plots/moc-betweenness.png")
