import osmnx as ox
import geobr

# Initial setup
print(ox.__version__)
ox.config(use_cache=True)

# Download census tracts
tracts = geobr.read_census_tract(3143302, year=2010, simplified=False)

# Get only urban tracts
tracts.query('zone == "URBANO" & code_district == "314330205"', inplace=True)

# Dissolve boundaries to create the mask
mask = tracts.dissolve(by='code_muni')

# Download corresponding street network
graph_nx = ox.graph_from_polygon(mask.geometry.all())

# Save graph for backup
ox.save_graph_geopackage(graph_nx, filepath='./data/moc_2010_network.gpkg')
