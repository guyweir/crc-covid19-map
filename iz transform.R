suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))
library(spdplyr)
library(rmapshaper)

izraw <- st_read(dsn = "./SG_IntermediateZoneBdry_2011", layer = "SG_IntermediateZone_Bdry_2011")
izraw <- izraw %>% select(InterZone, Name)
izraw <- st_transform(izraw, crs = "+proj=longlat +datum=WGS84")

izjson <- geojson_json(izraw)
izjson <- ms_simplify(izjson)

geojson_write(izjson,file = "izboundaries_simplified.geojson")




