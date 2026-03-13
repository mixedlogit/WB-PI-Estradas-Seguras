
library(tidyverse)
library(sf)

# Network -------------------------------------------------

road <- st_read("datasets/intermediate/SRE 2025/Estadual_PI.shp")
road.coincidente <- st_read(dsn = "datasets/intermediate/SRE 2025/Estadual_Coincidentes.shp")
road.eod <- st_read(dsn = "datasets/intermediate/SRE 2025/Estadual_EOP_EOD.shp")

str(road)
head(road)
glimpse(road)

unique(road$fonte)
unique(road$superficie)

mapview::mapview(road)+mapview::mapview(road.eod,color="red")+
  mapview::mapview(road.coincidente,color="green")

