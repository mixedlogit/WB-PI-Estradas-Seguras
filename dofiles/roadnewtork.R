
library(tidyverse)
library(sf)
library(mapview)

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

road <- road %>% 
  mutate(exte_dec = as.numeric(exte_dec))

road.coincidente <- road.coincidente %>% 
  mutate(exte_dec = as.numeric(exte_dec))

road.coincidente <- road.coincidente %>% 
  mutate(exte_dec = as.numeric(exte_dec))

sum(road$exte_dec[road$jurisdicao=="Estadual"])
sum(road.coincidente$exte_dec)

road %>% 
  group_by(jurisdicao,superficie) %>% 
  reframe(exte_dec = sum(exte_dec))

road.coincidente %>% 
  group_by(jurisdicao,superficie) %>% 
  reframe(exte_dec = sum(exte_dec))

road.pav <- rbind(road %>% 
                    filter(jurisdicao == "Estadual") %>% 
                    filter(superficie == "PAV" | superficie == "DUP"),
                  road.coincidente %>% filter(jurisdicao == "Estadual") %>% 
                    filter(superficie == "PAV" | superficie == "DUP"))

sum(road.pav$exte_dec)

mapview::mapview(road.pav)

# Estabelecimentos ----------------------------------------------
estab <- arrow::read_delim_arrow("datasets/intermediate/Anexo – LatLong Estabelecimentos.csv",delim = ";")

#arrow::write_parquet(x = estab,sink = "datasets/estab_piaui.parquet")

schools <- estab %>% 
  filter(COD_ESPECIE==4) %>% 
  st_as_sf(coords = c("LONGITUDE","LATITUDE"),crs=4674)

health <- estab %>% 
  filter(COD_ESPECIE==5) %>% 
  st_as_sf(coords = c("LONGITUDE","LATITUDE"),crs=4674)

st_write(obj = schools,dsn = "datasets/final/school_piaui.gpkg")
st_write(obj = health,dsn = "datasets/final/health_piaui.gpkg")

# Intersection ----------------------------------------------
install.packages("duckspatial")
library(duckspatial)

schools <- st_transform(schools,31983)
health <- st_transform(health,31983)
road.pav <- st_transform(road.pav,31983)

school.buff <- ddbs_buffer(x = schools,distance = 50)
health.buff <- st_buffer(x = health,dist = 50)

ggplot()+geom_sf(mapping = aes(),data = schools)
mapview(school.buff)


int.school <- ddbs_filter(
  x = school.buff,
  y = road.pav,
  predicate = "intersects"
)

int.health <- ddbs_filter(
  x = health.buff,
  y = road.pav,
  predicate = "intersects"
)

mapview(int.school)+mapview(road.pav)
